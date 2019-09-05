(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (let ([format (string-append "\t" (car args))]
        [rest (cdr args)])
    (apply fprintf (compile-port) format rest)
    (newline (compile-port))))

(define (emit-label label)
  (apply fprintf (compile-port) (list "~a:" label))
  (newline (compile-port)))

(define fixnum-tag 0)
(define fixnum-shift 2)
(define fixnum-mask 3)

(define char-tag 15)
(define char-shift 8)
(define char-mask 255)

(define bool-tag 31)
(define bool-shift 8)
(define bool-mask 255)

(define empty-list 47) ;; 00101111 - 0x2f

(define object-mask 7)
(define object-tag-pair 1)
(define object-tag-vector 2)
(define object-tag-string 3)

(define wordsize 4)

(define (immediate-rep expr)
  (cond [(integer? expr) (ash expr fixnum-shift)]
        [(null? expr) empty-list]
        [(boolean? expr) (let ([val (if expr 1 0)])
                           (logior (ash val bool-shift) bool-tag))]
        [(char? expr) (let ([val (char->integer expr)])
                           (logior (ash val char-shift) char-tag))]
        [else 0]))

(define (immediate? expr)
  (or (integer? expr) (null? expr) (char? expr) (boolean? expr)))

(define (lambda? x) (eq? (car x) 'lambda))

(define (remove-duplicates xs)
  (if (null? xs)
      xs
      (cons (car xs)
            (remove-duplicates (filter (lambda (el) (not (equal? (car xs) el))) xs)))))

(define (precompile-annotate-free-vars e env)
  (cond
    ((null? e) (list e '()))

    ; if the var is bound, then don't add it to the free list
    ((variable? e) (list e (if (member e env) '() (list e))))

    ; catch-all for immediates
    ((not (list? e)) (list e '()))

    ; handle if form so we don't end up with 'if' as a free var
    ((if? e)
     (let* ((parts (map (lambda (e) (precompile-annotate-free-vars e env)) (cdr e)))
            (annotated (map car parts))
            (free-vars (map cadr parts)))
       (list `(if ,@annotated)
             (remove-duplicates (apply append free-vars)))))

    ; deal with lambdas
    ((lambda? e)
     (let* ((args (cadr e))
            (body-form (caddr e))

            ; treat the outer environment as unbound 
            (annotated-body (precompile-annotate-free-vars body-form args))

            ; free variables list shouldn't include args, even though they'll be
            ; output as free when analyzing the body
            (inner-free (filter (lambda (v) (not (memq v args)))
                                (cadr annotated-body))))
       (list `(lambda ,args ,inner-free ,(car annotated-body))
             inner-free)))
  ; primitive call - ignore the function, map over args
    ((prim-apply? e)
     (let* ((results (map (lambda (p) (precompile-annotate-free-vars p env))
                          (cddr e)))
            (annotated (map car results))
            (free (apply append (map cadr results))))
       (list `(prim-apply ,(cadr e) ,@annotated)
             (remove-duplicates free))))

    ; check for let form and apply bindings
    ((let? e)
     (let* ((bindings (cadr e))
            (body-forms (cddr e))
            (bind-names (map car bindings))
            (bind-bodies (map cadr bindings))

            ; analyze all binding bodies
            (body-free (map (lambda (e) (precompile-annotate-free-vars e env))
                            bind-bodies))
            (new-bodies (map car body-free))
            (body-free (apply append (map cadr body-free)))

            (inner-env (append bind-names env))
            (inner-annotated
              (map (lambda (b) (precompile-annotate-free-vars b inner-env))
                   body-forms)))
       (list `(let ,(map list bind-names new-bodies) ,@(map car inner-annotated))
             (remove-duplicates (append (apply append (map cadr inner-annotated))
                                        body-free)))))

    (else (let* ((results (map (lambda (p) (precompile-annotate-free-vars p env)) e))
                 (annotated (map car results))
                 (free (apply append (map cadr results))))
            (list annotated (remove-duplicates free))))))

(define (precompile-add-labels pgm)
  (let ()
    (define next-label-tag 0)
    (define label-forms '())

    ; add a label with the given contents, returning its name as a symbol
    (define (make-label form)
      (let* ((new-tag next-label-tag)
             (tag-name (string->symbol (format #f "lbl~a" new-tag)))
             (new-label-forms (cons (list tag-name form) label-forms)))
        (begin (set! next-label-tag (+ 1 new-tag))
               (set! label-forms new-label-forms)
               tag-name)))

    (define (xform pgm)
      (cond
        ((not (list? pgm)) pgm) ; non-list stuff is unchanged
        ((null? pgm) pgm)

        ; transform lambdas into labels
        ((lambda? pgm)
         (let* ((arguments (cadr pgm))
                (free-vars (caddr pgm))
                (body (xform (cadddr pgm)))
                (name (make-label `(code ,arguments ,free-vars ,body))))
           `(closure ,name ,@free-vars)))

        ; recursively transform let expressions
        ((let? pgm)
         (let* ((bindings (cadr pgm))
                (body (cddr pgm))
                (binding-names (map car bindings))
                (binding-values (map cadr bindings)))
           `(let ,(map list binding-names (map xform binding-values))
                 ,@(map xform body))))

        ; handle function calls and if exprs
        ((if? pgm) `(if ,@(map xform (cdr pgm))))
        ((prim-apply? pgm)
         `(prim-apply ,(prim-apply-fn pgm)
                    ,@(map xform (prim-apply-args pgm))))

        ; turn normal function calls into funcall forms
        (else `(funcall ,(xform (car pgm))
                        ,@(map xform (cdr pgm))))))

    (let ((new-pgm (xform pgm)))
      `(labels ,label-forms
               ,new-pgm))))

(define (prim-apply? expr) (eq? (car expr) 'prim-apply))
(define (prim-apply-fn expr) (cadr expr))
(define (prim-apply-args expr) (cddr expr))
(define (prim-apply-arg-1 expr) (caddr expr))
(define (prim-apply-arg-2 expr) (cadddr expr))
(define (prim-apply-arg-3 expr) (cadddr (cdr expr)))

(define (emit-prim-apply-args expr stack-index env)
  (for-each
    (lambda (e) (emit-expr e stack-index env))
    (reverse (prim-apply-args expr))))

(define (emit-prim-apply expr stack-index env)
  (case (prim-apply-fn expr)
    [(add1)
     (emit-prim-apply-args expr stack-index env)
     (emit "addl $~a, %eax" (immediate-rep 1))]
    [(sub1)
     (emit-prim-apply-args expr stack-index env)
     (emit "subl $~a, %eax" (immediate-rep 1))]
    [(fixnum->char)
     (emit-prim-apply-args expr stack-index env)
     (emit "shl $~s, %eax" (- char-shift fixnum-shift))
     (emit "or $~s, %eax" char-tag)]
    [(char->fixnum)
     (emit-prim-apply-args expr stack-index env)
     (emit "shr $~s, %eax" (- char-shift fixnum-shift))]
    [(zero?)
     (emit-prim-apply-args expr stack-index env)
     (emit-eax-eq? 0)]
    [(null?)
     (emit-prim-apply-args expr stack-index env)
     (emit-eax-eq? empty-list)]
    [(fixnum?)
     (emit-prim-apply-args expr stack-index env)
     (emit "andl $~a, %eax" fixnum-mask)
     (emit-eax-eq? fixnum-tag)]
    [(boolean?)
     (emit-prim-apply-args expr stack-index env)
     (emit "andl $~a, %eax" bool-mask)
     (emit-eax-eq? bool-tag)]
    [(char?)
     (emit-prim-apply-args expr stack-index env)
     (emit "andl $~a, %eax" char-mask)
     (emit-eax-eq? char-tag)]
    [(+)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "addl ~a(%esp), %eax" stack-index)]
    [(cons)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     ;; store cdr
     (emit "movl %eax, ~a(%esi)" wordsize)
     ;; store car
     (emit "movl ~a(%esp), %eax" stack-index)
     (emit "movl %eax, 0(%esi)")
     ;; save pointer and tag it, then increment heap ptr
     (emit "movl %esi, %eax")
     (emit "orl $~a, %eax" object-tag-pair)
     (emit "addl $~a, %esi" (* 2 wordsize))]
    [(car)
     (emit-prim-apply-args expr stack-index env)
     (emit "movl -1(%eax), %eax")]
    [(cdr)
     (emit-prim-apply-args expr stack-index env)
     (emit "movl 3(%eax), %eax")]
    [(pair?) (emit-object-tag-eq? expr stack-index env object-tag-pair)]
    [(set-car!)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "movl ~a(%esp), %edx" stack-index)
     (emit "movl %eax, ~a(%edx)" (- object-tag-pair))]
    [(set-cdr!)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "movl ~a(%esp), %edx" stack-index)
     (emit "movl %eax, ~a(%edx)" (- wordsize object-tag-pair))]
    [(string?) (emit-object-tag-eq? expr stack-index env object-tag-string)]
    [(make-string)
     (emit-fixnum-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, 0(%esi)") ;; Store length at beginning of next memory slot
     (emit "movl %eax, %ecx")
     (emit "movl %esi, %eax")
     (emit "orl $~a, %eax" object-tag-string)
     ;; Add 7+4 to length in %ecx
     ;; * 7 to align to multiple of 8
     ;; * 4 because that's the "length" stored at beginning of memory
     ;; then bitwise-AND it with -8
     (emit "addl $11, %ecx")
     (emit "andl $-8, %ecx")
     (emit "addl %ecx, %esi")]
    [(string-set!)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "subl $~a, %eax" object-tag-string)
     (emit "addl $~a, %eax" wordsize) ;; skip `length`
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-fixnum-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "addl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-3 expr) (- stack-index wordsize) env)
     (emit "shr $~a, %eax" char-shift)
     (emit "movl ~a(%esp), %ecx" stack-index) ;; Move pointer from ~a(%esp) to %ecx
     (emit "movb %al, (%ecx)") ;; Move byte from last byte of %eax to location pointed to by %ecx
     (emit "mov $0, %eax")]
    [(string-ref)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "subl $~a, %eax" object-tag-string)
     (emit "addl $~a, %eax" wordsize) ;; skip `length`
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-fixnum-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "addl ~a(%esp), %eax" stack-index)
     (emit "movzb (%eax), %eax")
     (emit "shl $~s, %eax" char-shift)
     (emit "or $~s, %eax" char-tag)]
    [(make-vector)
     (emit-fixnum-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, 0(%esi)") ;; Store length at beginning of next memory slot
     (emit "movl %eax, %ecx")
     (emit "sall $2, %ecx")
     (emit "movl %esi, %eax")
     (emit "orl $~a, %eax" object-tag-vector)
     (emit "addl $11, %ecx")
     (emit "andl $-8, %ecx")
     (emit "addl %ecx, %esi")]
    [(vector?) (emit-object-tag-eq? expr stack-index env object-tag-vector)]
    [(vector-set!)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "subl $~a, %eax" object-tag-vector) ;; Pointer to vector is in %eax, untag it
     (emit "addl $~a, %eax" wordsize)          ;; Skip `length` by increasing pointer
     (emit "movl %eax, ~a(%esp)" stack-index)  ;; Save pointer on stack
     (emit-fixnum-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "sall $2, %eax")                    ;; Multiply `length` by four
     (emit "addl %eax, ~a(%esp)" stack-index)  ;; Add it to the pointer saved at ~a(%esp)
     (emit "movl ~a(%esp), %ecx" stack-index)  ;; Move pointer from ~a(%esp) to %ecx
     (emit-expr (prim-apply-arg-3 expr) (- stack-index wordsize) env)
     (emit "movl %eax, (%ecx)")                ;; Move pointer in %eax to location pointed to by %ecx
     (emit "mov $0, %eax")]
    [(vector-ref)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "subl $~a, %eax" object-tag-vector)
     (emit "addl $~a, %eax" wordsize)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-fixnum-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "sall $2, %eax")                    ;; Multiply `length` by four
     (emit "addl ~a(%esp), %eax" stack-index)
     (emit "movl (%eax), %eax")]))

(define (emit-object-tag-eq? expr stack-index env tag)
  (emit-prim-apply-args expr stack-index env)
  (emit "andl $~a, %eax" object-mask)
  (emit-eax-eq? tag))

(define (emit-fixnum-expr expr stack-index env)
  (emit-expr expr stack-index env)
  (emit "shr $~a, %eax" fixnum-shift))

(define (emit-eax-eq? val)
  (emit "cmpl $~a, %eax" val)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" bool-shift)
  (emit "orl $~a, %eax" bool-tag))

(define (variable? expr) (symbol? expr))
(define (let? expr) (eq? 'let (car expr)))
(define (let-bindings expr) (cadr expr))
(define (let-body expr) (cddr expr))
(define (binding-ident b) (car b))
(define (binding-value b) (cadr b))

(define (new-env) '())
(define (lookup ident env) (assoc ident env))
(define (extend-env ident stack-index env) (cons (cons ident stack-index) env))

(define (emit-let bindings body stack-index env)
  ; (display (format "\n(emit-let bindings=~a body=~a stack-index=~a env=~a)\n" bindings body stack-index env))
  (let loop ([b* bindings]
             [e env]
             [stack-index stack-index])
    (if (null? b*)
        (for-each (lambda (body-expr) (emit-expr body-expr stack-index e)) body)
        (let ((b (car b*)))
          (emit-expr (binding-value b) stack-index env)
          (emit "movl %eax, ~a(%esp)" stack-index)
          (loop (cdr b*)
             (extend-env (binding-ident b) stack-index e)
             (- stack-index wordsize))))))

(define (if? expr) (eq? 'if (car expr)))
(define (if-condition expr) (cadr expr))
(define (if-consequence expr) (caddr expr))
(define (if-alternative expr) (cadddr expr))

(define label-count 0)

(define (unique-label)
  (let ([l (format "label_~a" label-count)])
    (set! label-count (+ label-count 1))
    l))

(define (emit-if expr stack-index env)
  (let ([alternative-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr (if-condition expr) stack-index env)
    (emit "cmpl $~s, %eax" (immediate-rep #f))
    (emit "je ~a" alternative-label)
    (emit-expr (if-consequence expr) stack-index env )
    (emit "jmp ~a" end-label)
    (emit-label alternative-label)
    (emit-expr (if-alternative expr) stack-index env)
    (emit-label end-label)))

(define (emit-expr expr stack-index env)
  ; (display (format "\n(emit-expr expr=~a stack-index=~a env=~a)\n" expr stack-index env))
  (cond [(immediate? expr) (emit "movl $~a, %eax" (immediate-rep expr))]
        [(variable? expr)
         (let ([p (lookup expr env)])
           (if (pair? p)
               (emit "movl ~a(%esp), %eax" (cdr p))
               (error 'lookup (format "not found in env: ~a" expr))))]
        [(let? expr)
         (emit-let (let-bindings expr) (let-body expr) stack-index env)]
        [(if? expr) (emit-if expr stack-index env)]
        [(prim-apply? expr) (emit-prim-apply expr stack-index env)]
        [else (begin
                (display (format "unrecognized form: ~a\n" expr))
                (emit "movl $99, %eax"))]))

(define (emit-program expr env)
  (emit ".text")
  (emit ".p2align 4,,15")
  (emit ".globl scheme_entry")
  (emit ".type scheme_entry, @function")
  (emit-label "scheme_entry")

  ; Save registers
  (emit "push %esi")
  (emit "push %edi")
  (emit "push %edx")

  ; Save heap pointer
  (emit "movl 16(%esp), %esi")

  ; Compile!
  (emit-expr expr (- wordsize) env)

  ; Restore registers
  (emit "pop %edx")
  (emit "pop %edi")
  (emit "pop %esi")

  (emit "ret"))

(define (precompile program)
  (precompile-add-labels (car (precompile-annotate-free-vars program '()))))
