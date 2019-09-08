(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (let ([line-format (string-append "\t" (car args))]
        [rest (cdr args)])
    (apply fprintf (compile-port) line-format rest)
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
(define object-tag-closure 6)

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
     (emit "movl ~a(%esp), %edi" stack-index)
     (emit "movl %eax, ~a(%edi)" (- object-tag-pair))]
    [(set-cdr!)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "movl ~a(%esp), %edi" stack-index)
     (emit "movl %eax, ~a(%edi)" (- wordsize object-tag-pair))]
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
     (emit "movl (%eax), %eax")]
    [(closure?) (emit-object-tag-eq? expr stack-index env object-tag-closure)]))

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

(define (extend-env-var ident stack-index env)
  (cons (cons ident (list 'var stack-index)) env))

(define (extend-env-vars idents stack-indexes env)
  (fold-left (lambda (e arg index) (extend-env-var arg index e))
             env
             idents
             stack-indexes))

(define (extend-env-label label env)
  (cons (cons label (list 'label label)) env))

(define (extend-env-labels labels env)
  (fold-left (lambda (e label) (extend-env-label label e))
             env
             labels))

(define (extend-env-free free-var closure-offset env)
  (cons (cons free-var (list 'free-var closure-offset)) env))

(define (extend-env-frees free-vars closure-offsets env)
  (fold-left (lambda (e free-var offset) (extend-env-free free-var offset e))
             env
             free-vars
             closure-offsets))

(define (emit-let bindings body stack-index env)
  (let loop ([b* bindings]
             [e env]
             [stack-index stack-index])
    (if (null? b*)
        (for-each (lambda (body-expr) (emit-expr body-expr stack-index e)) body)
        (let ((b (car b*)))
          (emit-expr (binding-value b) stack-index env)
          (emit "movl %eax, ~a(%esp)" stack-index)
          (loop (cdr b*)
             (extend-env-var (binding-ident b) stack-index e)
             (- stack-index wordsize))))))

(define (if? expr) (eq? 'if (car expr)))
(define (if-condition expr) (cadr expr))
(define (if-consequence expr) (caddr expr))
(define (if-alternative expr) (cadddr expr))

(define label-count 0)
(define (unique-label)
  (let ([l (string->symbol (format "label_~a" label-count))])
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

(define (funcall? expr) (eq? (car expr) 'funcall))

(define (emit-funcall expr stack-index env)
  (let* ([call-target (cadr expr)]

         ; stack offset that args start at
         ; the (* 2 wordsize) is space for return addr and closure
         [args-start (- stack-index (* 2 wordsize))]

         [args (cddr expr)]
         ; build stack offsets for each argument
         ; results in a list of (arg-body stack-offset) pairs
         [args (map list args
                    (map (lambda (i) (- args-start (* wordsize i)))
                         (iota (length args))))]

         [eval-stack-index (- args-start (* wordsize (length args)))])

    ; evaluate arguments from left to right, storing into arg cells
    (for-each
      (lambda (arg) (begin (emit-expr (car arg) eval-stack-index env)
                           (emit "movl %eax, ~a(%esp)" (cadr arg))))
      args)

    ; evaluate closure we want to call
    (emit-expr call-target eval-stack-index env)

    ; store current closure pointer and switch to the new closure
    (emit "movl %edx, ~a(%esp)" stack-index)
    (emit "movl %eax, %edx")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; TODO: TAGGING IS BROKEN
    (emit "subl $~a, %edx" object-tag-closure)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; advance %esp and call the function
    (emit "subl $~a, %esp" (- stack-index))
    (emit "call *(%edx)")

    ; restore the stack pointer afterwards and reload our current closure
    (emit "addl $~a, %esp" (- stack-index))
    (emit "movl ~a(%esp), %edx" stack-index)))

(define (closure? expr) (eq? (car expr) 'closure))

(define (emit-closure expr stack-index env)
  (let ([label (cadr expr)]
        [free-vars (cddr expr)])
    ;; Store label in location pointed to by %esi
    (emit-variable label stack-index env)
    (emit "movl %eax, 0(%esi)")

    (for-each
      (lambda (free offset)
        (emit-variable free stack-index env)
        (emit "movl %eax, ~a(%esi)" offset))
      free-vars
      (free-vars-to-closure-offsets free-vars))

    ;; Move pointer to %eax and tag it
    (emit "movl %esi, %eax")
    (emit "orl $~a, %eax" object-tag-closure)

    ;; Increase allocation pointer in %esi
    ;; Add 7+((length-free-vars + 1)*wordsize) to pointer in %esi because
    ;; * 7 to align to multiple of 8
    ;; * `+ 1` because that's the size of the label we stored and want to skip.
    ;; * `+ length-free-vars` because that's the free vars stored after label we want to skip
    ;; then bitwise-AND it with -8

    (emit "addl $~a, %esi" (+ 7 (* wordsize (+ 1 (length free-vars)))))
    (emit "andl $-8, %esi")))

(define (emit-variable expr stack-index env)
  (let ([p (lookup expr env)])
    (if (not (pair? p))
        (error 'lookup (format "not found in env: ~a" expr))
        (case (cadr p)
          [var (emit "movl ~a(%esp), %eax" (caddr p))]
          [label (emit "movl $~a, %eax" (caddr p))]
          [free-var (emit "movl ~a(%edx), %eax" (caddr p))]))))

(define (emit-expr expr stack-index env)
  ; (display (format "\n(emit-expr expr=~a stack-index=~a env=~a)\n" expr stack-index env))
  (cond [(immediate? expr) (emit "movl $~a, %eax" (immediate-rep expr))]
        [(variable? expr)
         (emit-variable expr stack-index env)]
        [(let? expr)
         (emit-let (let-bindings expr) (let-body expr) stack-index env)]
        [(if? expr) (emit-if expr stack-index env)]
        [(prim-apply? expr) (emit-prim-apply expr stack-index env)]
        [(funcall? expr) (emit-funcall expr stack-index env)]
        [(closure? expr) (emit-closure expr stack-index env)]
        [else (begin
                (display (format "unrecognized form: ~a\n" expr))
                (emit "movl $99, %eax"))]))

(define (args-to-stack-offsets args)
  (map (lambda (i) (* (- wordsize) (+ 1 i))) (iota (length args))))

(define (free-vars-to-closure-offsets free-vars)
  (map (lambda (i) (* wordsize (+ 1 i))) (iota (length free-vars))))

(define (emit-label-code label env)
  (case (caadr label)
    ((code)
     (let*
       ([code-form (cadr label)]
        [name (car label)]
        [args (cadr code-form) ]
        [free-vars (caddr code-form) ]
        [body (cadddr code-form) ]
        [inner-env
          (extend-env-frees free-vars
                            (free-vars-to-closure-offsets free-vars)
                            (extend-env-vars args
                                             (args-to-stack-offsets args)
                                             env))]
        [locals-start (- (* wordsize (+ 1 (length args))))])

       (emit-label name)
       (emit-expr body locals-start inner-env)
       (emit "ret")))))

(define (emit-program labels body env)
  (let* ([env-with-labels (extend-env-labels (map car labels) env)])
    (emit ".text")
    (emit ".p2align 4,,15")
    (emit ".globl scheme_entry")
    (emit ".type scheme_entry, @function")

    (for-each (lambda (l) (emit-label-code l env-with-labels)) labels)

    (emit-label "scheme_entry")
    ; Save registers
    (emit "push %esi")
    (emit "push %edi")
    (emit "push %edx")

    ; Save heap pointer
    (emit "movl 16(%esp), %esi")

    ; Compile!
    (emit-expr body (- wordsize) env-with-labels)

    ; Restore registers
    (emit "pop %edx")
    (emit "pop %edi")
    (emit "pop %esi")

    (emit "ret")))

(define (precompile-annotate-free-vars expr free-vars)
  (cond
    ((null? expr) (list expr '()))

    ; if the var is bound, then don't add it to the free list
    ((variable? expr)
       (list expr (if (member expr free-vars) '() (list expr))))

    ; catch-all for immediates
    ((not (list? expr)) (list expr '()))

    ; handle if form so we don't end up with 'if' as a free var
    ((if? expr)
     (let* ((parts (map (lambda (expr) (precompile-annotate-free-vars expr free-vars)) (cdr expr)))
            (annotated (map car parts))
            (free-vars (map cadr parts)))
       (list `(if ,@annotated)
             (remove-duplicates (apply append free-vars)))))

    ; deal with lambdas
    ((lambda? expr)
     (let* ((args (cadr expr))
            (body-form (caddr expr))

            ; treat the outer environment as unbound 
            (annotated-body (precompile-annotate-free-vars body-form args))

            ; free variables list shouldn't include args, even though they'll be
            ; output as free when analyzing the body
            (inner-free (filter (lambda (v) (not (memq v args)))
                                (cadr annotated-body))))
       (list `(lambda ,args ,inner-free ,(car annotated-body))
             inner-free)))
  ; primitive call - ignore the function, map over args
    ((prim-apply? expr)
     (let* ((results (map (lambda (p) (precompile-annotate-free-vars p free-vars))
                          (cddr expr)))
            (annotated (map car results))
            (free (apply append (map cadr results))))
       (list `(prim-apply ,(cadr expr) ,@annotated)
             (remove-duplicates free))))

    ; check for let form and apply bindings
    ((let? expr)
     (let* ((bindings (cadr expr))
            (body-forms (cddr expr))
            (bind-names (map car bindings))
            (bind-bodies (map cadr bindings))

            ; analyze all binding bodies
            (body-free (map (lambda (expr) (precompile-annotate-free-vars expr free-vars)) bind-bodies))
            (new-bodies (map car body-free))
            (body-free (apply append (map cadr body-free)))

            (inner-free-vars (append bind-names free-vars))
            (inner-annotated (map (lambda (b) (precompile-annotate-free-vars b inner-free-vars)) body-forms)))

       (list `(let ,(map list bind-names new-bodies) ,@(map car inner-annotated))
             (remove-duplicates (append (apply append (map cadr inner-annotated)) body-free)))))

    (else (let* ((results (map (lambda (p) (precompile-annotate-free-vars p free-vars)) expr))
                 (annotated (map car results))
                 (free (apply append (map cadr results))))
            (list annotated (remove-duplicates free))))))

(define (precompile-add-labels expr)
  (define label-forms '())

  (define (transform expr)
    (cond
      ((not (list? expr)) expr)
      ((null? expr) expr)

      ((lambda? expr)
       (let* ((arguments (cadr expr))
              (free-vars (caddr expr))
              (body (transform (cadddr expr)))
              (label (unique-label))
              (code-expr `(code ,arguments ,free-vars ,body)))
         (begin
           (set! label-forms (cons (list label code-expr) label-forms))
           `(closure ,label ,@free-vars))))

      ((let? expr)
       (let* ((bindings (cadr expr))
              (body (cddr expr))
              (binding-names (map car bindings))
              (binding-values (map cadr bindings)))
         `(let ,(map list binding-names (map transform binding-values))
            ,@(map transform body))))

      ((if? expr) `(if ,@(map transform (cdr expr))))
      ((prim-apply? expr)
       `(prim-apply ,(prim-apply-fn expr)
                    ,@(map transform (prim-apply-args expr))))

      (else `(funcall ,(transform (car expr))
                      ,@(map transform (cdr expr))))))

  (let ((transformed-expr (transform expr)))
    `(labels ,label-forms
             ,transformed-expr)))

(define (precompile expr)
  (set! label-count 0)
  (precompile-add-labels (car (precompile-annotate-free-vars expr '()))))

(define (compile-program expr)
  (let ([precompiled (precompile expr)])
    (emit-program (cadr precompiled) (caddr precompiled) (new-env))))
