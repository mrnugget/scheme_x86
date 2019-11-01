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

(define (emit-local name)
  (format (compile-port) ".local ~a\n" name)
  (format (compile-port) ".comm ~a,4,4\n" name))

(define (emit-global name)
  (format (compile-port) ".globl ~a\n" name)
  (format (compile-port) ".comm ~a,4,4\n" name))

(define (emit-function-header name)
    (format (compile-port) ".text\n")
    (format (compile-port) ".globl ~a\n" name)
    (format (compile-port) ".type ~a, @function\n" name)
    (emit-label name))

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
(define object-tag-symbol 5)

(define wordsize 4)

(define (immediate-rep expr)
  (cond [(integer? expr) (ash expr fixnum-shift)]
        [(null? expr) empty-list]
        [(boolean? expr) (let ([val (if expr 1 0)])
                           (logior (ash val bool-shift) bool-tag))]
        [(char? expr) (let ([val (char->integer expr)])
                           (logior (ash val char-shift) char-tag))]
        [else 0]))

(define-syntax define-list-expr-check
  (syntax-rules ()
    [(_ check-name expr-s)
     (define (check-name expr)
       (and (not (null? expr)) (list? expr) (eq? (car expr) expr-s)))]))

(define-list-expr-check funcall? 'funcall)
(define-list-expr-check tailcall? 'tailcall)
(define-list-expr-check constant-ref? 'constant-ref)
(define-list-expr-check constant-init? 'constant-init)
(define-list-expr-check primitive-ref? 'primitive-ref)
(define-list-expr-check closure? 'closure)
(define-list-expr-check lambda? 'lambda)
(define-list-expr-check set? 'set!)
(define-list-expr-check prim-apply? 'prim-apply)
(define-list-expr-check let? 'let)
(define-list-expr-check let*? 'let*)
(define-list-expr-check letrec? 'letrec)
(define-list-expr-check quote? 'quote)
(define-list-expr-check if? 'if)
(define-list-expr-check and? 'and)
(define-list-expr-check foreign-call? 'foreign-call)
(define-list-expr-check prim-apply? 'prim-apply)

(define builtin-forms '(if let lambda closure set! quote))
(define builtin-primitives
  '(add1 sub1 fixnum->char char->fixnum zero? null?
         not fixnum? boolean? char? + eq? char=? cons
         car cdr pair? set-car! set-cdr! symbol?
         make-symbol symbol->string string? make-string
         string-set! string-ref string-length make-vector
         vector? vector-set! vector-ref closure?))

(define (immediate? expr)
  (or (integer? expr) (null? expr) (char? expr) (boolean? expr)))

(define (primitive-name? expr)
  (and (symbol? expr) (assq expr primitives)))

(define (lambda-body x) (cddr x))
(define (lambda-vars expr) (cadr expr))
(define (flatten-lambda-vars vars)
  (cond
    [(list? vars) vars]
    [(pair? vars) (cons (car vars) (flatten-lambda-vars (cdr vars)))]
    [else (list vars)]))
(define (lambda-vars-flattened expr)
  (flatten-lambda-vars (cadr expr)))
(define (lambda-single-vararg? args) (symbol? args))
(define (lambda-varargs? args) (not (list? args)))
(define (lambda-vararg-name args)
  (cond [(symbol? args) args]
        [(not (pair? (cdr args))) (cdr args)]
        [else (lambda-vararg-name (cdr args))]))

(define (identifier? expr) (symbol? expr))
(define (let-bindings expr) (cadr expr))
(define (let-body expr) (cddr expr))
(define (binding-ident b) (car b))
(define (binding-value b) (cadr b))

(define (set-variable x) (cadr x))
(define (set-value x) (caddr x))

(define (remove-duplicates xs)
  (if (null? xs)
      xs
      (cons (car xs)
            (remove-duplicates (filter (lambda (el) (not (equal? (car xs) el))) xs)))))

(define (prim-apply-fn expr) (cadr expr))
(define (prim-apply-args expr) (cddr expr))
(define (prim-apply-arg-1 expr) (caddr expr))
(define (prim-apply-arg-2 expr) (cadddr expr))
(define (prim-apply-arg-3 expr) (cadddr (cdr expr)))

(define (if-condition expr) (cadr expr))
(define (if-consequence expr) (caddr expr))
(define (if-alternative? expr) (not (null? (cdddr expr))))
(define (if-alternative expr) (cadddr expr))


(define (emit-prim-apply-args expr stack-index env)
  (for-each
    (lambda (e) (emit-expr e stack-index env))
    (reverse (prim-apply-args expr))))

(define (emit-ensure-eax-is tag err-proc stack-index env)
  (let ([skip-label (unique-label)])
    (emit "movl %eax, %edi")
    (emit "andl $~a, %edi" object-mask)
    (emit "cmpl $~a, %edi" tag)
    (emit "je ~a" skip-label)
    (emit-expr `(funcall (primitive-ref ,err-proc)) stack-index env)
    (emit-label skip-label)))

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
    [(not)
     (emit-prim-apply-args expr stack-index env)
     (emit "cmpl $~s, %eax" (immediate-rep #f))
     (emit-eax-to-bool)]
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
    [(eq? char=?)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-2 expr) (- stack-index wordsize) env)
     (emit "cmpl ~a(%esp), %eax" stack-index)
     (emit-eax-to-bool)]
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
     (emit-ensure-eax-is object-tag-pair 'error-no-pair stack-index env)
     (emit "movl -1(%eax), %eax")]
    [(cdr)
     (emit-prim-apply-args expr stack-index env)
     (emit-ensure-eax-is object-tag-pair 'error-no-pair stack-index env)
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
    [(symbol?) (emit-object-tag-eq? expr stack-index env object-tag-symbol)]
    [(make-symbol)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "subl $~a, %eax" object-tag-string)
     (emit "orl $~a, %eax" object-tag-symbol)]
    [(symbol->string)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "subl $~a, %eax" object-tag-symbol)
     (emit "orl $~a, %eax" object-tag-string)]
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
    [(string-length)
     (emit-expr (prim-apply-arg-1 expr) stack-index env)
     (emit "subl $~a, %eax" object-tag-string)
     (emit "mov (%eax), %eax")
     (emit "shl $~a, %eax" fixnum-shift)]
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
  (emit-eax-to-bool))

(define (emit-eax-to-bool)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" bool-shift)
  (emit "orl $~a, %eax" bool-tag))

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
    (if (if-alternative? expr)
        (emit-expr (if-alternative expr) stack-index env))
    (emit-label end-label)))

(define (emit-funcall expr stack-index env tailcall)
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

    ;; evaluate closure we want to call
    (emit-expr call-target eval-stack-index env)

    (unless (and (list? call-target) (equal? (cadr call-target) 'error-apply))
      (let ([label (unique-label)])
        ;; Save call target in tmp location %edi
        (emit "mov %eax, %edi")
        (emit "andl $~a, %eax" object-mask)
        (emit "cmpl $~a, %eax" object-tag-closure)
        (emit "je ~a" label)
        (emit-expr `(funcall (primitive-ref error-apply)) stack-index env)
        (emit-label label)
        ;; Restore original call target from tmp location
        (emit "mov %edi, %eax")))

    (emit "movl %edx, ~a(%esp)" stack-index)
    (emit "movl %eax, %edx")
    (emit "subl $~a, %edx" object-tag-closure)

    (if tailcall
        (begin
          (for-each
            (lambda (arg stack-offset)
              (begin (emit "movl ~a(%esp), %eax" (cadr arg))
                     (emit "movl %eax, ~a(%esp)" (- (- wordsize)
                                                    (* wordsize stack-offset)))))
            args
            (iota (length args)))
          ;; save number of args to %eax
          (emit "movl $~a, %eax" (length args))
          (emit "jmp *(%edx)"))

        (begin
          ; advance %esp and call the function
          (emit "subl $~a, %esp" (- stack-index))
          ;; save number of args to %eax
          (emit "mov $~a, %eax" (length args))
          (emit "call *(%edx)")

          ; restore the stack pointer afterwards and reload our current closure
          (emit "addl $~a, %esp" (- stack-index))
          (emit "movl ~a(%esp), %edx" stack-index)))))

(define (emit-closure expr stack-index env)
  (let ([label (cadr expr)]
        [free-vars (cddr expr)])
    ;; Store label in location pointed to by %esi
    (emit-identifier label stack-index env)
    (emit "movl %eax, 0(%esi)")

    (for-each
      (lambda (free offset)
        (emit-identifier free stack-index env)
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

(define (emit-identifier expr stack-index env)
  (let ([p (lookup expr env)])
    (if (not (pair? p))
        (error 'lookup (format "not found in env: ~a" expr))
        (case (cadr p)
          [var (emit "movl ~a(%esp), %eax" (caddr p))]
          [label (emit "movl $~a, %eax" (caddr p))]
          [free-var (emit "movl ~a(%edx), %eax" (caddr p))]))))

(define (emit-load-label label stack-index env)
  ;; Load the label into %eax
  (emit-identifier label stack-index env)
  ;; Now load what's at the label into %eax
  (emit "movl (%eax), %eax"))

(define (emit-foreign-call expr stack-index env)
  (let* ([call-target (format "scm_~a" (cadr expr))]

         ; For foreign calls, the args have to be above return address on stack,
         ; so we don't reserve _two_ slots above them, as we do for internal calls.
         ; Instead, we we reserve one slot for the current closure.
         [args-start (- stack-index wordsize)]

         ;; For foreign calls, we `(reverse)` the args because in the C
         ;; calling convention, they're in a different order
         [args (reverse (cddr expr))]
         [args (map list args
                    (map (lambda (i) (- args-start (* wordsize i)))
                         (iota (length args))))]

         [eval-stack-index (- args-start (* wordsize (length args)))])

    (for-each
      (lambda (arg) (begin (emit-expr (car arg) eval-stack-index env)
                           (emit "movl %eax, ~a(%esp)" (cadr arg))))
      args)


    (emit "movl %edx, ~a(%esp)" (- stack-index))

    ;; Since the return address needs a slot _below_ the args, we subtract
    ;; `eval-stack-index` (which is how much space the args take up on the stack)
    ;; and an additional `wordsize`, which is where the `call` will put the
    ;; return address
    (emit "subl $~a, %esp" (- (+ eval-stack-index wordsize)))
    (emit "call ~a" call-target)
    (emit "addl $~a, %esp" (- (+ eval-stack-index wordsize)))

    (emit "movl ~a(%esp), %edx" (- stack-index))))

(define (emit-expr expr stack-index env)
  (cond [(immediate? expr) (emit "movl $~a, %eax" (immediate-rep expr))]
        [(identifier? expr) (emit-identifier expr stack-index env)]
        [(let? expr) (emit-let (let-bindings expr) (let-body expr) stack-index env)]
        [(if? expr) (emit-if expr stack-index env)]
        [(prim-apply? expr) (emit-prim-apply expr stack-index env)]
        [(funcall? expr) (emit-funcall expr stack-index env #f)]
        [(tailcall? expr) (emit-funcall expr stack-index env #t)]
        [(closure? expr) (emit-closure expr stack-index env)]
        [(constant-ref? expr)
         (emit-load-label (cadr expr) stack-index env)]
        [(primitive-ref? expr)
         (emit-load-label (primitive-label (cadr expr)) stack-index env)]
        [(foreign-call? expr)
         (emit-foreign-call expr stack-index env)]
        [else (begin
                (display (format "unrecognized form: ~a\n" expr))
                (emit "movl $99, %eax"))]))

(define (args-to-stack-offsets args)
  (map (lambda (i) (* (- wordsize) (+ 1 i))) (iota (length args))))

(define (free-vars-to-closure-offsets free-vars)
  (map (lambda (i) (* wordsize (+ 1 i))) (iota (length free-vars))))

(define (emit-ensure-args-length op len stack-index env)
  ;; Check whether number of passed args (saved in %eax) matches
  ;; the number of args we expect
  (let ([skip-label (unique-label)]
        [jump-ins (case op
                    ['== "je"]
                    ['>= "jge"]
                    [else "je"])])
    (emit "cmpl $~a, %eax" len)
    (emit "~a ~a" jump-ins skip-label)
    (emit-expr `(funcall (primitive-ref error-args)) stack-index env)
    (emit-label skip-label)))

(define (emit-varargs-to-list vararg-offset min-args env)
  (let* ([done-label (unique-label)]
         [setup-loop (unique-label)]
         [loop-body (unique-label)])

    ;; %eax holds number of args in call
    ;; Save %eax to %ecx, which we use as a counter
    (emit "movl %eax, %ecx")

    ;; Check if we don't have any varargs
    (emit "cmpl $~a, %ecx" min-args)
    ;; If we have more, we build a list
    (emit "jg ~a" setup-loop)
    ;; Otherwise we just put empty-list in %eax and are done
    (emit "movl $~a, %eax" empty-list)
    (emit "jmp ~a" done-label)

    ;; Setup loop to build list
    (emit-label setup-loop)

    ;; cdr of last pair in list (first to be created) is empty-list
    (emit "movl $~a, %eax" empty-list)

    ;; We use %ecx to keep the offset of the next vararg we want to
    ;; turn into a pair.
    ;; We start with the "last" argument, which is lowest on the stack.
    ;; Minus "normal" args gives us number of varargs we have to
    ;; turn into pairs. `sub1` because the first vararg is part of args.
    (emit "subl $~a, %ecx" min-args)

    ;; Shift by two, to get from "number of varargs" to "wordsize * number of varargs"
    (emit "shl $~s, %ecx" 2)
    ;; Since the stack offset of last arg is already pointing at first vararg
    ;; we can ignore that offset
    (emit "subl $~a, %ecx" wordsize)

    ;; Loop body
    (emit-label loop-body)

    ;; Store %eax of last loop iteration (or setup) in cdr of pair
    (emit "movl %eax, ~a(%esi)" wordsize)

    ;; Here is the important part: getting value of current vararg from stack

    ;; We "shift" %esp by the offset in %ecx so that the original offset
    ;; of the last arg (which is the first vararg) points to the current
    ;; vararg
    (emit "sub %ecx, %esp")
    ;; Store last arg in car of first pair
    (emit "movl ~a(%esp), %eax" vararg-offset)
    ;; Now we restore %esp's old value
    (emit "add %ecx, %esp")

    (emit "movl %eax, 0(%esi)")
    ;; Save pointer in %eax and tag it, then increment heap ptr
    (emit "movl %esi, %eax")
    (emit "orl $~a, %eax" object-tag-pair)
    (emit "addl $~a, %esi" (* 2 wordsize))

    ;; Loop conditions:
    ;; Decrement %ecx, so that %esp shifted by %ecx points to the next, lower vararg
    (emit "subl $~s, %ecx" wordsize)
    ;; If we still have an offset: start loop again
    (emit "cmpl $0, %ecx")
    (emit "jge ~a" loop-body)

    ;; Now overwrite location of first vararg on stack with new pair
    (emit-label done-label)
    (emit "movl %eax, ~a(%esp)" vararg-offset)
    ;; Restore %eax
    (emit "movl %edi, %eax")))

(define (emit-label-code label env)
  (case (caadr label)
    ([code]
     (let*
       ([code-form (cadr label)]
        [name (car label)]

        [args (cadr code-form)]
        [flattened-args (flatten-lambda-vars args)]
        [args-len (length flattened-args)]

        [args-stack-offsets (args-to-stack-offsets flattened-args)]
        [free-vars (caddr code-form) ]
        [body (cadddr code-form) ]
        [inner-env
          (extend-env-frees free-vars
                            (free-vars-to-closure-offsets free-vars)
                            (extend-env-vars flattened-args
                                             args-stack-offsets
                                             env))]
        [locals-start (- (* wordsize (+ 1 args-len)))])

       (emit-label name)

       (if (or (lambda-single-vararg? args) (lambda-varargs? args))
           (let ([vararg-offset (caddr (lookup (lambda-vararg-name args) inner-env))])
             (emit-ensure-args-length '>= (sub1 args-len) locals-start env)
             (emit-varargs-to-list vararg-offset (sub1 args-len) inner-env))
           (emit-ensure-args-length '== args-len locals-start env))

       (emit-expr body locals-start inner-env)
       (emit "ret")))
    ([datum]
     (let ([name (car label)])
       (emit-local name)))))

(define (emit-primitive-init name stack-index env)
  (let ([label (primitive-label name)]
        [init-label (primitive-init-label name)])

    ; Save current closure
    (emit "movl %edx, ~a(%esp)" stack-index)

    ; Advance %esp and call the init function located at the label
    (emit "subl $~a, %esp" (- stack-index))
    (emit "call ~a" init-label)

    ; Restore the stack pointer afterwards and reload our current closure
    (emit "addl $~a, %esp" (- stack-index))
    (emit "movl ~a(%esp), %edx" stack-index)))

(define (emit-constant-init expr stack-index env)
  (let ([name (cadr expr)]
        [value-expr (caddr expr)])
    (emit-expr value-expr stack-index env)
    (emit "mov %eax, ~s" name)))

(define (emit-program labels constant-inits body env)
  (let* ([env-with-labels (extend-env-labels (append (map car labels) (primitive-labels primitives))
                                             env)])
    (emit ".text")
    (emit ".p2align 4,,15")

    (for-each (lambda (l) (emit-label-code l env-with-labels)) labels)

    (emit-function-header "scheme_entry")
    ; Save registers
    (emit "push %esi")
    (emit "push %edi")
    (emit "push %edx")

    ; Save heap pointer
    (emit "movl 16(%esp), %esi")

    ; Compile!

    ; First, turn primitives in `primitives` library into closures on heap
    ; This initializes all of them. Possible optimization: only init the ones
    ; that are used
    (for-each (lambda (p) (emit-primitive-init (car p) (- wordsize) env-with-labels)) primitives)

    ; Then, initialize the constants
    (for-each (lambda (c) (emit-constant-init c (- wordsize) env-with-labels)) constant-inits)

    ; Then the body
    (emit-expr body (- wordsize) env-with-labels)

    ; Restore registers
    (emit "pop %edx")
    (emit "pop %edi")
    (emit "pop %esi")

    (emit "ret")))

(define (precompile-annotate-free-vars expr)
  (define (walk-and-annotate expr free-vars)
    (cond
      ([immediate? expr] (list expr '()))
      ([primitive-ref? expr] (list expr '()))
      ([identifier? expr] (list expr (if (member expr free-vars) '() (list expr))))
      ([not (list? expr)] (list expr '()))

      ([foreign-call? expr]
       (let* ([results (walk-and-annotate (cddr expr) free-vars)]
              [annotated (car results)]
              [free-vars (cadr results)])
         (list `(foreign-call ,(cadr expr) ,@annotated) (remove-duplicates free-vars))))

      ([if? expr]
       (let* ([results (walk-and-annotate (cdr expr) free-vars)]
              [annotated (car results)]
              [free-vars (cadr results)])
         (list `(if ,@annotated) (remove-duplicates free-vars))))

      ([prim-apply? expr]
       (let* ([results (walk-and-annotate (cddr expr) free-vars)]
              [annotated (car results)]
              [free (cadr results)])
         (list `(prim-apply ,(cadr expr) ,@annotated) (remove-duplicates free))))

      ([lambda? expr]
       (let* ([args (lambda-vars expr)]
              [flattened-args (lambda-vars-flattened expr)]
              [body-form (caddr expr)]

              [results (walk-and-annotate body-form flattened-args)]
              [annotated (car results)]
              [free (cadr results)]

              [free-without-args (filter (lambda (v) (not (memq v flattened-args))) free)])
         (list `(lambda ,args ,free-without-args ,annotated)
               free-without-args)))

      ([let? expr]
       (let* ([binding-names (map car (let-bindings expr))]
              [binding-bodies (map cadr (let-bindings expr))]

              [annotated-bindings (walk-and-annotate binding-bodies free-vars)]
              [new-binding-bodies (car annotated-bindings)]
              [bindings-free-vars (cadr annotated-bindings)]

              [annotated-body (walk-and-annotate (let-body expr) (append binding-names free-vars))]
              [new-body (car annotated-body)]
              [body-free-vars (cadr annotated-body)]

              [all-free-vars (filter (lambda (v) (not (memq v binding-names)))
                                     (remove-duplicates (append body-free-vars bindings-free-vars)))])

         (list `(let ,(map list binding-names new-binding-bodies) ,@new-body)
               all-free-vars)))

      (else (let* ([results (map (lambda (p) (walk-and-annotate p free-vars)) expr)]
                   [annotated (map car results)]
                   [free (apply append (map cadr results))])
              (list annotated (remove-duplicates free))))))

    (let ([expr-and-vars (walk-and-annotate expr '())])
      (car expr-and-vars)))

(define (precompile-add-code-labels expr)
  (let* ([existing-label-forms (cadr expr)]
         [constant-inits (caddr expr)]
         [body-form (cadddr expr)])
  (define label-forms '())

  (define (transform expr)
    (cond
      ([immediate? expr] expr)
      ([identifier? expr] expr)
      ([not (list? expr)] expr)

      ([lambda? expr]
       (let* ((arguments (cadr expr))
              (free-vars (caddr expr))
              (body (transform (cadddr expr)))
              (label (unique-label))
              (code-expr `(code ,arguments ,free-vars ,body)))
         (begin
           (set! label-forms (cons (list label code-expr) label-forms))
           `(closure ,label ,@free-vars))))

      ([let? expr]
       (let* ((bindings (cadr expr))
              (body (cddr expr))
              (binding-names (map car bindings))
              (binding-values (map cadr bindings)))
         `(let ,(map list binding-names (map transform binding-values))
            ,@(map transform body))))

      ([if? expr] `(if ,@(map transform (cdr expr))))
      ([prim-apply? expr]
       `(prim-apply ,(prim-apply-fn expr)
                    ,@(map transform (prim-apply-args expr))))

      ([constant-ref? expr] expr)
      ([constant-init? expr] expr)
      ([primitive-ref? expr] expr)
      ([foreign-call? expr] expr)
      (else `(funcall ,(transform (car expr))
                      ,@(map transform (cdr expr))))))

  (let* ((transformed-expr (transform body-form))
        (new-labels-form (if (null? existing-label-forms)
                             label-forms
                             (append existing-label-forms label-forms))))
    `(labels ,new-labels-form
             ,constant-inits
             ,transformed-expr))))

(define (precompile-add-constants expr)
  (define label-forms '())
  (define constant-inits '())

  ;; `string-constant->make-string` rewrites `string` expressions into
  ;; a `make-string` expression with multiple `string-set` following:
  ;; Example:
  ;; input: (string "Hello World")
  ;; output:
  ;;  (let ([s (prim-apply make-string 11)])
  ;;     (prim-apply string-set! s 0 #\H)
  ;;     (prim-apply string-set! s 1 #\e)
  ;;     (prim-apply string-set! s 2 #\l)
  ;;     (prim-apply string-set! s 3 #\l)
  ;;     (prim-apply string-set! s 4 #\o)
  ;;     (prim-apply string-set! s 5 #\space)
  ;;     (prim-apply string-set! s 6 #\W)
  ;;     (prim-apply string-set! s 7 #\o)
  ;;     (prim-apply string-set! s 8 #\r)
  ;;     (prim-apply string-set! s 9 #\l)
  ;;     (prim-apply string-set! s 10 #\d)
  ;;     s)
  (define (string-constant->make-string expr)
    (let* ([chars (map translate-quote (string->list expr))]
           [len (length chars)])
      `(let ((s (prim-apply make-string ,len)))
         ,@(map (lambda (c i) `(prim-apply string-set! s ,i ,c)) chars (iota len))
         s)))

  (define (vector-constant->make-vector expr)
    (let* ([chars (map translate-quote (vector->list expr))]
           [len (length chars)])
      `(let ((s (prim-apply make-vector ,len)))
         ,@(map (lambda (c i) `(prim-apply vector-set! s ,i ,c)) chars (iota len))
         s)))

  (define (translate-quote expr)
    (cond
      [(immediate? expr) expr]
      [(pair? expr)
      (list 'prim-apply 'cons (translate-quote (car expr)) (translate-quote (cdr expr)))]
      [(vector? expr) (vector-constant->make-vector expr)]
      [(string? expr) (string-constant->make-string expr)]
      [(symbol? expr) `(funcall (primitive-ref string->symbol) ,(translate-quote (symbol->string expr)))]
      [else (error 'translate-quote (format "don't know how to quote ~s" expr))]))

  (define (transform expr)
    (cond
      ([or (string? expr) (vector? expr)] (transform `(quote ,expr)))

      ([not (list? expr)] expr)
      ([null? expr] expr)

      ([foreign-call? expr] `(foreign-call ,(cadr expr) ,@(map transform (cddr expr))))

      ([and (quote? expr) (immediate? (cdr expr))] (cdr expr))

      ([and (quote? expr) (immediate? (cdr expr))] (cdr expr))
      ([and (quote? expr) (assoc expr label-forms)] => cadr)

      [(quote? expr)
       (let* ((label (unique-label))
              (translated-quote (translate-quote (cadr expr))))
         (begin
           (set! label-forms (cons (list label '(datum)) label-forms))
           (set! constant-inits (cons `(constant-init ,label ,translated-quote) constant-inits))
           `(constant-ref ,label)))]

      ([list? expr] (map transform expr))

      (else expr)))

  (let ((transformed-expr (transform expr)))
    `(labels ,label-forms
             ,constant-inits
             ,transformed-expr)))

(define (precompile-transform-tailcalls expr)
  (let* ([label-forms (cadr expr)]
         [constant-inits (caddr expr)]
         [body-form (cadddr expr)])

    (define (transform expr)
      (cond
        ([if? expr] `(if ,(cadr expr) ,@(map transform (cddr expr))))
        ([let? expr] (let ([rev (reverse expr)])
                       (reverse (cons (transform (car rev)) (cdr rev)))))
        ([funcall? expr] (cons 'tailcall (cdr expr)))
        (else expr)))

    (define (transform-label label-form)
      (case (caadr label-form)
        ([code]
          (let ((code-form (cadr label-form)))
            `(,(car label-form)
               (code ,(cadr code-form)
                     ,(caddr code-form)
                     ,(transform (cadddr code-form))))))

        (else label-form)))

    `(labels ,(map transform-label label-forms) ,constant-inits ,body-form)))


(define (precompile-transform-assignments expr)
  (let ([assigned '()])
    (define (set-variable-assigned! v)
      (unless (member v assigned)
        (set! assigned (cons v assigned))))

    (define (variable-assigned v) (member v assigned))

    (define (mark expr)
      (if (null? expr) expr
          (begin
            (when (set? expr) (set-variable-assigned! (set-variable expr)))
            (when (list? expr) (for-each mark expr)))))

    (define (make-single-elem-vector elem)
      `(let ([v (prim-apply make-vector 1)])
         (prim-apply vector-set! v 0 ,elem)
         v))

    (define (transform expr)
      (cond
        [(set? expr) 
         `(prim-apply vector-set! ,(set-variable expr) 0 ,(transform (set-value expr)))]
        [(lambda? expr)
         (let* ([vars (filter variable-assigned (lambda-vars-flattened expr))])
           `(lambda
              ,(lambda-vars expr)
              ,@(if (null? vars)
                    (transform (lambda-body expr))
                    (list `(let
                             ,(map (lambda (v) (list v (make-single-elem-vector v))) vars)
                             ,@(transform (lambda-body expr)))))))]
        [(let? expr)
         `(let
            ,(map (lambda (binding)
                    (let ([var (car binding)]
                          [val (transform (cadr binding))])
                      (list var
                            (if (variable-assigned var)
                                (make-single-elem-vector val)
                                val))))
                  (let-bindings expr))
            ,@(transform (let-body expr)))]
        [(list? expr) (map transform expr)]
        [(and (symbol? expr) (variable-assigned expr)) `(prim-apply vector-ref ,expr 0)]
        [else expr]))

    (mark expr)
    (transform expr)))

(define (precompile-macro-expansion expr)
  (define (gen-name name count)
    (string->symbol (string-append (symbol->string name) "_" (number->string count))))

  (define unique-name
    (let ([counts '()])
      (lambda (name)
        (cond
          [(assv name counts) => (lambda (p) (let ([count (cdr p)])
                                               (set-cdr! p (add1 count))
                                               (gen-name name count)))]
          [(or (primitive-name? name) (builtin-name? name))
           (set! counts (cons (cons name 1) counts))
           (unique-name name)]
          [else
            (set! counts (cons (cons name 1) counts))
            name]))))

  (define (builtin-name? symbol)
    (or (member symbol builtin-forms)
        (member symbol builtin-primitives)))

  (define (bulk-extend-env vars vals env)
    (append (map list vars vals) env))

  (define (lookup-name name env)
    (cond
      [(lookup name env) => cadr]
      [else #f]))

  (define (map-lambda-params f params)
    (cond
      [(list? params) (map (lambda (x)
                             (if (pair? x)
                                 (cons (f (car x)) (cdr x))
                                 (f x)))
                           params)]
      [(pair? params) (cons (f (car params))
                            (map-lambda-params f (cdr params)))]
      [else (f params)]))

  (define (map-transform ls env) (map (lambda (e) (transform e env)) ls))

  (trace-define (transform expr env)
    (cond
      [(primitive-name? expr) `(primitive-ref ,expr)]
      [(identifier? expr)
       (or (lookup-name expr env)
           (and (primitive-name? expr) expr)
           (and (builtin-name? expr) expr)
           (error 'macro-alpha-conversion (format "undefined variable ~s" expr)))]
      [(primitive-ref? expr) expr]
      [(foreign-call? expr) `(foreign-call ,(cadr expr) ,@(map-transform (cddr expr) env))]
      [(funcall? expr) `(funcall ,@(map-transform (cdr expr) env))]
      [(if? expr)
       `(if ,(transform (if-condition expr) env)
            ,(transform (if-consequence expr) env)
            ,(if (if-alternative? expr) (transform (if-alternative expr) env) '()))]
      ; [(lambda? expr)
      ;  `(lambda ,(lambda-vars expr) ,@(map-transform (lambda-body expr) env))]
      ;
      [(lambda? expr)
       (let* ([params (lambda-vars-flattened expr)]
              [new-env (bulk-extend-env params (map unique-name params) env)])
         `(lambda
            ,(map-lambda-params (lambda (v) (lookup-name v new-env)) (lambda-vars expr))
            ,@(transform (lambda-body expr) new-env)))]

      ([prim-apply? expr]
       `(prim-apply ,(prim-apply-fn expr) ,@(map-transform (prim-apply-args expr) env)))
      ; [(let? expr)
      ;  (let ([bindings (map (lambda (b) (list (car b) (transform (cadr b) env))) (let-bindings expr))])
      ;    `(let ,bindings ,@(map-transform (let-body expr) env)))]
      [(let? expr)
       (let* ([bindings (let-bindings expr)]
              [names (map car bindings)]
              [new-env (bulk-extend-env names (map unique-name names) env)])
         `(let
            ,(map (lambda (binding)
                    (list (lookup-name (car binding) new-env)
                          (transform (cadr binding) env)))
                  bindings)
            ,@(map-transform (let-body expr) new-env)))]
      [(let*? expr)
       (let* ([first-binding (car (let-bindings expr))]
              [transformed-first-binding (list (list (car first-binding)
                                                     (transform (cadr first-binding) env)))]
              [rest-bindings (cdr (let-bindings expr))])
         (transform (if (null? rest-bindings)
             `(let ,transformed-first-binding ,@(let-body expr))
             `(let ,transformed-first-binding (let* ,rest-bindings ,@(let-body expr))))
                    env))]
      [(letrec? expr)
       (let* ([bindings (let-bindings expr)]
              [new-bindings (map (lambda (b) `(,(car b) #f)) bindings)]
              [inits (map (lambda (b) `(set! ,(car b) ,(cadr b))) bindings)])
         (transform `(let ,new-bindings ,@inits ,@(let-body expr)) env))]
      [(and? expr)
       (cond [(null? (cdr expr)) #t]
             [(null? (cddr expr)) (transform (cadr expr) env)]
             [else (transform `(if ,(cadr expr) (and ,@(cddr expr)) #f) env)])]
      [(list? expr) (map-transform expr env)]
      [else expr]))

  (transform expr (new-env)))

(define (sanitize-label name)
  (define (replace-char c)
    (case c
      [(#\-) #\_]
      [(#\!) #\b]
      [(#\=) #\e]
      [(#\>) #\g]
      [(#\?) #\p]
      [else c]))

  (list->string (map replace-char (string->list name))))

(define (primitive-label name)
  (string->symbol (sanitize-label (format "P_~a" name))))

(define (primitive-init-label name)
  (string->symbol (format "~a_init" (primitive-label name))))

(define (primitive-labels primitives)
  (let ([names (map car primitives)])
    (append (map primitive-label names)
            (map primitive-init-label names))))

(define primitives
  ;; TODO: we should really have macros for this, or define and parse a
  ;; separate file
  (list
    (list 'add-and-add-four '(lambda (x y) (prim-apply + 4 (prim-apply + x y))))
    (list 'add-three '(lambda (x) (prim-apply + 3 x)))
    (list 'add-four '(lambda (x) (prim-apply + 1 (add-three x))))
    (list 'calls-another-lambda '(lambda (x)
                                   (let ((g (lambda (x) (prim-apply + 1 x))))
                                     (g x))))
    (list 'length '(lambda (lst) (if (prim-apply null? lst)
                                     0
                                     (prim-apply add1 (length (prim-apply cdr lst))))))
    (list 'error '(lambda (origin message)
                    (foreign-call "error" origin message)))
    (list 'error-apply '(lambda ()
                          (foreign-call "error" "system" "attempt to apply non-procedure")))
    (list 'error-args '(lambda ()
                         (foreign-call "error" "system" "wrong number of arguments")))
    (list 'error-no-pair '(lambda ()
                            (foreign-call "error" "system" "argument not a pair")))

    (list 'string=? '(lambda (s1 s2)
                       (letrec ([rec (lambda (index)
                                       (if (prim-apply eq? index (prim-apply string-length s1))
                                           #t
                                           (if (prim-apply char=? (prim-apply string-ref s1 index) (prim-apply string-ref s2 index))
                                               (rec (prim-apply add1 index))
                                               #f)))])
                         (and (prim-apply string? s1) (prim-apply string? s2)
                              (prim-apply eq? (prim-apply string-length s1) (prim-apply string-length s2))
                              (rec 0)))))
    (list 'string '(lambda chars
                     (let ([s (prim-apply make-string (length chars))])
                       (letrec ([fill-chars (lambda (index args)
                                              (if (prim-apply not (prim-apply null? args))
                                                  (let ((arg (prim-apply car args))
                                                        (rest (prim-apply cdr args)))
                                                    (prim-apply string-set! s index arg)
                                                    (fill-chars (prim-apply add1 index) rest))))])
                         (fill-chars 0 chars)
                         s))))

    (list 'symbols_list '(prim-apply cons '() '()))
    (list 'string->symbol '(lambda (s) (let ((existing (find_symbol s)))
                                         (if existing existing
                                             (let ((new (prim-apply make-symbol s)))
                                               (prim-apply set-car! symbols_list (prim-apply cons new (prim-apply car symbols_list)))
                                               new)))))
    (list 'find_symbol '(lambda (str)
                          (letrec ([rec (lambda (ls)
                                          (if (prim-apply null? ls) #f
                                              (if (string=? str (prim-apply symbol->string (prim-apply car ls)))
                                                  (prim-apply car ls)
                                                  (rec (prim-apply cdr ls)))))])
                            (rec (prim-apply car symbols_list)))))))

(define (precompile expr)
  (precompile-transform-tailcalls
    (precompile-add-code-labels
      (precompile-add-constants
        (precompile-annotate-free-vars
          (precompile-transform-assignments
            (precompile-macro-expansion expr)))))))

(define (compile-primitives primitives)
  (define (compile-primitive p)
    (let* ([name (car p)]
           [code (cadr p)]

           ;; Step 1) precompile code
           [pre (precompile code)]

           [labels (cadr pre)]
           [const-inits (caddr pre)]
           [body (cadddr pre)]

           [env (extend-env-labels (append (map car labels)
                                           (primitive-labels primitives))
                                   '())])

      ;; Step 2) Emit global label into which initialized primitive (e.g. a
      ;; closure) will be stored
      (emit-global (primitive-label name))

      ;; Step 3) Emit the code labels belonging to single primitive (e.g.
      ;; lambdas used by primitive)
      (for-each (lambda (l) (emit-label-code l env)) labels)

      ;; Step 4) Emit the global function header that gets called by program
      ;; to initialize the primitives
      (emit-function-header (primitive-init-label name))

      ;; Step 5) Initialize the constants needed to initialize the primitive
      (for-each (lambda (c) (emit-constant-init c (- wordsize) env)) const-inits)

      ;; Step 6) Emit the `body` that will then leave the primitive in %eax
      (emit-expr body (- wordsize) env)

      ;; Step 7) Move %eax into the global label and return
      (emit "mov %eax, ~s" (primitive-label name))
      (emit "ret")))

    (set! label-count 0)
    (for-each compile-primitive primitives))

(define (compile-primitives-to-file filename)
  (let ([p (open-output-file filename 'replace)])
    (parameterize ([compile-port p])
      (compile-primitives primitives))
    (close-output-port p)))

(define (compile-program expr)
  (set! label-count 0)

  (let ([precompiled (precompile expr)])
    (emit-program (cadr precompiled)
                  (caddr precompiled)
                  (cadddr precompiled)
                  (new-env))))
