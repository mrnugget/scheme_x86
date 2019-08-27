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
(define pair-tag 1)

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

(define (prim-apply? expr) (eq? (car expr) 'prim-apply))
(define (prim-apply-fn expr) (cadr expr))
(define (prim-apply-args expr) (cddr expr))
(define (prim-apply-arg-1 expr) (caddr expr))
(define (prim-apply-arg-2 expr) (cadddr expr))

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
     ;; TODO: this can be moved and get rid of `mov` to %eax?
     (emit "movl ~a(%esp), %eax" stack-index)
     (emit "movl %eax, 0(%esi)")
     ;; save pointer and tag it, then increment heap ptr
     (emit "movl %esi, %eax")
     (emit "orl $~a, %eax" pair-tag)
     (emit "addl $~a, %esi" (* 2 wordsize))
     ]))


(define (emit-eax-eq? val)
  (emit "cmpl $~a, %eax" val)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" bool-shift)
  (emit "orl $~a, %eax" bool-tag))

(define (variable? expr) (symbol? expr))
(define (let? expr) (eq? 'let (car expr)))
(define (let-bindings expr) (cadr expr))
(define (let-body expr) (caddr expr))
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
        (emit-expr body stack-index e)
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
