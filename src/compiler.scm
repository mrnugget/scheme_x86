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

(define fixnum-shift 2)

(define char-tag 15)
(define char-shift 8)

(define bool-tag 31)
(define bool-shift 8)

(define empty-list 47) ;; 00101111 - 0x2f

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

(define (emit-prim-apply expr)
  (case (prim-apply-fn expr)
    [(add1)
     (for-each emit-expr (reverse (prim-apply-args expr)))
     (emit "addl $~a, %eax" (immediate-rep 1))]
    [(sub1)
     (for-each emit-expr (reverse (prim-apply-args expr)))
     (emit "subl $~a, %eax" (immediate-rep 1))]))

(define (emit-expr expr)
  (cond [(immediate? expr) (emit "movl $~a, %eax" (immediate-rep expr))]
        [(prim-apply? expr) (emit-prim-apply expr)]
        [else (emit "movl $99, %eax")]))

(define (emit-program expr)
  (emit ".text")
  (emit ".p2align 4,,15")
  (emit ".globl scheme_entry")
  (emit ".type scheme_entry, @function")
  (emit-label "scheme_entry")
  (emit-expr expr)
  (emit "ret"))
