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

(define (emit-prim-apply-args expr stack-index)
  (for-each
    (lambda (e) (emit-expr e stack-index))
    (reverse (prim-apply-args expr))))

(define (emit-prim-apply expr stack-index)
  (case (prim-apply-fn expr)
    [(add1)
     (emit-prim-apply-args expr stack-index)
     (emit "addl $~a, %eax" (immediate-rep 1))]
    [(sub1)
     (emit-prim-apply-args expr stack-index)
     (emit "subl $~a, %eax" (immediate-rep 1))]
    [(fixnum->char)
     (emit-prim-apply-args expr stack-index)
     (emit "shl $~s, %eax" (- char-shift fixnum-shift))
     (emit "or $~s, %eax" char-tag)]
    [(char->fixnum)
     (emit-prim-apply-args expr stack-index)
     (emit "shr $~s, %eax" (- char-shift fixnum-shift))]
    [(zero?)
     (emit-prim-apply-args expr stack-index)
     (emit-eax-eq? 0)]
    [(null?)
     (emit-prim-apply-args expr stack-index)
     (emit-eax-eq? empty-list)]
    [(fixnum?)
     (emit-prim-apply-args expr stack-index)
     (emit "andl $~a, %eax" fixnum-mask)
     (emit-eax-eq? fixnum-tag)]
    [(boolean?)
     (emit-prim-apply-args expr stack-index)
     (emit "andl $~a, %eax" bool-mask)
     (emit-eax-eq? bool-tag)]
    [(char?)
     (emit-prim-apply-args expr stack-index)
     (emit "andl $~a, %eax" char-mask)
     (emit-eax-eq? char-tag)]
    [(+)
     (emit-expr (prim-apply-arg-1 expr) stack-index)
     (emit "movl %eax, ~a(%esp)" stack-index)
     (emit-expr (prim-apply-arg-2 expr) (- stack-index wordsize))
     (emit "addl ~a(%esp), %eax" stack-index)]))

(define (emit-eax-eq? val)
  (emit "cmpl $~a, %eax" val)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" bool-shift)
  (emit "orl $~a, %eax" bool-tag))

(define (emit-expr expr stack-index)
  (cond [(immediate? expr) (emit "movl $~a, %eax" (immediate-rep expr))]
        [(prim-apply? expr) (emit-prim-apply expr stack-index)]
        [else (emit "movl $99, %eax")]))

(define (emit-program expr)
  (emit ".text")
  (emit ".p2align 4,,15")
  (emit ".globl scheme_entry")
  (emit ".type scheme_entry, @function")
  (emit-label "scheme_entry")
  (emit-expr expr (- wordsize))
  (emit "ret"))
