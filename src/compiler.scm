(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))

(define (emit-program expr)
  (emit "  .text")
  (emit "  .p2align 4,,15")
  (emit ".globl scheme_entry")
  (emit "  .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "  movl $~a, %eax" expr)
  (emit "  ret"))
