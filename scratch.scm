(load "src/compiler.scm")
(define in '(let ([f (lambda (c) (cons (lambda (v) (set! c v)) (lambda () c)))]) (let ([p (f 0)]) ((car p) 12) ((cdr p)))))
(assignment-conversion in)
