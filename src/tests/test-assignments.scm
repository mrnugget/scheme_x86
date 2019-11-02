(add-tests-with-string-output "assignments"
  [(let ([f (lambda (c)
              (prim-apply cons (lambda (v) (set! c v)) (lambda () c)))])
      (let ([p (f 0)]) ((prim-apply car p) 12) ((prim-apply cdr p))))
    => "12\n"]
  [(let ([f 1]) (set! f 2) f) => "2\n"]
  ;; macro expansion in `(set! ...)`
  [(let ([f 1]) (set! f (+ 2 3)) f) => "5\n"])
