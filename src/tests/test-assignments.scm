(add-tests-with-string-output "assignments"
  [(let ([f (lambda (c)
              (prim-apply cons (lambda (v) (set! c v)) (lambda () c)))])
      (let ([p (f 0)]) ((prim-apply car p) 12) ((prim-apply cdr p))))
    => "12\n"])
