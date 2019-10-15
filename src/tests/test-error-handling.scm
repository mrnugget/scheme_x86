(add-tests-with-stderr-output "error"
  ;; TODO: no symbols
  [(error "foobar" "this is an error") => "Exception in foobar: this is an error\n"])


(add-tests-with-stderr-output "calling non function"
  [(let ([f 6])
     (f f)) => "Exception in system: attempt to apply non-procedure\n"]
  [(let ([f 6])
     (f (f))) => "Exception in system: attempt to apply non-procedure\n"]
  [(1 2 3) => "Exception in system: attempt to apply non-procedure\n"])

(add-tests-with-stderr-output "wrong number of args"
  [(let ([f (lambda (x) (prim-apply zero? x))])
     (f)) => "Exception in system: wrong number of arguments\n"]
  [(let ([f (lambda () #t)])
    (f 1)) => "Exception in system: wrong number of arguments\n"]
  [(let ([f (lambda (x y) (prim-apply + x y))])
    (f 3 4 5)) => "Exception in system: wrong number of arguments\n"])

(add-tests-with-stderr-output "wrong arg to car/cdr"
  [(prim-apply car 1) => "Exception in system: argument not a pair\n"]
  [(prim-apply cdr 1) => "Exception in system: argument not a pair\n"])
