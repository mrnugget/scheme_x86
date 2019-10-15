(add-tests-with-stderr-output "error"
  ;; TODO: no symbols
  [(error "foobar" "this is an error") => "Exception in foobar: this is an error\n"])


(add-tests-with-stderr-output "calling non function"
  [(let ([f 6])
     (f f)) => "Exception in system: attempt to apply non-procedure\n"]
  [(let ([f 6])
     (f (f))) => "Exception in system: attempt to apply non-procedure\n"]
  [(1 2 3) => "Exception in system: attempt to apply non-procedure\n"])
