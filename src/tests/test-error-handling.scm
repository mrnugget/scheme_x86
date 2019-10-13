(add-tests-with-stderr-output "error"
  ;; TODO: no symbols
  [(error "foobar" "this is an error") => "Exception in foobar: this is an error\n"])
