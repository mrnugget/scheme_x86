(add-tests-with-string-output "let expressions"
  [(let ([x 3]) x)  => "3\n"]
  [(let ([x 3] [y 5]) (prim-apply + x y))  => "8\n"])
