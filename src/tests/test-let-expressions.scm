(add-tests-with-string-output "let expressions"
  [(let ([x 3]) x)  => "3\n"]
  [(let ([x 3] [y 5]) (prim-apply + x y))  => "8\n"]
  [(let ([x (prim-apply + 1 2)]) x) => "3\n"]
  [(let ([x (prim-apply + 1 2)]) (let ([y (prim-apply + x 3)]) y)) => "6\n"]
  [(let ([x (prim-apply + 1 2)]) (let ([x (prim-apply + 3 3)]) x)) => "6\n"])
