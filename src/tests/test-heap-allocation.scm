(add-tests-with-string-output "pairs"
  [(prim-apply cons 1 2) => "(1 . 2)\n"]
  [(prim-apply cons () ()) => "(() . ())\n"]
  [(prim-apply cons 1 (prim-apply cons 2 3)) => "(1 2 . 3)\n"]
  [(prim-apply cons 1 (prim-apply cons 2 (prim-apply cons 3 4))) => "(1 2 3 . 4)\n"])
