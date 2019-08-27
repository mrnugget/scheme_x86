(add-tests-with-string-output "pairs"
  [(prim-apply cons 1 2) => "(1 . 2)\n"])
