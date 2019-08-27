(add-tests-with-string-output "pairs"
  [(prim-apply cons 1 2) => "(1 . 2)\n"]
  [(prim-apply cons () ()) => "(() . ())\n"]
  [(prim-apply cons 1 (prim-apply cons 2 3)) => "(1 2 . 3)\n"]
  [(prim-apply cons 1 (prim-apply cons 2 (prim-apply cons 3 4))) => "(1 2 3 . 4)\n"]
  [(prim-apply car (prim-apply cons 1 2)) => "1\n"]
  [(prim-apply cdr (prim-apply cons 1 2)) => "2\n"]
  [(prim-apply pair? (prim-apply cons 1 2)) => "#t\n"]
  )
