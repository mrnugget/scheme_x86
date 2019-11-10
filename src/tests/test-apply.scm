(add-tests-with-string-output "apply"
  ;; Sanity checks
  ; [(let ([g (lambda (x) (if (prim-apply pair? x) #f (prim-apply + x x)))])
  ;     (g 9999)) => "19998\n"]
  ; )
  ; [(let ([g (lambda (x) (if (prim-apply pair? x) #f (prim-apply + x x)))])
  ;     (g '(9999))) => "#f\n"]
  ;
  ;; Arg list with 1 element
  [(let ([g (lambda (x) x)])
      (apply g '(9999))) => "9999\n"]
  ; )
  ;
  ;; Arg list with 2 elements
  [(let ([g (lambda (x y) (prim-apply cons x y))])
      (apply g '(9999 8888))) => "(9999 . 8888)\n"])
