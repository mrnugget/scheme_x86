(add-tests-with-string-output "apply"
  ; Sanity checks
  [(let ([g (lambda (x) (if (prim-apply pair? x) #f (prim-apply + x x)))])
      (g 9999)) => "19998\n"]

  [(let ([g (lambda (x) (if (prim-apply pair? x) #f (prim-apply + x x)))])
      (g '(9999))) => "#f\n"]

  ;; Arg list with 0 element
  [(let ([g (lambda () 9999)])
      (apply g '())) => "9999\n"]

  ;; Arg list with 1 element
  [(let ([g (lambda (x) x)])
      (apply g '(9999))) => "9999\n"]
  ;
  ;; Arg list with 2 elements
  [(let ([g (lambda (x y) (prim-apply cons x y))])
      (apply g '(9999 8888))) => "(9999 . 8888)\n"]

  ;; Arg list with 3 elements
  [(let ([g (lambda (x y z) (prim-apply + (prim-apply + x y) z))])
      (apply g '(9999 8888 7777))) => "26664\n"]

  ;; Arg list with 4 elements
  [(let ([g (lambda (x y z xx) (prim-apply + xx (prim-apply + (prim-apply + x y) z)))])
      (apply g '(9999 8888 7777 6666))) => "33330\n"]

  ;; Arg list with 5 elements
  [(let ([g (lambda (a b c d e f) (string a b c d e f))])
    (apply g '(#\a #\b #\c #\d #\e #\f))) => "\"abcdef\"\n"])
