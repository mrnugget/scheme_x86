
(add-tests-with-string-output "lambdas with varargs"
  ;; One vararg
  [((lambda (a b . c) (prim-apply pair? c)) 1 2 3) => "#t\n"]
  [((lambda (a b . c) (prim-apply car c)) 1 2 3) => "3\n"]
  [((lambda (a b . c) (prim-apply cdr c)) 1 2 3) => "()\n"]
  ;; No vararg
  [((lambda (a b . c) (prim-apply null? c)) 1 2) => "#t\n"]
  [((lambda (a b . c) (prim-apply pair? c)) 1 2) => "#f\n"]
  ;; Two varargs
  [((lambda (a b . c) (prim-apply pair? c)) 1 2 3 4) => "#t\n"]
  [((lambda (a b . c) (prim-apply car c)) 1 2 3 4) => "3\n"]
  [((lambda (a b . c) (prim-apply car (prim-apply cdr c))) 1 2 3 4) => "4\n"]
  [((lambda (a b . c) (prim-apply cdr (prim-apply cdr c))) 1 2 3 4) => "()\n"]
  ;; Three varargs
  [((lambda (a b . c) (prim-apply pair? c)) 1 2 3 4 5) => "#t\n"]
  [((lambda (a b . c) (prim-apply car c)) 1 2 3 4 5) => "3\n"]
  [((lambda (a b . c) (prim-apply car (prim-apply cdr c))) 1 2 3 4 5) => "4\n"]
  [((lambda (a b . c) (prim-apply car (prim-apply cdr (prim-apply cdr c)))) 1 2 3 4 5) => "5\n"]
  [((lambda (a b . c) (prim-apply cdr (prim-apply cdr (prim-apply cdr c)))) 1 2 3 4 5) => "()\n"])

(add-tests-with-stderr-output "lambdas with varargs and wrong number of args"
  [((lambda (a b . c) (prim-apply + a b)) 1) => "Exception in system: wrong number of arguments\n"]
  [((lambda (a b . c) (prim-apply + a b)) 1 2) => ""]
  [((lambda (a b . c) (prim-apply + a b)) 1 2 3 4 5) => ""])
