(load "src/compiler.scm")

(define in '(letrec ([f 12] [g 13]) (prim-apply + f g)))

(pretty-print (let* ([bindings (let-bindings in)]
       [new-bindings (map (lambda (b) `(,(car b) #f)) bindings)]
       [inits (map (lambda (b) `(set! ,(car b) ,(cadr b))) bindings)])
  `(let ,new-bindings ,@inits ,@(let-body in))))
