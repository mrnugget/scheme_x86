(add-tests-with-string-output "symbol type"
 [(let ((s "foobar"))
    (prim-apply symbol? s)) => "#f\n"]

 [(let ((s "foobar"))
    (prim-apply make-symbol s)) => "foobar\n"]

 [(let ((s "foobar"))
    (let ((sym (prim-apply make-symbol s)))
      (prim-apply symbol? sym))) => "#t\n"]

 [(prim-apply symbol? '()) => "#f\n"]
 [(prim-apply symbol? "") => "#f\n"]
 [(prim-apply symbol? '(1 2)) => "#f\n"]
 [(prim-apply symbol? '#()) => "#f\n"]
 [(prim-apply symbol? (lambda (x) x)) => "#f\n"])

(add-tests-with-string-output "quoting symbols"
 [(prim-apply symbol? 'foo) => "#t\n"]
 [(prim-apply string? 'foo) => "#f\n"]
 [(prim-apply pair? 'foo) => "#f\n"]
 [(prim-apply vector? 'foo) => "#f\n"]
 [(prim-apply null? 'foo) => "#f\n"]
 [(prim-apply boolean? 'foo) => "#f\n"]

 ['foo => "foo\n"]
 ['(foo bar baz) => "(foo bar baz)\n"]
 ['(foo foo foo foo foo foo foo foo foo foo foo)
  => "(foo foo foo foo foo foo foo foo foo foo foo)\n"]

 [(prim-apply eq? 'foo 'bar) => "#f\n"]
 [(prim-apply eq? 'foo 'foo) => "#t\n"])
