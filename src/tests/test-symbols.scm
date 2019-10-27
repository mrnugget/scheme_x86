(add-tests-with-string-output "symbol type"
 [(let ((s "foobar"))
    (prim-apply symbol? s)) => "#f\n"]

 [(let ((s "foobar"))
    (prim-apply make-symbol s)) => "foobar\n"]

 [(let ((s "foobar"))
    (let ((sym (prim-apply make-symbol s)))
      (prim-apply symbol? sym))) => "#t\n"]

 [(let ((s "foobar"))
    (let ((sym (prim-apply make-symbol s)))
      (prim-apply symbol? (prim-apply symbol->string sym)))) => "#f\n"]

 [(prim-apply symbol? '()) => "#f\n"]
 [(prim-apply symbol? "") => "#f\n"]
 [(prim-apply symbol? '(1 2)) => "#f\n"]
 [(prim-apply symbol? '#()) => "#f\n"]
 [(prim-apply symbol? (lambda (x) x)) => "#f\n"])

(add-tests-with-string-output "symbols linked list"
 [(find_symbol "foobar") => "#f\n"]
 [(prim-apply null? (prim-apply car symbols_list)) => "#t\n"]
 [(prim-apply null? (prim-apply cdr symbols_list)) => "#t\n"]

 [(let* ((s1 (string->symbol "foobar"))
         (ls (prim-apply car symbols_list)))
    (string=? "foobar" (prim-apply symbol->string (prim-apply car ls))))
    => "#t\n"]

 [(let* ((s1 (string->symbol "foobar"))
         (s2 (string->symbol "foobar")))
    (prim-apply car symbols_list)) => "(foobar)\n"])

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
