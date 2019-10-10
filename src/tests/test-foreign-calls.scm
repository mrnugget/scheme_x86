(add-tests-with-string-output "foreign calls"
 [(foreign-call "hello_from_c") => "Hello from C!\n()\n"]
 [(foreign-call "add_two" 2) => "4\n"]

 [(let ((x (foreign-call "add_two" 2))
        (y (foreign-call "add_two" 4)))
    (prim-apply + x y)) => "10\n"]

 [(let ((f (lambda (x y)
             (prim-apply +
                         (foreign-call "add_two" x)
                         (foreign-call "add_two" y)))))
    (f 2 4)) => "10\n"]

 [(foreign-call "print_three_args" 55 66 77) => "arg1=55, arg2=66, arg3=77\n()\n"])
