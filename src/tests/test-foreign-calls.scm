(add-tests-with-string-output "foreign calls"
 [(foreign-call "hello_from_c") => "Hello from C!\n()\n"])
