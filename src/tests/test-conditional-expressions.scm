(add-tests-with-string-output "if expressions"
  [(if #t 12 13) => "12\n"]
  [(if #f 12 13) => "13\n"])
