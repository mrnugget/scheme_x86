(load "compiler.scm")

(define (build)
  (unless (zero? (system "make emitted --quiet"))
    (error 'make "Could not build target.")))

(define (execute)
  (unless (zero? (system "./emitted > emitted.out"))
    (error 'make "Produced program exited abnormally.")))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (with-input-from-file "emitted.out"
        (lambda ()
          (let f ()
            (let ([c (read-char)])
              (cond
               [(eof-object? c) (void)]
               [else (display c) (f)]))))))))

(define (compile-program expr)
  (let ([p (open-output-file "emitted.s" 'replace)])
    (parameterize ([compile-port p])
      (emit-program expr))
    (close-output-port p)))

(define (run expr)
  (compile-program expr)
  (build)
  (execute)
  (get-string))

(define all-tests '())

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string]            ...)
     (set! all-tests
           (cons
            '(test-name [expr string  output-string] ...)
            all-tests))]))

(define (test-with-string-output test-id expr expected-output)
  (unless (string=? expected-output (run expr))
    (error 'test (format "Output mismatch for test ~s, expected ~s, got ~s."
                         test-id expected-output (get-string)))))

(define (test-one test-id test)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)])
    (printf "Test ~s:~s ..." test-id expr)
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out)]
     [else (error 'test (format "Invalid test type ~s." type))])
    (printf " Ok.\n")))

(define (test-all)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "Passed all ~s tests.\n" i)
        (let ([x (car ls)] [ls (cdr ls)])
          (let* ([test-name (car x)]
                 [tests (cdr x)]
                 [n (length tests)])
            (printf "Performing ~a tests ...\n" test-name)
            (let g ([i i] [tests tests])
              (cond
                [(null? tests) (f i ls)]
                [else
                 (test-one i (car tests))
                 (g (add1 i) (cdr tests))])))))))

(define (parse-test-file args)
  (do ([args args (cdr args)] [test-file ""])
    ((or (not (equal? "" test-file)) (null? args))
     test-file)
    (if (and (equal? (car args) "--test")
             (not (null? (cdr args)))
             (not (equal? (cadr args) "")))
        (set! test-file (format "~a.scm" (cadr args))))))

(let* ([test-file (parse-test-file (cdr (command-line)))]
       [test-dir (format "~a/~a" (current-directory) "tests")]
       [files (directory-list test-dir)]
       [load-file (lambda (f) (load (format "~a/~a" test-dir f)))])
  (if (not (equal? test-file ""))
      (begin
        (printf "Running single test file '~a'...\n" test-file)
        (load-file test-file)
        (test-all))
      (begin
        (printf "Running all test files...\n")
        (map load-file files)
        (test-all))))
