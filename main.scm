(load "token.scm")
(load "preprocess.scm")
(load "reparse.scm")

;; Add extra newline at end of source string. It eases lexing a lot!
(define (add-safety-belt σ)
  (string-append/shared σ "\n"))

(define text (read-string #f (open-input-file "example.hen")))
(-> text
    add-safety-belt
    σ→tokens
    (as-> n (parse-unaries '() n))
    car
    (as-> n (parse-extra-newlines '() n))
    car
    (as-> n (parse-heads '() n))
    car
    pretty-print)

