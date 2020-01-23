(import matchable
        (clojurian syntax)
        (chicken string)
        (srfi 1)
        (srfi 13))

;; During lexing, characters are taken as strings. This function transforms
;; them to Scheme's characters.
(define (characterify c)
  (match c
    ["\\b" #\alarm]
    ["\\f" #\x000C] ; form feed
    ["\\n" #\newline]
    ["\\r" #\x000D] ; carriage return
    ["\\t" #\tab]
    ["\\v" #\x0008] ; vertical tabulation
    ["\\s" #\space]
    ["\\\\" #\\]
    [_ (string-ref c 0)]))

