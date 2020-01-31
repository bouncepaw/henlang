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

(define (group-blocks tokens)
  (let loop [(rest tokens) (acc '()) (blocks '())]
    (if (null? rest)
      (reverse acc)
      (match (caar rest)
        ['block-open
         (loop (cdr rest) acc (cons '(block) blocks))]
        ['block-close
         (loop (cdr rest)
               (cons (-> blocks car reverse) acc)
               (cdr blocks))]
        [_
          (if (null? blocks)
            (loop (cdr rest)
                  (cons (car rest) acc)
                  blocks)
            (loop (cdr rest)
                  acc
                  (cons (cons (car rest) (car blocks))
                        (cdr blocks))))]))
    ))

