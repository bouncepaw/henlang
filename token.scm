(import matchable
        (clojurian syntax)
        (srfi 13)
        (srfi 69))

;; Pecks return Cons of contents of the token and the rest of characters.
;; For single char, contents would be nothing and rest would be cdr of input.
(define (peck-char chars)
  (cons "" (cdr chars)))

;; Comments' contents are kept for possible addition of metacomments.
(define (peck-comment chars)
  (define taken
    (->> chars
         cdr
         (take-while (lambda (c) (not (char=? c #\newline))))
         list->string
         string-trim))
  (cons taken (drop chars (length taken))))

;; TODO: support of multi-char chars such as `newline.
;; For now return one character only. This implementation is OK for some time.
(define (peck-character chars)
  (cons (->string (cadr chars)) (cddr chars)))

;; Based on lex-double-string from Toratau
(define (peck-string chars)
  (let next-char ((len 1) (rest (cdr chars)))
    (cond
      ((equal? (car rest)  #\" )
       (cons (take (drop chars 1) len)
             (drop chars (+ 2 len)))
      ((and (equal? (car rest) #\\)
            (equal? (cadr rest) #\"))
       (next-char (+ 2 len) (cddr rest)))
      (else (next-char (+ 1 len) (cdr rest)))))))

;; TODO: number pecking
;; TODO: symbol pecking


(define token-table
  `((#\{       brace-open    ,peck-char)
    (#\}       brace-close   ,peck-char)
    (#\%       comment       ,peck-comment)
    (#\[       bracket-open  ,peck-char)
    (#\]       bracket-close ,peck-char)
    (#\newline newline       ,peck-char)
    (#\`       character     ,peck-character)
    (#\"       string        ,peck-string)
    (#\(       paren-open    ,peck-char)
    (#\)       paren-close   ,peck-char)
    (#\,       comma         ,peck-char)
    (#\←       op-assign     ,peck-char)
    (#\'       op-quote      ,peck-char)
    (#\:       op-cons       ,peck-char)
    (#\&       op-unlambda   ,peck-char)
    (#\/       op-reduce     ,peck-char)
    (#\#       op-map        ,peck-char)
    (#\¤       op-let        ,peck-char)
    (#\λ       op-lambda     ,peck-char)
    (#\⇒       op-then       ,peck-char)
    (#\;       op-else       ,peck-char)))

