(import utf8
        matchable
        regex
        (clojurian syntax)
        (chicken string)
        (srfi 1)
        (srfi 13)
        (srfi 69))

;; Here are some helper functions.
(define (ascii-letter? c)
  (->> c ->string (string-match "[a-zA-Z]")))

;; Pecks return Cons of contents of the token and the rest of characters.
;; For single char, contents would be nothing and rest would be cdr of input.
(define (peck-none σ) (cons "" (substring σ 1)))

;; Comments' contents are kept for possible addition of metacomments.
(define (peck-comment σ)
  (cons
    (-> σ (substring 1 (string-index σ #\newline)) string-trim)
    (substring σ (-> σ (string-index #\newline) (+ 1)))))

;; Character literals start with ` (backtick).
;; Additional escape names:
;; `\b Backspace
;; `\f Formfeed
;; `\n New line
;; `\r Carriage return
;; `\t Horizontal tabulator
;; `\v Vertical tabulator
;; `\s Space
;; `\\ Backslash
;; New ones are subject no change.
(define (peck-character σ)
  (define-values (char rest)
    (if (char=? #\\ (string-ref σ 1))
        (values (substring σ 1 3) (substring 3))
        (values (substring σ 1 2) (substring 2))))
  (cond
    [(member char (list "\b" "\f" "\n" "\r" "\t" "\v" "\s" "\\"))
     (cons char rest)]
    [(string-prefix? "\\" char)
     (error 'peck-character "No such escape character" char)
     #f]
    [(string=? char " ")
     (error 'peck-character "Space character literal is \\s")
     #f]
    [else
      (cons char rest)]))

;; Based on lex-double-string from Toratau
(define (peck-string σ)
  (let next-char ((id 1))
    (cond
      [(char=? (string-ref σ id) #\")
       (cons (substring σ 1 (+ 1 id))
             (substring σ (+ 2 id)))]
      [(string-prefix? "\\\"" (substring σ id))
       (next-char (+ 2 id))]
      [else
        (next-char (+ 1 id))])))

;; Numbers are like 210 or 210.321.
(define (peck-number σ)
  (let next-char ((id 0) (point-encountered? #f))
    (case (string-ref σ id)
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (next-char (+ 1 id) point-encountered?)]
      [(#\.)
       (if point-encountered?
           (cons (substring σ 0 id) (substring σ id))
           (next-char (+ 1 id) #t))]
      [else
        (cons (substring σ 0 id) (substring σ id))])))

;; TODO: symbol pecking


(define token-table
  `((#\{       block-open    ,peck-none)
    (#\}       block-close   ,peck-none)
    (#\%       comment       ,peck-comment)
    (#\[       list-open     ,peck-none)
    (#\]       list-close    ,peck-none)
    (#\newline newline       ,peck-none)
    (#\`       character     ,peck-character)
    (#\"       string        ,peck-string)
    (#\(       paren-open    ,peck-none)
    (#\)       paren-close   ,peck-none)
    (#\,       comma         ,peck-none)
    (#\←       op-assign     ,peck-none)
    (#\'       op-quote      ,peck-none)
    (#\:       op-cons       ,peck-none)
    (#\&       op-unlambda   ,peck-none)
    (#\¤       op-let        ,peck-none)
    (#\λ       op-lambda     ,peck-none)
    (#\⇒       op-then       ,peck-none)
    (#\;       op-else       ,peck-none)))

