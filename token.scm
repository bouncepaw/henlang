(import matchable
        (clojurian syntax)
        (chicken string)
        (srfi 1)
        (srfi 13)
        (srfi 69))

;; Here are some helper functions.
(define (ascii-letter? c)
  (member (->string c)
          (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))

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

;; Character literals start with ` (backtick).
;; Supported names for characters are taken from here:
;; https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Characters.html
;; `alarm                 ; U+0007
;; `backspace             ; U+0008
;; `delete                ; U+007F
;; `escape                ; U+001B
;; `newline               ; the linefeed character, U+000A
;; `null                  ; the null character, U+0000
;; `return                ; the return character, U+000D
;; `space                 ; the preferred way to write a space, U+0020
;; `tab                   ; the tab character, U+0009
;; As you can see, most of those characters are useless in modern day
;; programming but perhaps they will come in handy one day.
;;
;; NB. Unlike Scheme, space character can only be expressed by using `space.
;; Using ` (a real space) is considered harmful.
(define (peck-character chars)
  (define-values (char rest)
    (if (-> chars cadr ascii-letter?)
        (let next-char ((len 1) (rest (cdr chars)))
          (if (ascii-letter? (car rest))
              (next-char (+ 1 len) (cdr rest))
              (values (-> chars (take len) (drop 1) list->string)
                      rest)))
        (values (-> chars cadr ->string) (cddr chars))))
  (cond
    ((or (eq? char "") (eq? char " "))
     (error 'peck-character "Empty character is illegal")
     #f)
    ((member char (list "alarm" "backspace" "delete" "escape" "newline"
                        "null" "return" "space" "tab"))
     (cons char rest))
    ((and (-> char string-length (eq? 1) not)
          (ascii-letter? char))
     (error 'peck-character "Unknown character name found" char)
     #f)
    (else (cons char rest))))

;; Based on lex-double-string from Toratau
(define (peck-string chars)
  (let next-char ((len 1) (rest (cdr chars)))
    (cond
      ((equal? (car rest)  #\" )
       (cons (take (drop chars 1) len)
             (drop chars (+ 2 len))))
      ((and (equal? (car rest) #\\)
            (equal? (cadr rest) #\"))
       (next-char (+ 2 len) (cddr rest)))
      (else (next-char (+ 1 len) (cdr rest))))))

;; Numbers are like 210 or 210.321. Tested!
(define (peck-number chars)
  (let next-char ((len 0) (rest chars) (point-encountered? #f))
    (case (car rest)
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (next-char (+ 1 len) (cdr rest) point-encountered?))
      ((#\.)
       (if point-encountered?
           (cons (list->string (take chars len)) (drop chars len))
           (next-char (+ 1 len) (cdr rest) #t)))
      (else
        (cons (list->string (take chars len)) (drop chars len))))))

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

