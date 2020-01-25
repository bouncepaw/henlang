(import matchable
        regex
        utf8
        (clojurian syntax)
        (chicken pretty-print)
        (chicken string)
        (chicken io)
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
    [(member char (list "\\b" "\\f" "\\n" "\\r" "\\t" "\\v" "\\s" "\\\\"))
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

;; TODO: fix it so it works
(define (peck-name σ)
  (print "pecking" σ "\n")
  ;; A name follows one of the schemes:
  ;; | greek_letter* 1char_name suffix?
  ;; | greek_letter* normal_char* suffix?
  ;; | greek_letter* normal_char* rarrow greek_letter* normal_char* suffix?
  ;;
  ;; Let's walk along and decide what's in the name and what's not.
  ;; Some helpers:
  (define ((member-λ lst) el) (member el lst))
  (define (count-while-σ p start-id)
    (let loop ((id start-id))
      (if (p (string-ref σ id))
          (loop (+ 1 id))
          id)))
  ;; Store length of the name to be taken from σ in len.
  (define len 0)
  ;; Any name may start with any number of greek letters (except of λ).
  (define greek?
    (-> "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω"
        string->list
        member-λ))
  (set! len (count-while-σ greek? len))
  ;; Names can't end with a greek letter now. If there is a delimeter next,
  ;; panic.
  (define delimeter?
    (-> car
        (map token-table)
        (append (string->list " \t\r"))
        member-λ))
  (when (delimeter? (string-ref σ len))
    (error 'peck-name "Name cannot end with a greek letter"
           (substring σ 0 (+ 1 len))))
  ;; Now, we have two ways: one one-char-name or any number of normal chars.
  ;; The normal char seq may be followed by a rarrow and another cluster of
  ;; greek and normal chars. Either of the ways may end with a suffix.
  (define one-char-name?
    (-> "⊤⊥∅+-/*^√=<>≤≥_∈∉∋∌∧∨∪∖∩∀∃"
        string->list
        member-λ))
  (define suffix?
    (-> "?!"
        string->list
        member-λ))
  (define (rarrow? c) (char=? c #\→))
  (define (normal-char? c)
    (not (or (greek? c)
             (delimeter? c)
             (suffix? c)
             (rarrow? c))))

  (if (-> σ (string-ref (+ 1 len)) one-char-name?)
      (set! len (+ 1 len))
      (begin
        (set! len (count-while-σ normal-char? len))
        ;; There may be a rarrow!
        (when (and #;(> (string-length σ) len)
                   (-> σ (string-ref len) rarrow?))
          (print "aaa")
          (set! len (+ 2 len))
          (set! len (count-while-σ greek? len))
          (set! len (count-while-σ normal-char? len)))))
  ;; Whatever happened, there may be a suffix:
  (when (-> σ (string-ref (+ 1 len)) suffix?)
    (set! len (+ 1 len)))
  (cons (substring σ 0 (+ 1 len)) (substring σ (+ 1 len)))
  )


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

(define (σ→tokens inputσ)
  (define (token-commencer? c)
    (define special-chars (map car token-table))
    (member c special-chars))
  (let loop [(σ inputσ) (acc '())]
    (cond
      [(string-null? σ)
       (reverse acc)]
      [(token-commencer? (string-ref σ 0))
       (let* [(alist-entry (alist-ref (string-ref σ 0) token-table))
              (token-type (car alist-entry))
              (token-peck-λ (cadr alist-entry))
              (res (token-peck-λ σ))
              (token-content (car res))
              (rest (cdr res))]
         (loop rest
               (cons (cons token-type token-content) acc)))]
      [(char-whitespace? (string-ref σ 0))
       (loop (substring σ 1) acc)]
      [(char-numeric? (string-ref σ 0))
       (let* [(res (peck-number σ))
              (number-itself (car res))
              (rest (cdr res))]
         (loop rest
               (cons (cons 'number number-itself) acc)))]
      [else
        (let* [(res (peck-name σ))
               (name-itself (car res))
               (rest (cdr res))]
          (loop rest
                (cons (cons 'name name-itself) acc)))]
      )))

;; Add extra newline at end of source string. It eases lexing a lot!
(define (add-safety-belt σ)
  (string-append/shared σ "\n"))

(define text (read-string #f (open-input-file "example.hen")))
(pretty-print (σ→tokens text))

