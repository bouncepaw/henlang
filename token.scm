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
  (define special-chars (map car token-table))
  (define one-char-names
    (string->list "⊤⊥∅+-/*^√=<>≤≥_∈∉∋∌∧∨∪∖∩∀∃"))
  ;; No λ!
  (define greek-letters
    (string->list "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω"))
  (define suffices (list "?" "!"))
  (define (normal-char? γ)
    (and (not (member γ special-chars))
         (not (member γ one-char-names))
         (not (member γ greek-letters))
         (not (member γ suffices))
         (not (char=? γ #\→))))
  (define (count-while-σ s p _id)
    (let loop ((id _id))
      (print "iteration")
      (if (p (string-ref s id))
          (loop (+ 1 id))
          id)))
  (define ((member-λ lst) el) (member el lst))
  ;; A name follows one of the schemes:
  ;; | greek_letter* normal_char* suffix?
  ;; | greek_letter* normal_char* rarrow greek_letter* normal_char* suffix?
  ;; | greek_letter* 1char_name suffix?
  ;;
  ;; Let's walk along and decide what's in the name and what's not.
  ;; First of all, any name may contain any number of greek letters.
  (define len (count-while-σ σ (member-λ greek-letters) 0))
  (print len)
  ;; Then, we fork to two rails.
  (if (member (string-ref σ len) one-char-names)
      (begin
        (set! len (+ 1 len))
        ;; Here may be a suffix
        (if (member (string-ref σ (+ 1 len)) suffices)
            (set! len (+ 1 len)))
        ;; Anyway, return it the name
        (cons (substring σ 0 len) (substring σ len)))
      (begin
        ;; Any number of normal chars here.
        (set! len (count-while-σ σ normal-char? len))
        ;; Here we fork.
        (if (char=? #\→ (string-ref σ len))
            (begin
              ;; Greek letters may be here
              (set! len (count-while-σ σ (member-λ greek-letters) len))
              ;; Normal chars may be here
              (set! len (count-while-σ σ normal-char? len))
              ;; Here may be a suffix
              (if (member (string-ref σ (+ 1 len)) suffices)
                  (set! len (+ 1 len)))
              (cons (substring σ 0 len) (substring σ len)))
            (begin
              ;; Here may be a suffix. If not, one char back.
              (if (-> σ (string-ref len) (member suffices) not)
                  (set! len (- len 1)))
              (cons (substring σ 0 len) (substring σ len)))))))


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

(define text (read-string #f (open-input-file "example.hen")))
(pretty-print (σ→tokens text))

