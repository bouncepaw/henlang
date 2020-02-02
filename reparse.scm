;; New attempt at parsing. Old file parse.scm to remain.
(import matchable
        (clojurian syntax)
        (chicken string)
        (srfi 1)
        (srfi 13))

;; this one's complicated

(define (τ-literal? τ)
  (-> τ caar (member '(string character number unlambda quoted))))

(define (τ-next-type=? nexts τ)
  (eq? τ (caar nexts)))

(define (τ-next-val=? nexts τ)
  (eq? τ (cdar nexts)))

(define (with-new-car kons new-car)
  (cons new-car (cdr kons)))

(define (%assign parse-λ prevs nexts)
  (parse-input (with-new-car prevs (list 'assign (car prevs) (car nexts)))
               (cdr nexts)))

(define (%cons parse-λ prevs nexts)
  (parse-input (with-new-car prevs (cons* 'cons (car prevs) (car nexts)))
               (cdr nexts)))

(define (parse-input prevs nexts . stop-codons) '() )

;; ∀ binary: binary ∈ [cons, then, else, assign]
;; The list above is also their precedence table.
(define (parse-binary prevs nexts ) '() )

;; ∀ unary: unary ∈ [comment, list, backslash, quote, unlambda, lambda]
;; Pool of resulting tokens after this transformation is
;; [block-{open, close}, string, paren-{open, close}, comma,
;;  op-assign, op-cons, op-then, op-else,
;;  symbol, unlambda, let, lambda] ; these four are not op-*
(define (parse-unaries prevs nexts)
  (define (recurse-prefix f)
    (parse-unaries (cons (f (cadr nexts)) prevs)
                   (cddr nexts)))
  (define (recurse-prefix* f)
    (let* ([res (f (cdr nexts))]
           [obj (car res)]
           [new-nexts (cdr res)])
      (parse-unaries (cons obj prevs) new-nexts)))
  (cond
    [(null? nexts)
     (cons (reverse prevs) nexts)]
    [else
      (match (caar nexts)
        ;; Comments are ignored.
        ['comment
         (parse-unaries prevs (cdr nexts))]
        ;; Quotes prepend names.
        ['op-quote
         (unless (eq? 'name (caadr nexts))
           (error 'parse-unaries "Cannot quote non-name token" nexts))
         (recurse-prefix (lambda (symbol) (cons 'symbol (string->symbol (cdr symbol)))))]
        ;; Backslashes are used for escaping newlines.
        ['backslash
         (unless (eq? 'newline (caadr nexts))
           (error 'parse-unaries "Use backslash only for escaping newlines"
                  (take nexts 10)))
         (parse-unaries prevs (cddr nexts))]
        [else
          (parse-unaries (cons (car nexts) prevs) (cdr nexts))]
        )]))


