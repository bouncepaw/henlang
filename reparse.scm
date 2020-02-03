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

(define (parse-extra-newlines prevs nexts)
  (define (cons-maybe pair-new pair-old)
    (if (null? pair-new)
        pair-old
        (cons pair-new pair-old)))
  (cond
    [(null? nexts)
     (cons (reverse prevs) nexts)]
    [(and (or (eq? 'newline (caar nexts))
              (eq? 'newline (caadr nexts)))
          (> (length nexts) 1))
     (let* [[should-move-by-1? #f]]
       ;; TODO: magically not works
       ;; some errors found.
       ;; commit: stripping extra newlines
       (parse-extra-newlines
         (cons-maybe
           (match (take nexts 2)
             ;; Ignore newlines around {}
             [(('block-open . "") ('newline . ""))
              (cons 'block-open "")]
             [(('block-close . "") ('newline . ""))
              (cons 'block-close "")]
             [(('comma . "") ('newline . ""))
              (cons 'comma "")]
             [(('paren-open . "") ('newline . ""))
              (cons 'paren-open "")]
             [(('newline . "") ('block-close . ""))
              (set! should-move-by-1? #t)
              '()]
             [(('newline . "") ('block-open . ""))
              (set! should-move-by-1? #t)
              '()]
             [(('newline . "") ('comma . ""))
              (set! should-move-by-1? #t)
              '()]
             [(('newline . "") ('paren-close . ""))
              (set! should-move-by-1? #t)
              '()]
             [else
               (set! should-move-by-1? #t)
               (car nexts)]
             )
           prevs)
         ((if should-move-by-1? cdr cddr) nexts)))]
    [else
      (parse-extra-newlines (cons (car nexts) prevs) (cdr nexts))]
    ))

;; There are two types of `heads´: lambda and let ones.
;; Let's look at their structures:
;; head----+ +--tail
;; |       | |     |
;; λ a, b, c { ... }
;; λ         { ... }
;;
;; head-+ +neck-+ +--tail
;; |    | |     | |     |
;; ¤ loop [ ... ] { ... }
;; ¤      [ ... ] { ... }
;; ¤ loop         { ... }
;; ¤              { ... }
;;
;; This function creates head tokens.
(define (parse-heads prevs nexts)
  (cond
    [(null? nexts)
     (cons (reverse prevs) nexts)]
    ;; Let head parsing.
    [(and (τ-next-type=?      nexts  'op-let)
          (τ-next-type=? (cdr nexts) 'name))
     (parse-heads (cons (cons 'let-head (cdadr nexts))
                        prevs)
                  (cddr nexts))]
    [(and (τ-next-type=? nexts 'op-let)
          (not (or (τ-next-type=? (cdr nexts) 'list-open)
                   (τ-next-type=? (cdr nexts) 'block-open))))
     (error 'parse-heads "Unexpected token in let" (take nexts 10))]
    [(τ-next-type=? nexts 'op-let)
     (parse-heads (cons (cons 'let-head '())
                        prevs)
                  (cdr nexts))]
    [else
      (parse-heads (cons (car nexts) prevs) (cdr nexts))]
    ))


