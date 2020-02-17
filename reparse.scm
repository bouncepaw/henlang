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
       (parse-extra-newlines
         (cons-maybe
           (match (take nexts 2)
             ;; Ignore newline after →
             [(('op-assign . "") ('newline . ""))
              (cons 'op-assign "")]
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

;; There are three types of `heads´: assign, lambda and let ones.
;; Let's look at their structures:
;; head  tail
;; |  |  |
;; a  ←  b
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
    ;; Assign head parsing.
    [(τ-next-type=? nexts 'op-assign)
     (let* [[head `(assign-head ,(string->symbol (cdar prevs)))]]
       (parse-heads (cons head (cdr prevs)) (cdr nexts)))]
    ;; Lambda head parsing.
    [(τ-next-type=? nexts 'op-lambda)
     (let* [[res (%lambda/head nexts)]
            [new-nexts (car res)]
            [head (cdr res)]]
       (parse-heads (cons head prevs) new-nexts))]
    ;; Let head parsing.
    [(and (τ-next-type=?      nexts  'op-let)
          (τ-next-type=? (cdr nexts) 'name))
     (parse-heads (cons (cons 'let-head (string->symbol (cdadr nexts)))
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

(define (%lambda/head nexts)
  (let loop [[rest (cdr nexts)] [args '()]]
    (cond
      [(τ-next-type=? rest 'comma)
       (loop (cdr rest) args)]
      [(τ-next-type=? rest 'block-open)
       (unless (%lambda/args-ok? args)
         (error '%lambda/head "Only last argument can be a tail argument"
                args))
       (cons rest
             (cons 'lambda-head
                   (%lambda/preprocess-args args)))]
      [(τ-next-type=? rest 'name)
       (let* [[res (%lambda/arg rest)]
              [arg (car res)]
              [new-rest (cdr res)]]
         (loop new-rest (cons arg args)))]
      [else
        (error '%lambda/head "Unexpected token at lambda definition"
               (take nexts 10) (take rest 10) args)])))

;; Only last arg can be a tail. List of args come here reversed (with first
;; element being the may-be tail).
(define (%lambda/args-ok? args)
  ;; tailiness is stored in car as a boolean.
  (define tail? car)
  (or (= (count tail? args) 0)
      (and (= (count tail? args) 1)
           (tail? (car args)))))

;; Either of the forms:
;; ? name ,
;; ? name {
;; name ,
;; name {
(define (%lambda/arg nexts)
  (define ((τ=?-λ τ) x) (eq? τ (car x)))
  (define block-open? (τ=?-λ 'block-open))
  (define comma?      (τ=?-λ 'comma))
  (define name?       (τ=?-λ 'name))
  (define (question? x) (eq? "?" (cdr x)))
  (match (take nexts 3)
    [((? name? question? ques)
      (? name?           name)
      (or (? block-open? delim)
          (? comma?      delim)))
     (cons (cons #t (cdr name)) (cddr nexts))]
    ;; when not tail arg
    [((? name?           ('name . name))
      (or (? block-open? delim)
          (? comma?      delim))
      _)
     (cons (cons #f name) (cdr nexts))]
    [_
      (print "end of world")]
    ))

(define (proper->dotted lst)
  (cond
    [(= 1 (length lst))
     (car lst)]
    [(eq? '() (cddr lst))
     (cons (car lst) (cadr lst))]
    [else
      (cons (car lst) (proper->dotted (cdr lst)))]))

(define (%lambda/preprocess-args args)
  (let* [[extract-arg (o string->symbol cdr)]
         [tailed?     (caar args)]
         [dot-reverse (o proper->dotted reverse)]]
    (cond
      [tailed? (dot-reverse (map extract-arg args))]
      [else (reverse (map extract-arg args))])))

(define (τ-matching? τ1 τ2)
  (match (list τ1 τ2)
         [('block-open 'block-close) #t]
         [('list-open 'list-close) #t]
         [('paren-open 'paren-close) #t]
         [_ #f]))

(define push xcons)
(define (push-push layers v)
  (cons (cons v (car layers))
        (cdr layers)))
(define (layer-pack layer)
  (cons* 'nest #f (reverse layer)))

;; layers is FIFO.
;; layer is pair: car is type ('{block list paren}-open), cdr is contents
;; Once layer is finished, it is added to the previous one. If there is no,
;; algo is finished.
(define (parse-nest nexts layers)
  (if (null? nexts)
      (reverse (car layers))
      (case (caar nexts)
        [(block-open paren-open list-open)
         (parse-nest (cdr nexts)
                     (-> layers (push-push (car nexts)) (push '())))]
        [(block-close paren-close list-close)
         (unless (τ-matching? (caaadr layers) (caar nexts))
           (error 'parse-nest "Mismatched pairs" (caaadr layers) (caar nexts)))
         (parse-nest (cdr nexts)
                     (-> layers cdr
                         (push-push (layer-pack (car layers)))
                         (push-push (car nexts))))]
        [else
          (parse-nest (cdr nexts)
                      (push-push layers (car nexts)))])))

;; TODO: implement
;; this thing composes all things composable
(define (parse-composition prevs nexts)
  nexts)
