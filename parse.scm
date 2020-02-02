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

(define (parse-input prevs nexts . stop-codons)
  (cond
    [(null? nexts)
     (cons prevs nexts)]
    [(member (caar nexts) stop-codons)
     (cons prevs nexts)]
    [else
      (match (caar nexts)
        ;; Prefix operators
        ['op-quote
         (recurse-prefix (o string->symbol cdr))]
        ['op-unlambda
         (recurse-infix (lambda (obj)
                          (cons 'unlambda obj)))]
        ['op-let
         (recurse-prefix* parse-let)]
        ['op-lambda
         (recurse-prefix* parse-lambda)]
        ;; Infix operators
        ['op-assign
         (unless (eq? 'name (caar prevs))
           (error 'parse-assign "Expected left hand of assignment to be a name"
                  (string-join (cdar prevs) "←" (cdar nexts))))
         (%assign parse-input prevs nexts)]
        ['op-cons
         (parse-input (cons (cons 'cons (cons (car prevs) (car nexts)))
                            (cdr nexts)))]
        )]))

;; ∀ binary: binary ∈ [cons, then, else, assign]
;; The list above is also their precedence table.
(define (parse-binary prevs nexts (recurse? #t))
  ((compose parse-assigns parse-elses parse-thens, parse-conses)
   prevs nexts recurse?))

;; ∀ unary: unary ∈ [comment, list, backslash, quote, unlambda, lambda]
;; Pool of resulting tokens after this transformation is
;; [block-{open, close}, string, paren-{open, close}, comma,
;;  op-assign, op-cons, op-then, op-else,
;;  symbol, unlambda, let, lambda] ; these four are not op-*
(define (parse-unaries prevs nexts (recurse? #t))
  (define (recurse-prefix f)
    (parse-unaries (cons (f (cdar nexts)) prevs)
                   (cdr nexts)))
  (define (recurse-prefix* f)
    (let* ([res (f (cdr nexts))]
           [obj (car res)]
           [new-nexts (cdr res)])
      (parse-unaries (cons obj prevs) new-nexts)))
  (cond
    [(null? nexts)
     (cons prevs nexts)]
    [else
      (match (caar nexts)
        ;; Comments are ignored.
        ['comment
         (parse-unaries prevs (cdr nexts) recurse?)]
        ;; Quotes prepend names.
        ['op-quote
         (unless (eq? 'name (caadr nexts))
           (error 'parse-unaries "Cannot quote non-name token" nexts))
         (recurse-prefix (o string->symbol cdr))]
        ;; Backslashes are used for escaping newlines.
        ['backslash
         (unless (eq? 'newline (caadr nexts))
           (error 'parse-unaries "Use backslash only for escaping newlines"
                  (take nexts 10)))
         (parse-unaries prevs (cddr nexts) recurse?)]
        ['op-unlambda
         (recurse-prefix* %unlambda)]
        ['op-let
         (recurse-prefix* %let)]
        ['op-lambda
         (recurse-prefix* %lambda)]
        )]))

(define (%unlambda nexts)
  (unless (or ('name (caadr nexts))
              ('op-lambda (caar nexts)))
    (error 'parse-unlambda "Unlambda only lambdas or names"
           (take nexts 10)))
  (let* [[new-objs+new-nexts (parse-unaries '() nexts #f)]
         [new-obj (cons 'unlambda (caar new-objs+new-nexts))]
         [new-nexts (cdr new-objs+new-nexts)]]
    (cons new-obj new-nexts)))

(define (%list nexts)
  (let loop [[rest nexts] [acc '()]]
    (cond
      [(eq? 'list-close (caar rest))
       (cons (cdr rest) (reverse acc))]
      [(eq? 'comma (caar rest))
       (loop (cdr rest) acc)]
      [else
        (let* [[res (parse-input '() rest '(comma list-close))]
               [obj (car res)]
               [new-rest (cdr res)]]
          (loop new-rest (cons obj acc)))])))


;; Either of the forms:
;; λ { ... }
;; λ arg, arg, arg ... { ... }
(define (%lambda nexts)
  (let loop [[rest nexts] [args '()] [gathering? #t]]
    (cond
      [(and gathering? (τ-next-type=? rest 'block-open))
       (loop rest args #f)]
      [(τ-next-type=? rest 'comma)
       (loop rest args gathering?)]
      [(τ-next-type=? rest 'block-open)
       (let* [[res (parse-block (cdr nexts))]
              [obj (car res)]
              [new-rest (cdr res)]]
         (unless (%lambda/args-ok? args)
           (error '%lambda "Only last argument can be a tail argument"
                  args))
         (cons new-rest
               (cons 'lambda
                     `((args ,(reverse args))
                       (body ,obj)))))]
      [(τ-next-type=? rest 'name)
       (let* [[res (%lambda/arg rest)]
              [arg (car res)]
              [new-rest (cdr res)]]
         (loop new-rest (cons arg args) gathering?))]
      [else
        (error '%lambda "Unexpected token at lambda definition"
               (take nexts 10))])))

;; Only last arg can be a tail.
(define (%lambda/args-ok? args)
  (define (tail? arg) (alist-ref 'tail? arg))
  (or (= (count tail? args) 0)
      (and (= (count tail? args) 1)
           (tail? (last args)))))

;; Either of the forms:
;; ? name ,
;; ? name {
;; name ,
;; name {
(define (%lambda/arg nexts)
  (let loop [[rest nexts]
             [tail? #f]
             [name '()]
             [pos 1]]
    (cond
      [(and (> pos 1)
            (or (τ-next-type=? rest 'comma)
                (τ-next-type=? rest 'block-open)))
       (cons `((name . ,name)
               (tail? . ,tail?))
             rest)]
      [(not (τ-next-type=? rest 'name))
       (error '%lambda/arg
              "Unexpected token in argument list"
              (take nexts 10))]
      [(and (= pos 1)
            (τ-next-val=? rest "?"))
       (loop (cdr rest) #t '() 2)]
      [(and (= pos 2) tail?)
       (loop (cdr rest) tail? (cdar nexts) 3)]
      [(= pos 1)
       (loop (cdr rest) tail? (cdar nexts) 2)]
      [else
        (error '%lambda/arg
               "Unexpected token in argument list"
               (take nexts 10))])))

(define (%let nexts)
  (define name
    (if (eq? 'name (caar nexts))
        (cdar nexts)
        #f))
  (set! nexts (if name (cdr nexts) nexts))
  (let* [[list-res (parse-list nexts)]
         [args (car list-res)]
         [list-rest (cdr list-res)]
         [block-res (parse-block list-rest)]
         [block (car block-res)]
         [block-rest]]
    (cons* block-rest
           'let
           `((name ,name)
             (args ,args)
             (block ,block)))))


