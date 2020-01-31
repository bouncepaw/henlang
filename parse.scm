(import matchable
        (clojurian syntax)
        (chicken string)
        (srfi 1)
        (srfi 13))

;; this one's complicated

(define (parse-list nexts)
  (let loop [[rest nexts] [acc '()]]
    (cond
      [(eq? 'list-close (caar rest))
       (cons (cdr rest) (reverse acc))]
      [(eq? 'comma (caar rest))
       (loop (cdr nexts) acc)]
      [else
        (let* [[res (parse-input nexts)]
               [obj (car res)]
               [new-rest (cdr res)]]
          (loop new-rest (cons obj acc)))])))

(define (parse-lambda nexts)
  (let loop [[rest nexts] [args '()]]
    (cond
      [(eq? 'block-open (caar rest))
       (let* [[res (parse-block nexts)]
              [obj (car res)]
              [new-rest (cdr res)]]
         (cons new-rest
               (cons 'lambda
                     `((args ,(reverse args))
                       (body ,obj)))))]
      [(eq? 'name (caar rest))
       (loop (cdr rest) (cons (cdar rest) args))]
      [(eq? 'comma (caar rest))
       (loop (cdr rest) args)]
      [else
        (error 'parse-lambda "Unexpected token at lambda definition"
               (car rest))])))

(define (parse-let nexts)
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
    (cons block-rest
          (cons 'let
                `((name ,name)
                  (args ,args)
                  (block ,block))))))

(define (parse-input prevs nexts . stop-codons)
  (define (recurse-infix f)
    (parse-input (cons (f (cdar nexts)) prevs)
                 (cdr nexts)))
  (cond
    [(null? nexts) prevs]
    [(member (caar nexts) stop-codons)
     (cons prevs (cdr nexts))]
    [else
      (match (caar nexts)
        ;; Prefix operators
        ['op-quote
         (recurse-infix (o string->symbol cdr))
        ['op-unlambda
         (recurse-infix (lambda (obj)
                          (cons 'unlambda obj)))
        ['op-let
         (let* ([res (parse-let nexts)]
                [let-obj (car res)]
                [new-nexts (cdr res)])
           (parse-input (cons let-obj prevs) new-nexts))]
        ['op-lambda
         (let* [[res (parse-lambda nexts)]
                [λ-obj (car res)]
                [new-nexts (cdr res)]]
           (parse-input (cons λ-obj prevs) new-nexts))]
        ;; Infix operators
        ['op-assign
         (unless (eq? 'name (caar prevs))
           (error 'parse-assign "Expected left hand of assignment to be a name"
                  (string-join (cdar prevs) "←" (cdar nexts))))
         (parse-input (cons (list 'assign (car prevs) (car nexts))
                            (cdr prevs))
                      (cdr nexts))]
        ['op-cons
         (parse-input (cons (cons 'cons (cons (car prevs) (car nexts)))
                            (cdr nexts)))]
        )]))
