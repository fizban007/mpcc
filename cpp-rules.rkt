#lang racket

(provide add-rule! match-rules)

(define-values-for-syntax (append-rule get-rules)
  (let ([expr-rules '()])
    (values
     (lambda (rule)
       (set! expr-rules (append expr-rules (list rule))))
     (lambda ()
       expr-rules))))

(define-syntax (add-rule! stx)
  (syntax-case stx ()
    [(_ pattern rule) #'(begin-for-syntax (append-rule (list pattern rule)))]))

;; (define-for-syntax expr-rules '(((list 'new type) (pretty-print (list "new" type)))))

(define-syntax (match-rules stx)
  (let ([stmts (cadr (syntax->datum stx))])
    (define (append-match rules)
      (cond [(eq? rules '()) '()]
            [else
             (append (list `[,(caar rules) ,(cadar rules)]) (append-match (cdr rules)))]))
    ;; (print (append (list 'match) (append-match expr-rules)))
    (datum->syntax stx (append (list 'match stmts)
                               (append-match (get-rules))))))
