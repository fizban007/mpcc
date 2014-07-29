#lang racket

(require (for-syntax syntax/parse))

(define-syntax (apply-quote stx)
  (define (cons-quote args)
    (let ([f (cadr args)]
          [xs (cddr args)])
      (cons f (map (lambda (a) (list 'quasiquote a)) xs))))
  (let ([xs (syntax->datum stx)])
    (datum->syntax stx (cons-quote xs))))

(define-for-syntax expr-rules '())

(define-syntax (add-rule stx)
  (syntax-case stx ()
    [(_ pattern rule) #'(begin-for-syntax (set! expr-rules (append expr-rules (list (list pattern rule)))))]))

;; (define-for-syntax expr-rules '(((list 'new type) (pretty-print (list "new" type)))))

(define-syntax (match-rules stx)
  (let ([stmts (cadr (syntax->datum stx))])
    (define (append-match rules)
      (cond [(eq? rules '()) '()]
            [else
             (append (list `[,(caar rules) ,(cadar rules)]) (append-match (cdr rules)))]))
    ;; (print (append (list 'match) (append-match expr-rules)))
    (datum->syntax stx (append (list 'match stmts)
                               (append-match expr-rules)))))

;; (define-syntax aq apply-quote)

(define-syntax (make-quote stx)
                   (let ([xs (syntax->datum stx)])
                     (datum->syntax stx `(quote ,(cdr xs)))))
;; (define-for-syntax op-assign "=")
(define op-assign "=")

;; Define operators
(define arith-op '("+" "-" "*" "/" "%"))
(define inc-op '("++" "--"))
(define logical-op '("!" "&&" "||"))

(define binary-op '("+" "-" "*" "/" "%" "&&" "||" "&" "|" ">>" "<<" "^"))
(define unary-op '("!" "~" "-"))
(define compound-assign-op '("+=" "-=" "*=" "/=" "%=" ">>=" "<<=" "&=" "^=" "|="))
(define relation-op '("<" ">" "<=" ">=" "==" "!="))

(define (check-op op op-group)
  (cond [(string? op) (cond
                       [(eq? (member op op-group) #f) #f]
                       [else #t])]
        [(symbol? op) (cond
                       [(eq? (member (symbol->string op) op-group) #f) #f]
                       [else #t])]))

(define (binary-op? op)
  (check-op op binary-op))

(define (unary-op? op)
  (check-op op unary-op))

(define (relation-op? op)
  (check-op op relation-op))

(define (compound-assign-op? op)
  (check-op op compound-assign-op))
;; End of operators definition


(define tab-size 4)

(define (indent-line str n)
  (string-append (make-string (* n tab-size) #\space) str))

(define (to-string terms)
  (cond
   [(list? terms) (map (lambda (term) 
                         (cond [(string? term) term]
                               [(number? term) (number->string term)]
                               [else (symbol->string term)]
                               )) (flatten terms))]
   [(symbol? terms) (symbol->string terms)]
   [(string? terms) terms]))

(define (concat-str mylist)
  (apply string-append mylist))

(define (add-comma mylist)
  (concat-str (to-string (add-between mylist ", "))))

(define (pretty-print-line statement [breaknum 1])
  (concat-str (append 
               (add-between (to-string statement)
                            " ") `(";" ,(make-string breaknum #\newline)))))

(define (pretty-print statement)
  (concat-str (add-between (to-string statement)
                            " ")))

(define (pretty-args args)
  (add-comma (unparse-args args)))

(define (unparse-args args)
  (match args
    [(list (list t n) ... (list dt dn dv) ...)
     (map concat-str (map
                      (lambda (x) (cond
                              [(eq? (length x) 2) (add-between (to-string x) " ")]
                              [(eq? (length x) 3) (add-between #:before-last " = "
                                                   (to-string x) " ")])) args))]))

(define (make-char char)
  (concat-str `("'" ,(cond [(symbol? char) (symbol->string char)]
                           [else char]) "'")))

(define (paren-unparse-expr expr)
  (cond [(list? expr) (list "(" (unparse-expr expr) ")")]
        [else (unparse-expr expr)]))

(define (unparse-expr expr)
  (match expr
    [(list 'new type) (pretty-print (list "new" type))]
    [(list 'new type expr ...) (pretty-print (list "new" (unparse-expr (flatten (list type expr)))))]
    [(list 'new-array type num) (pretty-print (list "new" type "[" num "]"))]
    [(list 'del name) (pretty-print (list "delete" name))]
    [(list 'del-array name) (pretty-print (list "delete[]" name))]
    [(list 'decl type name) (pretty-print (list type name))]
    [(list 'decl type name val) (pretty-print (list type name op-assign (unparse-expr val)))]
    [(list 'assign name val) (pretty-print (list name op-assign (unparse-expr val)))]
    [(list (? compound-assign-op? op) name val) (pretty-print (list name (to-string op) (unparse-expr val)))]
    [(list (? binary-op? op) op1 op2) (pretty-print (list (paren-unparse-expr op1) (to-string op) (paren-unparse-expr op2)))]
    [(list (? unary-op? op) op1) (pretty-print (list (to-string op) (paren-unparse-expr op1)))]
    [(list (? relation-op? op) op1 op2) (pretty-print (list (paren-unparse-expr op1) (to-string op) (paren-unparse-expr op2)))]
    [(list 'ret val) (pretty-print (list "return" (paren-unparse-expr val)))]
    ;; Function application
    [(list a ..2) (pretty-print (list (first expr) "(" (add-comma (map unparse-expr (rest expr))) ")"))]
    ;; [(list 'br) "\n"]
    ;; Single term
    [(var a) (pretty-print (list (cond [(string? a) (concat-str (list "\"" a "\""))]
                                       [else a])))]))

(define (unparse-func func [indent 0])
  (match func
    [(list 'fn name args -> ret) (pretty-print-line (flatten (list ret name "(" (pretty-args args) ")")) 2)]
    [(list 'fn name args) (pretty-print-line (flatten (list "void" name "(" (pretty-args args) ")")) 2)]
    [(list 'fn name args body) 
     (concat-str (list (pretty-print (flatten (list "void" name "(" (pretty-args args) ")"))) " {\n"
                       (unparse-expr-list body (+ indent 1)) "}\n\n"))]
    [(list 'fn name args -> ret body) 
     (concat-str (list (pretty-print (flatten (list ret name "(" (pretty-args args) ")"))) " {\n"
                       (unparse-expr-list body (+ indent 1)) "}\n\n"))]
))



(define (unparse-include stmt [indent 0])
  (let ([headers (rest stmt)])
    (define (to-header file)
      (cond [(string? file) (concat-str (list "\"" file "\""))]
            [(symbol? file) (concat-str (list "<" (symbol->string file) ">"))]))
    (cond [(list? headers)
           (concat-str
            (append (map (lambda (file) (concat-str (list "#include " (to-header file) "\n"))) headers)
             '("\n")))]
          [else (concat-str (list "#include " (to-header headers) "\n\n"))])))

(define (unparse-define stmt [indent 0])
  (let ([symbol (car (rest stmt))]
        [expr (cadr (rest stmt))])
    (concat-str (list "#define " (pretty-print (list (symbol->string symbol) " " (unparse-expr expr))) "\n"))))


(define (unparse-expr-list expr-list [indent 0])
  (match expr-list
    [(list (list a ..1) ..1)
     (let ([kw (caar expr-list)]
           [expr (first expr-list)])
       (let ([result (cond
                      [(eq? kw 'br) "\n"]
                      [(eq? kw 'include) (unparse-include expr)]
                      [(eq? kw 'def) (unparse-define expr)]
                      [(eq? kw 'fn) (unparse-func expr)]
                      [(eq? kw 'comment) (concat-str (list "/*" (unparse-expr-list (rest expr) indent) "*/\n"))]
                      [else (concat-str (list (indent-line (unparse-expr expr) indent) ";\n"))])])
         ;; (concat-str (list (indent-line (unparse-expr (first expr-list)) indent) ";\n"
         (concat-str (list result
                           ;; (concat-str (list (indent-line (unparse-expr (first expr-list)) indent)
                           (unparse-expr-list (rest expr-list) indent)))))]
    ['() ""]
))


(define (begin-c stmt [filename ""])
  (cond
   [(eq? filename "") (printf (unparse-expr-list stmt))]
   [(string? filename)
    (with-output-to-file filename #:exists 'replace
      (lambda () (printf (unparse-expr-list stmt))))]))

(define (main stmts)
  `(fn main ([int argc] [char** argv]) -> int ,stmts))

;; (define-syntax (cout stx)
;;   (datum->syntax stx
;;                  `(list (pretty-print (list "std::cout <<" (add-between (map paren-unparse-expr (quote ,(cdr (syntax->datum stx)))) "<<"))))))
(define cout
  (lambda args
    (list (pretty-print (list "std::cout <<" (add-between (map paren-unparse-expr args) "<<"))))))

    ;; [(list 'cout val ...) (pretty-print (list "std::cout <<" (add-between (map paren-unparse-expr val) "<<")))]
    ;; [(list 'cout-line val ...) (pretty-print (list "std::cout <<" (add-between (map paren-unparse-expr val) "<<") "<< std::endl"))]

(provide (all-defined-out))

