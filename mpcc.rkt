#lang racket

(require (for-syntax syntax/parse))
(require "cpp-rules.rkt")

(define-syntax (apply-quote stx)
  (define (cons-quote args)
    (let ([f (cadr args)]
          [xs (cddr args)])
      (cons f (map (lambda (a) (list 'quasiquote a)) xs))))
  (let ([xs (syntax->datum stx)])
    (datum->syntax stx (cons-quote xs))))

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

;; (define (concat-str mylist)
;;   (apply string-append mylist))
(define concat-str
  (lambda mylist
    (apply string-append (flatten mylist))))

(define (add-comma mylist)
  (concat-str (to-string (add-between mylist ", "))))

(define (pretty-print-line statement [breaknum 1])
  (concat-str (add-between (to-string statement)
                            " ") `(";" ,(make-string breaknum #\newline))))

(define (pretty-print statement)
  (concat-str (add-between (to-string statement)
                            " ")))

(define (pretty-args args)
  (add-comma (unparse-args args)))

(define (unparse-args args)
  (match args
    [(list (list t n) ... (list dt dn dv) ...)
     (map concat-str (map (lambda (x) (cond
                              [(eq? (length x) 2) (add-between (to-string x) " ")]
                              [(eq? (length x) 3) (add-between #:before-last " = "
                                                   (to-string x) " ")])) args))]))

(define (make-char char)
  (concat-str `("'" ,(cond [(symbol? char) (symbol->string char)]
                           [else char]) "'")))

(define (paren-unparse-expr expr)
  (cond [(list? expr) (list "(" (unparse-expr expr) ")")]
        [else (unparse-expr expr)]))

;;  Define the language rules here
;; Single term
(define-rule '(var a) '(pretty-print (list (cond [(string? a) (concat-str "\"" a "\"")]
                                              [else a]))))
;; Function application
(define-rule '(list a ..2) '(pretty-print (list (first expr) "(" (add-comma (map unparse-expr (rest expr))) ")")))

(define-rule '(list 'new type) '(pretty-print (list "new" type)))
(define-rule '(list 'new type expr ...) '(pretty-print (list "new" (unparse-expr (append (list type) expr)))))
(define-rule '(list 'new-array type num) '(pretty-print (list "new" type "[" num "]")))
(define-rule '(list 'del name) '(pretty-print (list "delete" name)))
(define-rule '(list 'del-array name) '(pretty-print (list "delete[]" name)))
(define-rule '(list 'decl type name) '(pretty-print (list type name)))
(define-rule '(list 'decl type name val) '(pretty-print (list type name op-assign (unparse-expr val))))
(define-rule '(list 'assign name val) '(pretty-print (list name op-assign (unparse-expr val))))
(define-rule '(list (? compound-assign-op? op) name val) '(pretty-print (list name (to-string op) (unparse-expr val))))
(define-rule '(list (? binary-op? op) op1 op2) '(pretty-print (list (paren-unparse-expr op1) (to-string op) (paren-unparse-expr op2))))
(define-rule '(list (? unary-op? op) op1) '(pretty-print (list (to-string op) (paren-unparse-expr op1))))
(define-rule '(list (? relation-op? op) op1 op2) '(pretty-print (list (paren-unparse-expr op1) (to-string op) (paren-unparse-expr op2))))
(define-rule '(list 'ret val) '(pretty-print (list "return" (paren-unparse-expr val))))

(define (unparse-expr expr)
  (match-rules expr))


(define (unparse-func func [indent 0])
  (match func
    [(list 'fn name args -> ret body ...)
     (cond [(eq? body '()) (pretty-print-line (flatten (list ret name "(" (pretty-args args) ")")) 2)]
           [else (concat-str (pretty-print (flatten (list ret name "(" (pretty-args args) ")"))) " {\n"
                       (unparse-statement-list body (+ indent 1)) "}\n\n")])]
    [(list 'fn name args body ...)
     (cond [(eq? body '()) (pretty-print-line (flatten (list "void" name "(" (pretty-args args) ")")) 2)]
           [else (concat-str (pretty-print (flatten (list "void" name "(" (pretty-args args) ")"))) " {\n"
                       (unparse-statement-list body (+ indent 1)) "}\n\n")])]))


(define (unparse-include stmt [indent 0])
  (let ([headers (rest stmt)])
    (define (to-header file)
      (cond [(string? file) (concat-str "\"" file "\"")]
            [(symbol? file) (concat-str "<" (symbol->string file) ">")]))
    (cond [(list? headers)
           (concat-str
            (append (map (lambda (file) (concat-str "#include " (to-header file) "\n")) headers)
             '("\n")))]
          [else (concat-str "#include " (to-header headers) "\n\n")])))

(define (unparse-define stmt [indent 0])
  (let ([symbol (car (rest stmt))]
        [expr (cadr (rest stmt))])
    (concat-str "#define " (pretty-print (list (symbol->string symbol) " " (unparse-expr expr))) "\n")))

(define (unparse-if stmt [indent 0])
  (match stmt
    [(list 'if (list conditions exprs ...) ..1)
     (let ([num-clauses (length conditions)]
           [num 0])
       (define (make-if i)
         (list (cond [(and (equal? i (- num-clauses 1)) (eq? (list-ref conditions i) 'else))
                      "else {\n"]
                     [else (concat-str (cond [(equal? i 0) (indent-line "if ( " indent)]
                                             [else "else if ( "])
                                       (unparse-expr (list-ref conditions i)) " ) {\n")])
               (unparse-statement-list (list-ref exprs i) (+ indent 1))
               (indent-line (cond [(equal? i (- num-clauses 1)) "}\n"]
                                             [else "} "]) indent)))
       
       (concat-str (apply append (map make-if (build-list num-clauses values)))))]))


(define (unparse-statement-list expr-list [indent 0])
  (match expr-list
    [(list a ..1)
     (let* ([expr (first expr-list)]
            [kw (cond [(list? expr) (caar expr-list)])])
       (let ([result (cond
                      [(eq? kw 'br) "\n"]
                      [(eq? kw 'include) (unparse-include expr)]
                      [(eq? kw 'def) (unparse-define expr)]
                      [(eq? kw 'fn) (unparse-func expr)]
                      [(eq? kw 'comment) (concat-str "/*" (unparse-statement-list (rest expr) indent) "*/\n")]
                      [(eq? kw 'if) (unparse-if expr indent)]
                      [(string? expr) (indent-line (concat-str "/* " expr "*/\n") indent)]
                      [(string? kw) (indent-line (concat-str kw "\n") indent)]
                      [else (concat-str (indent-line (unparse-expr expr) indent) ";\n")])])
         ;; (concat-str (list (indent-line (unparse-expr (first expr-list)) indent) ";\n"
         (concat-str result
                           ;; (concat-str (list (indent-line (unparse-expr (first expr-list)) indent)
                           (unparse-statement-list (rest expr-list) indent))))]
    ['() ""]
))


(define (begin-c stmt [filename ""])
  (cond
   [(eq? filename "") (printf (unparse-statement-list stmt))]
   [(string? filename)
    (with-output-to-file filename #:exists 'replace
      (lambda () (printf (unparse-statement-list stmt))))]))

(define (main stmts)
  `(fn main ([int argc] [char** argv]) -> int ,stmts))


(provide (all-defined-out))

