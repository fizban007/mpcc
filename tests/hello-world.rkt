#lang racket

(require "../mpcc.rkt")

(define cout
  (lambda args
    (list (pretty-print (list "std::cout <<" (add-between (map paren-unparse-expr args) "<<") ";")))))

(define using
  (lambda namespaces
    (list (concat-str (map (lambda (name) (concat-str (pretty-print (list "using namespace" name)) ";\n")) namespaces)))))
;;     (list (pretty-print (list "using namespace")))))

(define endl '|std::endl|)

(begin-c
 `((include stdio.h iostream math.h)
   "Use namespace std here"
   ,(apply-quote using std)
   
   (decl (const double) Pi (acos -1.0))
   "This is a line break"
   (br)
   
   "Dummy function declaration here, just to show off default values"
   (fn test ([int& num] [int inc 1] [double*** ptr 0]))

   "Real function definition
and two-line comment"
   (fn test ([int& num] [int inc] [double*** ptr])
       (assign num (+ num inc)))

   (fn main ([int argc] [(char *) |argv[]|]) -> int
       (decl int* a (new-array int 10))
        (decl (const int * const) c (new int 10))
        (decl int b (+ *a (* 4 1)))
        "This is another line break"
        (br)
        (test b 2)
        (printf "%d\\n" b)
        ;; (cout *a " " b " " (+ b *c) ,endl)
        ;; ,(cout a b *a " " (+ b *c) endl)
        ,(apply-quote cout a b *a " " (+ b *c) ,endl)
        (ret 0)))
 "hello-world.C")

