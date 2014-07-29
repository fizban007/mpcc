#lang racket

(require "../mpcc.rkt")

(define endl '|std::endl|)

(begin-c
 `((include stdio.h iostream math.h)
   ("using namespace std;")
   
   (decl (const double) Pi (acos -1.0))
   (br)
   (fn test ([int& num] [int inc 1]))
   
   (fn test ([int& num] [int inc])
       ((assign num (+ num inc))))

   (fn main ([int argc] [(char *) |argv[]|]) -> int
       (decl int* a (new-array int 10))
       (decl (const int * const) c (new int 10))
       (decl int b (+ *a (* 4 1)))
       (br)
       (test b 2)
       (printf "%d\\n" b)
       ;; (cout *a " " b " " (+ b *c) ,endl)
       ;; ,(cout a b *a " " (+ b *c) endl)
       ,(apply-quote cout a b *a " " (+ b *c) ,endl)
       (ret 0))
   ))
;; "test.cpp")

