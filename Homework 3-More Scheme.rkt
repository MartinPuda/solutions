#lang racket

;Homework 3: More Scheme

(define (echo lst)
  (if (empty? lst) '()
      (cons (first lst)
            (cons (first lst)
                  (echo (rest lst))))))

(echo '(a b c))

(define (repeat x n)
  (if (zero? n) '()
      (cons x (repeat x (sub1 n)))))

(define (echo-lots lst n)
  (if (empty? lst) '()
      (append (repeat (first lst) n)
              (echo-lots (rest lst) n))))

(echo-lots '(a (b c)) 3)

(define (echo-all o)
  (if (list? o)
      (map echo-all (echo o))
      o))

(echo-all '(a (b c)))

(define (nth i lst)
  (if (zero? i)
      (first lst)
      (nth (sub1 i) (cdr lst))))

(nth 0 '(a b c))
(nth 1 '(a (b c) d))

(define (assoc-all keys a-list)
  (map (lambda (key) (second (assoc key a-list))) keys))

(assoc-all '(a d c d) '((a apple)(b boy)(c (cat cow))(d dog)))

(define (my-filter fn lst)
  (cond ((empty? lst) '())
        ((fn (first lst))
         (cons (first lst)
               (my-filter fn (rest lst))))
        (else (my-filter fn (rest lst)))))
        
(my-filter even? '(3 4 6 7 8))      
