; incremement number by adding 1
(define add1
  (lambda (n)
  (+ n 1)))

; decrement number by subtracting 1
(define sub1
  (lambda (n)
  (- n 1)))

; define addition and subtraction as recursive operations
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

; sum a list of numbers, a.k.a. a "tuple"
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

; define multiplication recursively
(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

; sum elements of two tups
(define tup+  (lambda (tup1 tup2)    (cond      ((null? tup1) tup2)      ((null? tup2) tup1)      (else        (cons (+ (car tup1) (car tup2))          (tup+ (cdr tup1) (cdr tup2)))))))

; comparison operators, defined recursively
; greater than:
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

; less than:
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

; equality:
(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

; exponent operators
(define o^
  (lambda (n m)
    (cond
      ((zero? n) 0)
      ((zero? m) 1)
      (else (o* n (o^ n (sub1 m)))))))

; division
(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

; get the length of a list
;(defined as olength, since there's already a length)
(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (olength (cdr lat)))))))

; pick the nth element (not 0 indexed!)
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; pick the element index to remove
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
            (rempick (sub1 n) (cdr lat)))))))

; remove all number atoms from list
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
        ((number? (car lat))
        (no-nums (cdr lat)))
        (else (cons (car lat)
              (no-nums (cdr lat)))))))))

; generate a tuple of only number elements from lat
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
        ((number? (car lat))
        (cons (car lat) (all-nums (cdr lat))))
        (else (all-nums (cdr lat))))))))

; return true if args a1 and a2 are the same atom
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

; how many times does atom a appear in lat
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
        ((eq? (car lat) a)
        (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat))))))))

; is n == 1?
(define one?
  (lambda (n)
    (o= n 1)))

; better version of rempick
(define rempick-better
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
            (rempick (sub1 n) (cdr lat)))))))
