; Ian Taylor's Additional Notes:

; Chapter 5
; thinking more about lists:
(define l '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(car (car (car l)))        ; returns 'tomato
(car (car (car (cdr l))))  ; returns 'bean


; Chapter 5
; Definition of an S-Expression (p 92):
;   An S-expression is either an atom or a
;   (possibly empty) list of S-expressions.

; Chapter 7
; Test
(set? '(apples peaches pears plums))           ; => #t
(set? '(apples peaches pears apples plums))    ; => #f
(makeset '(apples peaches pears apples plums))
(makeset-multi '(apple peach pear peach plum apple lemon peach))
(makeset-multi '(apple 3 pear 4 9 apple 3 4))

(define set1 '(stewed tomatoes and macaroni))
(define set2 '(macaroni and cheese))
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(revrel '((a b) (c d) (e g)))
(revrel2 '((a b) (c d) (e g)))

(equal? (revrel '((a b) (c d) (e g)))
        (revrel2 '((a b) (c d) (e g))))

(fun? '((grape raisin) (plum prune) (stewed grape))) ; => #t

(fullfun? '((grape raisin) (plum prune) (stewed prune))) ; => #f
(fullfun? '((grape raisin) (plum prune) (stewed grape))) ; => #t

(one-to-one? '((chocolate chip) (doughy cookie))) ; => #t


; Chapter 8
; Example: Name of the function returned by
; (rember-f test?), where test? is eq?
(define test? eq?)
(define rember-eq? (rember-f test?))

(define a 'sausage)
(define l '(pizza with sausage and bacon))

; test atom-to-function
(define nexp '(+ 5 3))
(atom-to-function (operator nexp))

; multirember-f
(define a 'tuna)
(define lat '(shrimp salad tuna salad and tuna))
((multirember-f eq?) a lat)

; multirember&co
(define a 'tuna)
(define lat '(strawberries tuna and swordfish))
(define col a-friend)

(define ls '(strawberries tuna and swordfish))
(define col last-friend)

; this is called Continuation-passing_style.
; try a simpler example:
(define list-sum
  (lambda (l k)
    (cond
      ((null? l) (k 0))
      ((list-sum (cdr l) (lambda (s) (k (+ s (car l)))))))))

(list-sum '(1 2 3 4) (lambda (x) x))   ; here, when we call the function, we pass a lambda for k


;(define add_cps
;  (lambda (a, b, done)))


;; multiinsert&co:
(define lat (quote (bread chips and fish or fish and chips)))
(define col
  (lambda (lat L R)
    lat))
(multiinsertLR&co 'salty 'fish 'chips lat col) ; => (col (quote (chips salty and salty fish or salty fish and chips salty)) 2 2)
