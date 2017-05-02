; Ian Taylor's Additional Notes:

; Chapter 5
; thinking more about lists:
(define l '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(car (car (car l)))        ; returns 'tomato
(car (car (car (cdr l))))  ; returns 'bean
