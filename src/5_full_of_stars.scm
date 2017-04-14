; rember* vs rember
; test using (define l '((coffee) cup ((tea) cup) (and (hick)) cup))
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))             ; is first element of l null? if so, return null
      ((atom? (car l))                   ; is first element of l an atom?
        (cond
          ((eq? (car l) a)                  ; if so, is it eqal to a?
            (rember* a (cdr l)))            ; then recur on the remainder of l, removing a
          (else (cons (car l)               ; otherwise, begin building new list
            (rember* a (cdr l))))))
    (else (cons (rember* a (car l))     ; if first element of l is neither null nor an atom...
        (rember* a (cdr l)))))))        ; then build new list...

; regarding the above:
; rember* can work on lists of lists, because it treats each element
; of the list as a list. of each element, it asks: "is this null?", "is this an atom?"
; see the first commandment!

; thinking more about lists:
(define l '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(car (car (car l)))        ; returns 'tomato
(car (car (car (cdr l))))  ; returns 'bean

; ... carry on...
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons old (cons new (insertR* new old (cdr l)))))
        (else (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l))          ; notice how the recursion splits into two branches:
                (insertR* new old (cdr l)))))))     ; 1) recurring down (car l) and 2) recurring down (cdr l)

; occur* counts occurences of a in l
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) a)
            (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
    (else (o+ (occur* a (car l))
              (occur* a (cdr l)))))))

; try occur* with l below
(define l '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

; N.B. *-functions still only hunt for atoms. So we couldn't do:
; (occur* '(banana brandy) l)

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
    (else
      (cons (subst* new old (car l))
            (subst* new old (cdr l)))))))
