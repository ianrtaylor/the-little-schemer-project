; 08/19/2017
; irtaylor's Notes, since this chapter is hard(er):
; *-functions work on lists of S-expressions. Their distinguishing feature,
; is that they recur down both the car AND the cdr of a list. This defines
; a recurrence relation, as in T(n) = 2T(n/2) + n ... well, not that equation
; exactly, but something like it. This is best visualized as a tree structure:
; One branch proceeds down the car of the list, and the other branch proceeds
; down the cdr of the list.

; use for these exercises:
(define l '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
(define new 'orange)
(define old 'banana)

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

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (cons old                 ; cons both new AND old, so that we can recur down the
              (insertL* new old (cdr l)))))     ; remaining elements of l
        (else (cons (car l) (insertL* new old (cdr l))))))
    (else (cons (insertL* new old (car l))
                (insertL* new old (cdr l)))))))

; return true/false if a is/is not an atom in l
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))              ; if (car l) is an atom...
        (or (eq? a (car l))         ; ...then return the answer to the question:...
            (member* a (cdr l))))   ; ...is a equal to  (car l) or is a an atom of (cdr l)?
    (else (or (member* a (car l))
              (member* a (cdr l)))))))


; leftmost is not a true *-function, since it only recurs down
; the list's car; it does not recur down the cdr.
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
    (else (leftmost (car l))))))

; rewritten version of eqlist?
