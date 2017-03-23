; remember: remove member a from lat
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat ))
      (else (cons (car lat)
            (rember a (cdr lat)))))))

; firsts takes a list (null or composed of non-empty lists).
; it builds another list composed of first S-expression of
; each internal list.
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

; insert new to the Right of first occurence of old in lat
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))          ; null returns null
      (else (cond
          ((eq? (car lat) old)          ; is the first element of lat equal to old?
            (cons old                   ; if yes, cons old onto
              (cons new (cdr lat))))    ; a new list made by consing new onto the remainder of lat (i.e. everything after old)
          (else (cons (car lat)         ; if we have found old yet, save the first element of lat
                (insertR new old        ; and cons it onto the insertR list of everything remaining
                  (cdr lat)))))))))

; insert new to the Left of first occurence of old in lat
(define insertL
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
    (else (cond
        ((eq? (car lat) old)            ; is the first element of lat equal to old?
          (cons new lat))               ; if so, cons new onto lat
        (else (cons (car lat)
              (insertL new old
                (cdr lat)))))))))

; substitute the first occurence of old with new in lat
(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) (quote ()))
  (else (cond
    ((eq? (car lat) old)
      (cons new (cdr lat)))
  (else (cons (car lat)
      (subst new old
        (cdr lat)))))))))

; replace either first occurence of o1 or
; first of o2 with new
(define subst2
  (lambda (new o1 o2 lat)
  (cond
      ((null? lat) (quote ()))
      (else (cond
        ((or (eq? (car lat) o1)
              (eq? (car lat) o2))
                (cons new (cdr lat)))
      (else (cons (car lat)
        (subst2 new o1 o2
        (cdr lat)))))))))

; remove all occurences of a from lat
(define multirember
  (lambda (a lat)
  (cond
    ((null? lat) (quote ()))
    (else (cond
      ((eq? (car lat) a)                    ; if the first element of lat is equal to a
        (multirember a (cdr lat)))          ; continue to hunt for a. no need to cons, b/c we're leaving a out!
        (else (cons (car lat)                     ; otherwise, cons first element
              (multirember a (cdr lat)))))))))
