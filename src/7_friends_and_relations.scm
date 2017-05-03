; Chapter 7: Friends And Relations

; true for S-expressions where each atom occurs no more than once
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat))))))
