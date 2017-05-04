; Chapter 7: Friends And Relations

; true for S-expressions where each atom occurs no more than once
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat))
        (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat)))))))

; makeset using multirember, which will remove all occurences
; of (car lat) from (cdr lat)
(define makeset-multi
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
    (else (cons (car lat)
          (makeset-multi (multirember (car lat) (cdr lat))))))))
