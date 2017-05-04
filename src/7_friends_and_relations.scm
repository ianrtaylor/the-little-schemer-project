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

; is each atom of set1 in set2?
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
    (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

; Set Equality: Two sets are equal if they are
; both subsets of each other.
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))
