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

; is at least one atom of set1 in set2?
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
    (else
      (or (member? (car set1) set2)
          (intersect? (cdr set1) set2))))))

; return the intersection of set1 and set2
; i.e. return the atoms shared by set1 and set2
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2)))))

; return the union of set1 and set2
; i.e. return non-duplicate list of
; all atoms in set1 and set2
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)        ; this step eliminates duplicates
       (union (cdr set1) set2))         ; if (car set1) in set2, then we don't need to save it
    (else (cons (car set1)
          (union (cdr set1) set2))))))

; set difference function:
; return set1 - set2
(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (xxx (cdr set1) set2))
    (else (cons (car set1)
          (xxx (cdr set1) set2))))))

; given a list of sets,
; return the atoms common to all
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set)
            (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((null? x) #f)
      ((atom? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
    (else #f))))
