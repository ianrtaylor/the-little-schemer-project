; Chapter 10: What Is The Value of All of This?

; Entry: A pair of lists whose first list is a set (i.e. no atom occurs more than once)
; Additionally, both lists must be of equal length. An entry is a type of key-value store

; build an entry from a set of names and a list of values:
(define new-entry build)

(build '(appetizer entree bevarage)
        '(pate boeuf vin))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
      (first entry)               ; names is the list of keys
      (second entry)              ; values is the list of values
      entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))    ; we let the user decide what to do when names is null
      ((eq? (car names) name)
        (car values))
    (else (lookup-in-entry-help name
            (cdr names)
            (cdr values)
            entry-f)))))

; (lookup-in-entry 'entree '((appetizer entree bevarage) (pate boeuf vin))
;   (lambda (x) 'notfound))  => 'boeuf

; A table, or environment, is a list of entries. '() is the empty table
; extend-table takes an entry and a table and creates
; a new table, putting the new entry in front of the old table:
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
    (else (lookup-in-entry name
            (car table)
            (lambda (name)
              (lookup-in-table name
                (cdr table)
                table-f)))))))

; recall the function value from ch 6...
; "rep" means representation:
(define rep-a 'a)
(define rep-b 'b)
(define rep-c 'c)
(define rep-car 'car)
(define rep-quote 'quote)

(cons rep-a
  (cons rep-b
    (cons rep-c
      (quote ()))))     ; => '(a b c)

; this is a representation of the expression below:
(cons rep-car
  (cons (cons rep-quote
          (cons
            (cons rep-a
              (cons rep-b
                (cons rep-c
                  (quote ()))))
              (quote ())))
        (quote ())))              ; => '(car (quote (a b c)))

; note in above, that we need to cons things onto quote () so that the representation
; has correctly formatted parentheses

(define e '(car (quote  (a b c))))
; we need to redine value to be able to call "(value e) => 'a"
; ...

; thus far, we have encountered six types:
; *const, *quote, *identifier, *lambda, *cond, *application
