; Chapter 8: Lambda The Ultimate!!
; I'm so excited!

; -f functions are all about taking other functions as arguments
; Thus, the power of the Lambda is the use of functions as data

; Up to this point, there are three versions of rember:
;   - rember using =
;   - rember using eq?
;   - rember using equal?

; rember-f can be whichever version we want, since we tell it which
; equality-checking function to use:
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
              (rember-f test? a (cdr l)))))))

; a curried equality functions
; curried functions chain functions of one argument each:
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; (eq?-c 'salad) returns a function that takes x as an argument
; and tests whether x is eq? to 'salad.
; we can name that function thusly:
(define eq?-salad
  (eq?-c 'salad))

; really, there's no need to name this function though. we could do:
; ((eq?-c x) y) where x is 'salad and  y is 'tuna.

; rember-f takes a function as a parameter, and it RETURNS A
; FUNCTION which takes two parameters and compares them using the
; function-parameter initially passed to rember-f
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
      (else (cons (car l)
        ((rember-f test?) a (cdr l))))))))

; the same goes for insertL-f and insertR-f:
; they take functions as parameters and return new functions
; which operate differently based on the original function passed in
; as a parameter
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
          (cons new (cons old (cdr l))))
      (else (cons (car l)
            ((insertL-f test?) new old
              (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
          (cons old (cons new (cdr l))))
      (else (cons (car l)
            ((insertR-f test?) new old
              (cdr l))))))))


; if we want to make insertR-f and insertL-f more generic,
; we can define a function insert-g which will insert an argument
; to either the right or left of the desired argument.

; to do this, we need to first define functions that insert left and right.
; these two functions will essentially be the difference between insertL and insertR:
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; now we can define insert-g:
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
          (seq new old (cdr l)))        ; here we can use the function seq
      (else (cons (car l)
            ((insert-g seq) new old
              (cdr l))))))))

; now we can simplify insertL and insertR
(define insertL (insert-g seqL))

(define insertR (insert-g seqR))

; and now we can define them again in the most generic way,
; i.e. by passing the lambda itself, rather than naming it
(define insertL
  (insert-g
    (lambda (new old l)               ; this is the seqL lambda
      (cons new (cons old l)))))

(define insertR
  (insert-g
    (lambda (new old l)               ; this is the seqR lambda
      (cons old (cons new l)))))

; Now we can do the same again for subst, which could be like:
(define subst
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((eq? (car l) old)
        (cons new (cdr l)))                ; this is like our seq functions
    (else (cons (car l)
            (subst new old (cdr l)))))))

; First, we'll define the sequence function:
(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))
