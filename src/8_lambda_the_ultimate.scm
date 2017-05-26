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


; A fancy way to define rember
(define seqrem
  (lambda (new old l) l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))    ; this is tricky... #f is a placeholder of sorts, just to provide a
                                    ; parameter for insert-g.

;; FULL DISCLOSURE:
; I copied these next functions from https://github.com/pkrumins/the-little-schemer/blob/master/08-lambda-the-ultimate.ss#L266
; If I have a criticism of this book, it's that it can be inconsistent, and its typeface can be confusing.
; Even when directly copying from the book, I could get these functions to work, so I cheated a little.

(define operator
  (lambda (aexp)
    (car aexp)))          ; note: for some reason, in ch 6 operator is defined with (car (cdr aexp))

; value uses 1st-sub-exp
;
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; value uses 2nd-sub-exp
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define atom-to-function
  (lambda (atom)
    (cond
      ((eq? atom 'o+) +)
      ((eq? atom 'o*) *)
      ((eq? atom 'o^) expt)
      (else #f))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
    (else
      ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))


; now doing the same w/ multirember
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat))
          ((multirember-f test?) a
            (cdr lat)))
      (else
        (cons (car lat)
          ((multirember-f test?) a
            (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define multiremberT
(lambda (test? lat)
  (cond
    ((null? lat) (quote ()))
    ((test? (car lat))
      (multiremberT test? (cdr lat)))
  (else (cons (car lat)
    (multiremberT test? (cdr lat)))))))

; using collectors:
; this function looks at every atom of lat to see whether it is eq? to a.
; non-eq? atoms are collected in one list ls1; the others for which the answer
; is true are collected in a second list ls2. Finally, it determines the value
; of (f ls1 ls2)
(define multirember&co
  (lambda (a lat col)                   ; col stands for collector / continuation
    (cond
      ((null? lat)
        (col (quote ()) (quote ())))
      ((eq? (car lat) a)
        (multirember&co a
          (cdr lat)
          (lambda (newlat seen)
            (col newlat (cons (car lat) seen)))))
    (else
      (multirember&co a
        (cdr lat)
        (lambda (newlat seen)
          (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
      (cons (quote tuna) seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons (quote and) newlat)
      seen)))

(define last-friend
  (lambda (x y)
    (length x)))

; using the above function, we can say
; (define ls '(strawberries tuna and swordfish))
; (define col last-friend)
; and then (multirember&co (quote tuna) ls col) => 3
; b/c ls contains three things that are not tuna, and
; therefore the last-friend function will be used on ls and '(tuna)

; see https://en.wikipedia.org/wiki/Continuation-passing_style

; multiinsertLR inserts mnew to left of oldL and to right of oldR
; in lat if oldL and oldR are different
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL)
        (cons new (cons oldL
          (multiinsertLR new oldL oldR
            (cdr lat)))))
      ((eq? (car lat) oldR)
        (cons oldR (cons new
          (multiinsertLR new oldL oldR
            (cdr lat)))))
    (else
      (cons (car lat)
        (multiinsertLR new oldL oldR
          (cdr lat)))))))

; now define w/ a collector
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
        (multiinsertLR&co new oldL oldR
          (cdr lat)
          (lambda (newlat L R)
            (col (cons new (cons oldL newlat))
                 (add1 L) R))))
      ((eq? (car lat) oldR)
        (multiinsertLR&co new oldL oldR
          (cdr lat)
          (lambda (newlat L R)
            (col (cons oldR (cons new newlat))
                 L (add1 R)))))
    (else
      (multiinsertLR&co new oldL oldR
        (cdr lat)
        (lambda (newlat L R)
          (col (cons (car lat) newlat)
               L R)))))))

;; Notes on the above: In CPS, when we define a function, the last argument is essentially
; a function that we want to call when returning. In this case, we're going to return col.
; Thus, when we actually call the function, we must define col to decide what to do with the return.
; On each recursion, col changes, due to the lambda passed after (cdr lat). Once we hit the null case,
; we will then pass '() 0 0 as arguments to col. These arguments will become newlat, L R, for the final col,
; which will then begin to unwind the stack and evaluate all the col functions

; resuming ...
;remember:
;all *-functions work on lists that are either
; - empty,
; - an atom consed onto a list , or
; - a list consed onto a list.

; even? checks if an argument is even
(define even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

; remove all odd numbers from a list of nested lists
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((even? (car l))
            (cons (car l)
                  (evens-only* (cdr l))))
        (else (evens-only* (cdr l)))))
    (else (cons (evens-only* (car l))
                (evens-only* (cdr l)))))))

; let the fun begin:
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl) (o* (car l) p) s))))
       (else
         (evens-only*&co (cdr l)
                         (lambda (newl p s)
                           (col newl p (+ (car l) s)))))))
    (else
      (evens-only*&co (car l)
                      (lambda (al ap as)
                        (evens-only*&co (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl)
                                               (o* ap dp)
                                               (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))
