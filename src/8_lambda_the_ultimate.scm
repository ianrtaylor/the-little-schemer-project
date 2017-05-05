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
  (lambda (test? a l)                      ; test? is whatever function we pass to rember-f
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))          ; if test? compares (car l) and 'a truthfully, then return the cdr of l
    (else
      (cons (car l)
        (rember-f test? a (cdr l)))))))

; Currying: Translating a function w/ multiple arguments
; into a sequence of functions, each w/ a single arg
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; (eq?-c 'salad) => (lambda (x) (eq? x 'salad))
; which means, eq?-c returns a function!

; A Curried version of rember-f:
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  ((rember-f test?) a (cdr l))))))))

; Redefining insertL and insertR... I skipped ahead a bit
; for comprehension's sake. These two functions differ by
; the order in which they cons arguments onto the list.
; Thus, we can define those differences independently:
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; Now we define insert-g such that it can
; be given an argument as to which seqence
; function to use:
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
              (seq new old (cdr l)))  ; use whichever sequence we supply to cons new and old onto cdr l
      (else (cons (car l)
                  ((insert-g seq) new old
                    (cdr l))))))))

; insert-g can now be used to define insertL and insertR
(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

; Read the above like this:
; insertL is the insert-g function which uses seqL
; insertR is the insert-g function which uses seqR


; A better way to define these functions uses the
; seq functions anonymously, which makes it clear
; how insert-g is behaving
(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define insertR
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

; doing the same for subst, our substitution function
(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))   ; not quite sure about this one

; another version of rember, using seqrem:
(define seqrem
  (lambda (new old l) l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
