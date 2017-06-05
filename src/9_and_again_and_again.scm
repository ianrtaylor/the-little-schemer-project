; Chapter 9: ... and Again, and Again, and Again...

; looking is a "partial function", as opposed to a "total function"
(define looking
(lambda (a lat)
  (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
        (keep-looking a (pick sorn lat) lat))  ; keep-looking does not recur on a smaller portion of lat, like (cdr lat)
    (else                                      ; b/c of this, there's a chance the recursion might not terminate!
      (eq? sorn a)))))

; eternity is an infinite loop
(define eternity
  (lambda (x)
    (eternity x)))

; shift takes a pair whose first component is a pair,
; and builds a pair by shifting the second part of the first
; component into the second component.
(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair))
        (second pair)))))

; ; ; ; ;
; The next three functions (align, length*, and weight*) are related:
; length* and weight* are used to test facts about how align works, to
; determine if it will yield a value for every argument.
; weight* demonstrates that align's arguments become successively smaller in "weight",
; and thus they approach a point where the function terminates.
; ; ; ; ;

; align violates the 7th Commandment... but it is NOT a partial function
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
        (align (shift pora)))   ; shift only rearranges the pair it gets. it doesn't make the argument smaller.
    (else (build (first pora)
                 (align (second pora)))))))

; length* counts the number of atoms in align's arguments
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
    (else
      (o+ (length* (first pora))
          (length* (second pora)))))))

; the more times first gets called, the greater the weight
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
    (else
      (o+ ( o* (weight* (first pora)) 2)
          (weight* (second pora)))))))

; ; ; ; ;
;
; ; ; ; ;

; shuffle is like align, but uses ch7's revpair function
; shuffle is not a total function. it will never terminate
; for arguments like '((a b) (c d))
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
        (shuffle (revpair pora)))
    (else (build (first pora)
      (shuffle (second pora)))))))


; The one? function is true when n == 1
(define one?
  (lambda (n) (= n 1)))

; no one knows if this function is total or not...
; see Lothar Collatz, https://en.wikipedia.org/wiki/3x_%2B_1_problem, https://esolangs.org/wiki/Collatz_function,
; and also https://xkcd.com/710/
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
    (else
      (cond
        ((even? n) (C (/ n 2)))
      (else
        (C (add1 (* 3 n)))))))))

; see Wilhelm Ackermann, https://en.wikipedia.org/wiki/Ackermann_function
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else
        (A (sub1 n)
           (A n (sub1 m)))))))

; (A 4 3) will take a long, LONG time to calculate.

; So, we need a tool that can tell us whether or not a function will
; return a value for every argument. This way, we could know if a function
; is stuck in an infinite loop, or just taking a long time.
