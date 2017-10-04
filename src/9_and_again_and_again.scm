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

; write a function that checks whether some function stops for just the
; empty list:

; (define will-stop?
;   (lambda (f)
;     ...))

; This function is impossible to define because of the Halting Problem!
; will-stop? is a function that we can describe precisely, but cannot define
; in our language.



; ok... so what is (define ...) ???
; GET READY!

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

; can we define length w/o the `define` keyword?
; w/o define we cannot refer to length

; this function determines the length of the empty list and nothing else
(lambda (l)
  (cond
    ((null? l) 0)
  (else (add1 (eternity (cdr l))))))    ; any argument to eternity returs no answer

; this function is perhaps best named length0, but w/o `define` we cannot call length0
; this makes it difficult to continue defining things like length<=1, length<=2, etc
; thus, if we were to try and define lenght1 (i.e. the function that determines),
; the length of lists that contain one or fewer items, we'd get:

; legnth<=1
(lambda (l)
  (cond
    ((null? l) 0)
  (else
    (add1
      ((lambda (l)        ; here we simply write out lenght0
        (cond
          ((null? l) 0)
        (else (add1
              (eternity (cdr l))))))
      (cdr l))))))    ; i.e. : (add1 length0 (cdr l))

; we cannot write out length∞, so we must figure out a different solution:
((lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
    (else (add1 (length (cdr l)))))))
 eternity)

; note for the above: eternity is just a name. don't worry about whether or not it's been given any arguments.
; since we're defining Higher Order Functions, we can treat functions as variables.


 ; length<=1
 ((lambda (f)
    (lambda (l)
      (cond
        ((null? l) 0)
      (else (add1 (f (cdr l)))))))
((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
      (else (add1 (g (cdr l)))))))
eternity))

; etc...

; now, we can make the function that takes length as an argument, and returns
; the function that looks like length:
; mk-length for "make length"

; length0
((lambda (mk-length)
    (mk-length eternity))
 (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; ...etc...
; length<=3
((lambda (mk-length)
  (mk-length
    (mk-length
      (mk-length
        (mk-length eternity)))))
 (lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
    (else (add1 (length (cdr l))))))))

; ...and so on...
; recursion is thus like an infinite tower of applications of mk-length to an
; arbitrary function. b/c it is arbitrary, we could just pass in mk-length,
; and use mk-length instead of length
((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
  (lambda (l)
    (cond
      ((null? l) 0)
    (else (add1 (mk-length (cdr l))))))))

; length<=1 again:
((lambda (mk-length)
    (mk-length mk-length))
 (lambda (mk-length)
  (lambda (l)
    (cond
      ((null? l) 0)
    (else (add1
      ((mk-length eternity)
        (cdr l))))))))


; Another note: when tryign to decide how to de-recursion a function,
; try pulling out the recursive call, and making it a parameter of a function.
; For example, the recursive factorial:
(define factorial
  (lambda (n)
    (if (= n 0) 1
        (* n (factorial (- n 1))))))

; And here, abstracting out the recursion:
(define almost-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 0) 1
          (* n (f (- n 1)))))))

; see how almost-factorial is Higher Order? It must take a function f as its
; argument, otherwise it won't make any sense.

; (Y f) = fixpoint-of-f
; (f fixpoint-of-f) = fixpoint-of-f
; (Y f) = fixpoint-of-f = (f fixpoint-of-f)
; (Y f) = (f (Y f))

; lazy, non-combinator version:
; (define Y
;   (lambda (f)
;     (f (Y f))))

; doesn't work in strict scheme, because
; (Y f)
;   = (f (Y f))
;   = (f (f (Y f)))
;   = (f (f (f (Y f))))
; etc...

; to make a strict-compatible version, we must realize that (Y f) is going
; to become a function that takes one argument. Thus, we can say:
; (Y f) = (lambda (x) ((Y f) x))
;
; similarly: cos = (lambda (x) (cos x))
; i.e., we could use cos or (lambda (x) (cos x)) to compute cosines, and they
; would both do the exact same things

; thus:
(define Y
  (lambda (f)
    (f (lambda (x) ((Y f) x)))))

; however, this is not the true Y-combinator, b/c it is explicitly recursive, and thus has free variables.
