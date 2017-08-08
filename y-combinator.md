Chapter 9 is brutal...
I'm going to be revisiting it repeatedly, refining my notes for a while.

In the mean time, skipping ahead to Ch 10

```scheme
(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

     (define (part-factorial self)
         (let ((f (self self)))
           (lambda (n)
             (if (= n 0)
                 1
                 (* n (f (- n 1)))))))


                 (define (part-factorial self)
                     ((lambda (f)
                        (lambda (n)
                          (if (= n 0)
                              1
                              (* n (f (- n 1))))))
                      (self self)))

                      (define (part-factorial self)
                        (almost-factorial
                          (self self)))
    (define factorial
        ((lambda (x) (x x))
         (lambda (self)
           (almost-factorial (self self)))))
```
