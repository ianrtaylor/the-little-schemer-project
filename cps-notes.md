Here are some notes on multiinsertLR&co, which is a really challenging exercise.
The basic definition for multiinsertLR&co is:

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)...)...)

Recall that with all recursive functions, we recur until we hit our base case.
For this function, the base case is:

((null? lat) (col (quote ()) 0 0))

By the time we hit the null case, however, we have  repeatedly passed new lambdas
for col, such that the col function is recursively built up.

Say, for example, that we run multiinsertLR&co as follows:

(define lat (quote (bread chips and fish or steam and chips)))
(define col
  (lambda (lat L R)
    lat))
(multiinsertLR&co 'salty 'fish 'chips lat col)  ;; => '(bread chips salty and salty fish or steam and chips salty)

By the time we hit the null case, col has been defined as follows:

((lambda (newlat L R)
   ((lambda (newlat L R)
      ((lambda (newlat L R)
         ((lambda (newlat L R)
            ((lambda (newlat L R)
               ((lambda (newlat L R)
                  ((lambda (newlat L R)
                     ((lambda (newlat L R)
                        ((lambda (lat L R) lat)
                         (cons
                          (car
                           (list
                            'bread
                            'chips
                            'and
                            'fish
                            'or
                            'steam
                            'and
                            'chips))
                          newlat)
                         L
                         R))
                      (cons 'chips (cons 'salty newlat))
                      L
                      (add1 R)))
                   (cons
                    (car
                     (list 'and 'fish 'or 'steam 'and 'chips))
                    newlat)
                   L
                   R))
                (cons 'salty (cons 'fish newlat))
                (add1 L)
                R))
             (cons (car (list 'or 'steam 'and 'chips)) newlat)
             L
             R))
          (cons (car (list 'steam 'and 'chips)) newlat)
          L
          R))
       (cons (car (list 'and 'chips)) newlat)
       L
       R))
    (cons 'chips (cons 'salty newlat))
    L
    (add1 R)))
 '()
 0
 0)


... which is straight-up crazy. 
