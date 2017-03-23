rlwrap scheme     # run the scheme repl with arrow keys enabled


In the MIT-GNU REPL, often need to quote() things or use ' for certain functions
E.g.
(cons 'peanut '(butter and jelly))
(car '(turkey))
(cdr '(hamburger 1 2 3))
(car (quote (turkey 1))) identical to (car '(turkey 1))

null? returns false for everything except the null list ()




How to Answer the question: "What does f() do?":
The function f() takes <num_args> arguments of type <specify_type>
