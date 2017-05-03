; Chapter 6: Shadows

; What is an arithmetic expression?
;   1 + 3 is, but (1 + 3) is only a REPRESENTATION of an arithmetic
;   expression, b/c it has parens.
;   really, (1 + 3) is an S-Expression, which represents 1 + 3.

; numbered? determines is an arithmetic expression representation
;   contains only numbers, +, x, and ^
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))             ; not sure about the simplified version. it doesn't check valid operators
    (else
      (and (numbered? (car aexp))
          (numbered? (car (cdr (cdr aexp)))))))))


; Definitions for Prefix Notation Expressions
; e.g. '(o* 1 3)
(define 1st-sub-exp-prefix
  (lambda (aexp)
    (car (cdr aexp))))

(define operator-prefix
  (lambda (aexp)
    (car aexp)))

; This is the same for prefix and infix notation
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value-prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator-prefix nexp) (quote o+))
        (o+ (value-prefix (1st-sub-exp-prefix nexp))
          (value-prefix (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote o*))
        (o* (value-prefix (1st-sub-exp-prefix nexp))
          (value-prefix (2nd-sub-exp nexp))))
    (else
        (o^ (value-prefix (1st-sub-exp-prefix nexp))
          (value-prefix (2nd-sub-exp nexp)))))))



; Definitions for Prefix Notation Expressions
; e.g. '(o* 1 3)
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote o+))
        (o+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote o*))
        (o* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
    (else
        (o^ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

; Test:
(value '((4 o+ 5) o* (2 o^ 3)))           ; => 72
(value-prefix '(o+ 1 (o^ 3 4)))           ; => 82
