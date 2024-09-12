#lang racket

;;Question 1: Tail Recursion W/Helper Function
(define (string-multiply s n)
  (define (multiply-helper s n [accs ""])
    (if (equal? n 0)
        accs
        (string-append s (multiply-helper s (- n 1) accs))
        )
    )
  (multiply-helper s n)
)

;;