#lang racket

;;Question 1: Tail Recursion W/Helper Function
(define (string-multiply s n)
  (define (multiply-helper s n [accs ""])
    (if (equal? n 0)
        accs
        (multiply-helper s (- n 1) (string-append accs s))
        )
    )
  (multiply-helper s n)
)
(string-multiply "Hello" 3)
;;Question 1: LetRec example :-)
(define (string-multiply2 s n)
  (letrec ([helper (lambda (s n [accs ""])
                     (if (equal? n 0)
                         accs
                         (string-append s (helper s (- n 1) accs))
                         )
                     )
                   ])
    (helper s n)
   )
  )

