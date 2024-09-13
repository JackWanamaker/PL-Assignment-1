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
(string-multiply "dog" 5)
;;Question 1: LetRec example w/ Tail Recursion :-)
(define (string-multiply2 s n)
  (letrec ([helper (lambda (s n [accs ""])
                     (if (equal? n 0)
                         accs
                         (helper s (- n 1) (string-append accs s))
                         )
                     )
                   ])
    (helper s n)
   )
  )
(string-multiply2 "dog" 5)

;;Question 2a
(define (pyramid-print s n)
  (define accn 0)
  (letrec ([helper (lambda (s n [accs s])
                     (displayln accs)
                     (set! accn (+ accn 1))
                     (if (= accn n)
                         (void)
                         (helper s n (string-append accs s s))
                     ))]
           )
    (helper s n)
    )
  )
(pyramid-print "x" 4)
;;Question 2b
(define (centered-pyramid s n)
  (define accn 0)
  (letrec ([helper (lambda (s n [accs s])
                     (display (whitespace n))
                     (displayln accs)
                     (set! accn (+ accn 1))
                     (if (= accn n)
                         (void)
                         (helper s n (string-append accs s s))
                     ))]
           [whitespace (lambda (n [spacestring ""] [whiten accn])
                         (if (= whiten n)
                             spacestring
                             (whitespace n (string-append spacestring " ") (+ whiten 1))
                        ))]
           )
    (helper s n)
    )
  )
(centered-pyramid "x" 4)

;;Question 3 
(define (lst-alphabetized? myList)
  (define alpha #t)
  (letrec ([helper (lambda (myList [prevItem (first myList)] [alphabet alpha])
                     (cond
                       [(false? alphabet)
                        (set! alpha #f)]
                       [(empty? myList)
                        (void)]
                       [(if (string>=? (first myList) prevItem)
                            (helper (rest myList) (first myList) #t)
                            (helper `() "" #f))])
                     alpha)])
    (helper myList)
    ))
;;(lst-alphabetized? (list "apple" "banana" "carrot"))
(lst-alphabetized? (list "apple" "banana" "carrot"))
(lst-alphabetized? (list "zebra" "yak" "xenomorph"))
