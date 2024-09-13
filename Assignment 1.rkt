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
(lst-alphabetized? (list "apple" "banana" "carrot"))
(lst-alphabetized? (list "zebra" "yak" "xenomorph"))

;;Question 4a
(define (recursive-power base exp)
  (letrec ([helper (lambda (base exp [acc 0])
                     (if (equal? acc exp)
                         1
                         (* base (helper base exp (+ acc 1)))))])
    (helper base exp)))
(recursive-power 3 4)

#|Question 4b
As we first call the function, we add two items to the stack:
1. The initial function
2. The helper function.
As it is, this is still a small stack. However, as soon as the first if statement
is evaluated, and another call on the helper function occurs, there is an
additional item added to the stack. Looking at the variables, there is also
no way for us to track the progress of the recursion. This is really the crux
of why regular recursion can be problematic. Only until we reach the base case
do any calculations start occurring to find our result. However, by the time we
reach this, the stack is already 6 items high. Imagine what it would be with
a bigger exponent. The stack would keep growing!
|#
;;Question 4c
(define (tail-power base exp)
  (letrec ([helper (lambda (base exp [acc 0] [result 1])
                     (if (equal? acc exp)
                         result
                         (helper base exp (+ acc 1) (* result base))))])
    (helper base exp)))
(tail-power 2 3)

;;Question 5a
(define (fib n)
  (if (or (= n 0) (= n 1))
      1
      (+ (fib (- n 1)) (fib (- n 2))) ) )
(fib 4)

(define (tail-fib n)
  (letrec ([helper (lambda (n [acc 0] [result 1] [prev 0])
                     (if (= n acc)
                         result
                         (helper n (+ acc 1) (+ result prev) result)))])
    (helper n)))
(tail-fib 1000)

#|Question 5b
For this function to work, the input needs to be >= 1, so we can
calculate the value from the 1st value forward ->
To do this, I added an initial condition instead of an if
statement, which checks to see if n is within range. If not,
it throws an error.
|#
(define (tail-fib-bounds n)
  (letrec ([helper (lambda (n [acc 0] [result 1] [prev 0])
                     (cond
                       [(<= n 0)
                        (error "Input needs to be >=1")]
                       [(= n acc)
                         result]
                       [else (helper n (+ acc 1) (+ result prev) result)]))])
    (helper n)))
(tail-fib-bounds -1)
