; SICP exercise solutions,
; and some random lisp snippets
; in between

(defun inc (n)
  (+ n 1))

(defun dec (n)
  (- n 1))

(defun average (x y)
  (/ (+ x y) 2))

(defun even? (n)
  (= (rem n 2) 0))

(defun divides? (a b)
  (= (rem b a) 0))

(defun square (n) (* n n))

(defun cube (n) (* n n n))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ((find-divisor n
                       (next test-divisor)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun prime? (n)
  (= (smallest-divisor n) n))

(defun expmod (base power n)
  (cond ((= power 0) 1)
        ((even? power)
         (rem (square 
                (expmod base
                        (/ power 2)
                        n))
              n))
        ((rem (* base
                 (expmod base
                        (- power 1)
                        n))
              n))))

(defun fermat-test (n)
 (defun try (a)
   (= (expmod a n n) a))
 (try (+ 1 (random (- n 1)))))

(defun fast-prime (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime n
                                     (- times 1)))
        (nil)))

; exercise 1.21 - 199, 1999, 7

; common lisp has a different
; builtin for time apparently...
(defun runtime ()
  (get-internal-run-time))

(defun timed-prime-test (n)
  (fresh-line)
  (write n)
  (start-prime-test n (runtime)))

(defun start-prime-test (n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))

(defun report-prime (elapsed)
  (write "***")
  (write elapsed))

; Exercise 1.22
; This function takes two arguments, `start` and `end`,
; and tests all odd number in the range [start, end]
; for primality, printing the time taken
; for each test.
(defun search-for-primes (start end)
  (cond ((> start end) (write-line "done"))
        ((even? start) 
         (search-for-primes (+ 1 start)
                            end))
        (t 
         (timed-prime-test start)
         (search-for-primes (+ 2 start)
                            end))))

; Exercise 1.23
; The objective to stop checking redundant
; numbers in the [smallest-divisor] test.

(defun next (n)
  (if (= n 2) 
      3 
      (+ 2 n)))
; 1.25
; Some Carmichael numbers: 
; 561, 1105, 1729, 2465, 2821, 6601

; this function is supposed to prove
; that the fermat test is not effective
; for Carmichael numbers. It returns true 
; if a^n is congruent to a modulo n for 
; every a < n.
(defun f1-25 (n)
  (defun try (a)
    (= (expmod a n n) (rem a n)))
  (defun test (counter)
    (cond ((= n counter) T)
          ((not (try counter)) NIL)
          ((test (+ counter 1)))))  
  (test 2))

; section 1.3 

(defun id (x) x)

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))

(defun integral (f a b dx)
  (defun add-dx (x)
    (+ dx x))
  (* (sum f (+ a (/ dx 2.0)) #'add-dx  b)
     dx))


; exercise 1.29
; Simpson's integral approximation.
; assume n is even

(defun simpson-integral (f a b n)
  (let ((h (/ (- b a) n)))
   (defun y (k)
     (funcall f (+ a (* k h))))
   (defun add-2 (x) (+ x 2))
   (* (/ h 3)
      (+ (y 0)
         (y n)
         (* 2 (sum #'y 
                   2 
                   #'add-2 
                   (- n 2)))
         (* 4 (sum #'y
                   1
                   #'add-2
                   (- n 1)))))))


; Exercise 1.30
; Iterative sum procedure

(defun sum-iter (term a next b)
  (defun iter (a acc)
    (if (> a b) 
        acc 
        (iter (funcall next a) 
              (+ acc (funcall term a)))))
  (iter a 0))

; Exercise 1.31
; Product

(defun product (term a next b)
  (defun iter (a acc)
    (if (> a b)
        acc
        (iter (funcall next a)
              (* acc (funcall term a)))))
  (iter a 1))

(defun fact-1.31 (n)
  (product #'id 1 #'inc n))

; approximating Pi using [product]

(defun approx-pi (n)
  (defun pi-term (k)
    (* (/ (* 2 k)
          (- (* 2 k) 1))
       (/ (* 2 k)
          (+ (* 2 k) 1))))
 (* 2.0 (product #'pi-term 1.0 #'inc n)))


; Exercise 1.32
; Generalizing [product] and [sum] further by 
; writing a function [accumulate], and then 
; writing [product] and [sum] using accumulate.

; procedure generating an iterative process
(defun accumulate (combiner null-val term a next b)
  (defun acc-iter (a res)
    (if (> a b)
        res
        (acc-iter (funcall next a)
                  (funcall combiner
                           (funcall term a)
                           res))))
  (acc-iter a null-val))

; procedure generating a recursive process
(defun accumulate-rec (combiner null-val term a next b)
  (if (> a b)
      null-val
      (funcall combiner (funcall term a) 
               (accumulate-rec combiner
                               null-val
                               term
                               (funcall next a)
                               next
                               b))))

(defun product-acc (term a next b)
  (accumulate #'* 1 term a next b))


(defun sum-acc (term a next b)
  (accumulate #'+ 0 term a next b))

; Exercise 1.33
; Filtered accumulate that only 
; accepts terms that pass a certain
; filter.

(defun filtered-accumulate (combiner base-val term a
                                     next b filter)
  (defun filter-acc-iter (a res)
    (if (> a b)
        res
        (filter-acc-iter (funcall next a)
                         (if (funcall filter a)
                             (funcall combiner
                                      (funcall term a)
                                      res)
                             res))))
  (filter-acc-iter a base-val))

; 1.33 a) Finds out sum of the squares of the numbers
; in the range [a, b] that are prime.

(defun sqr-sum-of-primes (a b)
  (filtered-accumulate #'+
                        0
                        #'square
                        a
                        #'inc
                        b
                        #'prime?))

; 1.33 b> Product of all 
; positive integers less than n that 
; are relatively prime to n
(defun rel-prime-* (n)
  (filtered-accumulate #'*
                       1
                       #'id
                       2
                       #'inc
                       (- n 1)
                       (lambda (x) (= (gcd n x) 1))))


; Section 1.3

(defun avg (x y) (/ (+ x y) 2))

(defun close-enough? (a b)
  (< (abs (- a b)) 0.001))

(defun search-zero (f neg-point pos-point)
  (let ((mid-point (avg neg-point pos-point)))
   (if (close-enough? neg-point pos-point)
       mid-point
       (let ((val-at-mid (funcall f mid-point)))
         (cond ((> val-at-mid 0) (search-zero f neg-point mid-point))
               ((< val-at-mid 0) (search-zero f mid-point pos-point))
               (val-at-mid))))))

(defun half-interval-method (f a b)
  (let ((val-at-a (funcall f a))
        (val-at-b (funcall f b)))
    (cond ((and (< val-at-a 0) (> val-at-b 0))
           (search-zero f a b))
          ((and (> val-at-a 0) (< val-at-b 0))
           (search-zero f b a))
          ((error "Values at points must have opposite sign")))))

(defun fp-close-enough? (x y)
    (< (abs (- x y)) tolerance))

(defvar tolerance 0.0001)
(defun fixed-point (f first-guess)
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (fp-close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(defun average-damp (f)
  (lambda (x) (/ (+ x (funcall f x)) 
                 2)))

; Exercise 1.35: Phi = (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) 

(defun fixed-point-log (f first-guess)
  (defun try (guess)
    (let ((next (funcall f guess)))
      (write guess)
      (write-char #\linefeed)
      (if (fp-close-enough? next guess)
          next
          (try next))))
  (try first-guess))

; Exercise 1.37

(defun cont-frac (n d k)
  (defun cont-frac-iter (i acc)
    (if (= i 1)
        acc
        (cont-frac-iter (- i 1)
                        (/ (funcall n i)
                           (+ (funcall d i)
                              acc)))))
  (cont-frac-iter k 0))


; Exercise 1.39

(defun tan-cf (x k)
  (defun tan-cf-iter (i acc)
    (if (= i 1)
        acc
        (tan-cf-iter (- i 1)
                     (- (- (* 2 i) 1)
                        (/ (square x)
                           acc)))))
  (/ x (tan-cf-iter k 1)))

(defvar dx 0.0001)
(defun deriv (f)
  (lambda (x) (/ (- (funcall f (+ x dx))
                    (funcall f x)) 
                 dx)))

(defun newtons-transform (g)
  (lambda (x) (- x
                 (/ (funcall g x)
                    (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newtons-transform g) guess))


(defun fixed-point-of-transform (transform g guess)
  (fixed-point (funcall transform g) guess))

(defun sqrt-nm (x)
  (newtons-method (lambda (y) (- (square y) x) )
                  1.0))

(defun cubic (a b c)
  (lambda (x) (+ (cube x) 
              (* a (square x)) 
              (* b x) 
              c)))


(defun apply-twice (f)
  (lambda (x) (funcall f (funcall f x))))

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun repeated (f times)
  (if (= times 1)
      f
      (repeated (apply-twice f) (- times 1))))

(defun smooth (f)
  (lambda (x) (/ (+ (funcall f (- x dx))
                    (funcall f x)
                    (funcall f (+ x dx)))
                 3)))



(defun iterative-improve (check improve)
  (defun improve-n-iter (x)
    (if (funcall check x)
        x
        (improve-n-iter (funcall improve x))))
  #'improve-n-iter)

(defun sqrt-e146 (x)
  (funcall (iterative-improve 
             (lambda (guess)
               (< (abs (- (square guess) x))
                   0.001)) 
             (lambda (guess)
               (average guess (/ x guess)))) 
           1.0))

