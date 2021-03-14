(defun old-make-rat (n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; Exercise 2.1: A version of the [make-rat] function
; That handles both positive and negative args

(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (< d 0))
           (cons (/ (- n) g)
                 (/ (- d) g)))
          ((or (< n 0) (< d 0))
           (cons (/ (- (abs n)) g)
                 (/ (abs d) g)))
          ((cons (/ n g) (/ d g))))))


(defun numer (x)(car x))
(defun denom (x)(cdr x))

(defun print-rat (x)
  (write-char #\Newline)
  (write (numer x))
  (write-string "/")
  (write (denom x)))

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom y)))
            (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Exercise 2.2

(defun make-point (x y)
  (cons x y))

(defun x-point (pt) (car pt))
(defun y-point (pt) (cdr pt))

(defun make-segment (p1 p2) (cons p1 p2))
(defun start-segment (seg) (car seg))
(defun end-segment (seg) (cdr seg))

(defun print-point (p)
  (write-char #\linefeed)
  (write-string "(")
  (write (x-point p))
  (write-string ",")
  (write (y-point p))
  (write-string ")"))

(defun avg (x y) (/ (+ x y) 2))

(defun mid-point (s)
  (let ((start (start-segment s))
        (end   (end-segment   s)))
    (make-point (avg (x-point start) (x-point end))
                (avg (y-point start) (y-point end)))))

; Exercise 2.2
(defun make-rect (tl w h)
  (cons tl (cons w h)))

(defun w-rect (r)
  (car (cdr r)))

(defun h-rect (r)
  (cdr (cdr r)))

(defun tl-rect (r)
  (car r))

(defun per-rect (r)
  (* 2 (+ (h-rect r) (w-rect r))))

; Exercise 2.4
(defun my-cons (x y)
  (lambda (m) (funcall m x y)))

(defun my-car (z)
  (funcall z (lambda (p _) p)))

(defun my-cdr (z)
  (funcall z (lambda (_ q) q)))

(defun make-%-interval (base err%)
  (let ((err (* (/ err% 100) base)))
    (cons (- base err) (+ base err))))

(defun make-interval (lo hi)
  (cons lo hi))

(defun lower-bound (x)
  (car x))

(defun upper-bound (x)
  (cdr x))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(defun div-interval (x y)
  (let ((ux (upper-bound x))
        (ly (lower-bound y)))

    (if (or (= ux 0) (= ly 0))
        (error "Cannot divide by zero")
        (mul-interval x
                  (make-interval
                    (/ 1.0 ux)
                    (/ 1.0 ly))))))

(defun sub-interval (x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(defun par1 (r1 r2)
  (div-interval
    (mul-interval r1 r2)
    (add-interval r1 r2)))

(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(defun list-ref (l n)
  (if (= n 0)
      (car l)
      (list-ref (cdr l)
                (- n 1))))

(defun len (items)
  (defun len-iter (items n)
    (if (null items)
        n
        (len-iter (cdr items)
                  (+ n 1))))
  (len-iter items 0))

(defun .append (list-1 list-2)
  (if (null list-1)
      list-2
      (cons (car list-1)
            (.append (cdr list-1)
                     list-2))))
; Exercise 2.17
(defun last-pair (l)
  (if (null (cdr l))
      l
      (last-pair (cdr l))))

; Exercise 2.18
(defun rev (l)
  (defun .rev (curr prev)
    (if (null curr)
        prev
        (.rev (cdr curr)
              (cons (car curr) prev))))
  (.rev l nil))

; Exercise 2.20
(defun same-parity (a &rest b)
  (defun .parity (curr)
    (cond ((null curr) nil)
          ((= (mod a 2)
              (mod (car curr) 2))
           (cons (car curr)
                 (.parity (cdr curr))))
          ((.parity (cdr curr)))))
  (cons a (.parity b)))


; Exercise 2.21
(defun square-list (items)
  (mapcar (lambda (x) (* x x)) items))

; Exercise 2.23
(defun for-each (f l)
  (if (null l)
    T
    (prog2
     (funcall f (car l))
     (for-each f (cdr l)))))

(defun count-leaves (tree)
  (cond ((null tree) 0)
        ((not (consp tree)) 1)
        ((+ (count-leaves (car tree))
            (count-leaves (cdr tree))))))


(defun deep-rev (l)
  (defun deep-rev-rec (prev curr)
    (if (null curr)
        prev
        (let ((carcurr (if (consp    (car curr))
                           (deep-rev (car curr))
                           (car curr))))
          (deep-rev-rec (cons carcurr
                              prev)
                        (cdr curr)))))
  (deep-rev-rec nil l))
