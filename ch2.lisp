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

(defun make-rect (tl w h)
  (cons tl (cons w h)))


(defun w-rect (r)
  (car (cdr r)))

(defun h-rect (r)
  (cdr (cdr r)))

(defun tl-rect (r)
  (car r))












