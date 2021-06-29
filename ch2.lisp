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

; Exercise 2.28 - Fringe
(defun fringe (tree)
  (if (null tree)
      nil
      (let ((cartree (car tree))
            (fringe-of-rest (fringe (cdr tree))))
        (if (consp cartree)
            (append (fringe cartree)
                    fringe-of-rest)
            (cons cartree fringe-of-rest)))))

(defun scale-tree (tree factor)
  (mapcar (lambda (subtree)
            (if (consp subtree)
                (scale-tree subtree factor)
                (* subtree factor)))
          tree))


(defun square-tree (tree)
  (mapcar (lambda (subtree)
            (if (consp subtree)
                (square-tree subtree)
                (* subtree subtree)))
          tree))

(defun tree-map (fn tree)
  (mapcar (lambda (sub-tree)
            (if (consp sub-tree)
                (tree-map fn sub-tree)
                (funcall fn sub-tree)))
          tree))

(defun filter (predicate seq)
  (cond ((null seq) nil)
        ((funcall predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        ((filter predicate (cdr seq)))))

(defun accumulate (op acc seq)
  (if (null seq)
      acc
      (funcall op (car seq)
          (accumulate op
                      acc
                      (cdr seq)))))

(defun enumerate-interval (lo hi)
  (if (> lo hi)
      nil
      (cons lo
            (enumerate-interval (+ lo 1) hi))))

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
        ((not (consp tree)) (list tree))
        ((append
           (enumerate-tree (car tree))
           (enumerate-tree (cdr tree))))))

(defun subsets (s)
 (if (null s)
     (list nil)
     (let ((rest (subsets (cdr s))))
       (append rest (mapcar (lambda (ls)
                               (cons (car s) ls))
                               rest)))))

(defun xmap (p seq)
  (accumulate (lambda (x y)
                (cons x (funcall p y)))
              nil
              seq))

(defun xappend (seq1 seq2)
  (accumulate #'cons
              seq2
              seq1))

(defun xlength (seq)
  (accumulate (lambda (_ x) (+ x 1)) 0 seq))


(defun horner-eval (x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* higher-terms x)))
   0
   coefficient-sequence))

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate op init (mapcar #'car seqs))
            (accumulate-n op init (mapcar #'cdr seqs)))))

(defun dot-* (v w)
  (accumulate #'+ 0 (mapcar #'* v w)))


; Skipping some exercises that I have done prior but
; did not commit on this repo

(defun flatmap (proc seq)
  (accumulate #'append nil (mapcar proc seq)))

; 2.40

(defun unique-pairs (n)
  (flatmap (lambda (i)
            (mapcar (lambda (j) (list i j))
                    (enumerate-interval 1 (- i 1))))
          (enumerate-interval 1 n)))


(defun unique-triples (n)
  (filter (lambda (i) (not (null i))) 
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                               (mapcar (lambda (k) (list i j k))
                                       (enumerate-interval 1 (- j 1))))
                             (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n))))

(defun triples-with-sum (sum n)
  (filter (lambda (xs)
            (= (accumulate #'+ 0 xs) sum))
          (unique-triples n)))
