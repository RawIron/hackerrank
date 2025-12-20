;;; Example
;;
;;  p(x) = 2x + 1, [2,20]
;;
;;  area = rectangle + triangle
;;       = (5 * 18) + (36 * 18 * 0.5)
;;       = 414
;;
;;  volume = conical cylinder
;;              height 18
;;              larger base radius  R 41
;;              smaller base radius r 5
;;         = 1/3 pi * 18 * (41^2 + 41 * 5 + 5^2)
;;         = 36021.5
;;
;;  relative error = (exact - approximate) / exact
;;                 = (36021 - 36018) / 36021
;;                 = 0.00008
;;                 < 0.01
;;
;;        hackerrank expected output
;;          36024.1 with a relative error 0.01 ????


(defun zip (lst1 lst2)
  (mapcar #'list lst1 lst2))


(defun midpoint (a b)
  "return the arithmetic midpoint of a and b"
  (/ (+ a b) 2))


;;  "the words are here" == (list "the" "words" "are" "here")
(defun split-string-to-words (str)
  (with-input-from-string (stream str)
    (loop
      :for word = (handler-case 
                    (read stream nil nil)
                    (end-of-file () nil))
      :while word
      :collect word)))


(defun output (lst)
  "print elements of any list"
  (dolist (elem lst)
    (format t "~A~%" elem)))

(defun input-pair ()
  "parse 2 objects with read from *stdin*"
  (list (read) (read)))

(defun input ()
  (split-string-to-words (read-line)))


(defun cylinder_volume (radius height)
  "calculate volume of a cylinder"
  (* height (* pi (expt radius 2))))

(defun rectangle_area (width height)
  "calculate area of a rectangle"
  (* width height))


;;  calculate y value of a polynom
;;
;;  polynom described by a list of pairs
;;  pair is of the form
;;    (coefficient exponent)
;;
;;  (2 ((2 1) (1 2))) == 8
;;       2x  + x^2
(defun calc-polynom (x p)
  "evaluate a polynom at x"
  (let ((y 0.0d0))
    (dolist (term p)
      (let ((coef (first term))
            (expo (second term)))
      (setf y (+ y (* coef (expt x expo))))))
    y))


;;  approximate area and volume
;;  for a polynomial function
;;  over the interval [a,b]
;;  using Obersumme
(defun calc-area-volume (range polynom)
  (let* ((a (first range))
        (b (second range))
        (step 0.001d0)
        (prev-y (calc-polynom a polynom))
        (area 0.0d0)
        (volume 0.0d0))
    (do ((x (+ a step) (+ x step)))
        ((> x b) 'done)
      (let* ((y (calc-polynom x polynom))
            (pick #'max)
            (rectangle (rectangle_area step (funcall pick prev-y y)))
            (cylinder (cylinder_volume (funcall pick prev-y y) step)))
        (setf area (+ area rectangle))
        (setf volume (+ volume cylinder))
        (setf prev-y y)))
    (values area volume)))


(defun main ()
  (let* ((coef (input))
        (expo (input))
        (range (input-pair)))
    (multiple-value-bind (area volume) 
        (calc-area-volume range (zip coef expo))
      (format t "~D~%" area)
      (format t "~D~%" volume))))

(main)
