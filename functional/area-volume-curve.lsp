;;;


(defun zip (lst1 lst2)
  (mapcar #'list lst1 lst2))


(defun midpoint (a b)
  "return the arithmetic midpoint of a and b"
  (/ (+ a b) 2))


(defun split-string-to-words (str)
  (with-input-from-string (stream str)
    (loop
      :for word = (handler-case 
                    (read stream nil nil)
                    (end-of-file () nil))
      :while word
      :collect word)))


;;  print elements of any list
(defun output (lst)
  (dolist (elem lst)
    (format t "~A~%" elem)))

(defun input-pair ()
  (list (read) (read)))

(defun input ()
  (split-string-to-words (read-line)))


;;  calculate volume of a cylinder
(defun cylinder_volume (radius height)
  (* height (* pi (expt radius 2))))

;;  calculate area of a rectangle
(defun rectangle_area (width height)
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
  (let* ((y 0.0d0)
        (a (first range))
        (b (second range))
        (prev-y (calc-polynom a polynom))
        (step 0.001d0)
        (area 0.0d0)
        (volume 0.0d0))
    (do ((x (+ a step) (+ x step)))
        ((> x b) 'done)
      (setf y (calc-polynom x polynom))
      (setf area (+ area (rectangle_area step (max prev-y y))))
      (setf volume (+ volume (cylinder_volume (max prev-y y) step)))
      (setf prev-y y))
    (list area volume)))


(defun main ()
  (let* ((coef (input))
        (expo (input))
        (range (input-pair))
        (result (calc-area-volume range (zip coef expo))))
    (format t "~D~%" (first result))
    (format t "~D~%" (second result))))

(main)
