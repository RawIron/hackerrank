;;;


(defun zip (lst1 lst2)
  (mapcar #'list lst1 lst2))


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
(defun calc-area-volume (range polynom)
  (let ((y 0.0d0)
        (step 0.001d0)
        (area 0.0d0)
        (volume 0.0d0))
    (do ((x (+ (first range) step) (+ x step)))
        ((> x (second range)) 'done)
      (setf y (calc-polynom x polynom))
      (setf area (+ area (* step y)))
      (setf volume (+ volume (* step (* pi (expt y 2))))))
    (list area volume)))


(defun main ()
  (let* ((coef (input))
        (expo (input))
        (range (input-pair))
        (result (calc-area-volume range (zip coef expo))))
    (format t "~D~%" (first result))
    (format t "~D~%" (second result))))

(main)
