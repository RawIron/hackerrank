;;; is a list of (x,y) pairs taken from a function


;;  read a value
(defun input1 ()
  (read))

;;  read a pair
(defun input2 ()
  (list (read) (read)))

;;  read a list of pairs
(defun input (n)
  (let ( (pairs '()) )
    (do ( (i 1 (+ i 1)) )
        ( (> i n) 'done )
      ; append concats two lists!
      ; take a list and concat with a list of pairs
      (setf pairs (append pairs (list (input2)))))
    pairs))

;;  print elements of any list
(defun output (lst)
  (dolist (elem lst)
    (format t "~A~%" elem)))


;;  (x,y) pairs are unique
;;  x values must be unique
;;  does not assume (x,y) pairs are sorted
(defun func-valuesp (pairs)
  (let ((xs (make-hash-table)))
    (dolist (p pairs)
      (if (not (gethash (first p) xs))
        (setf (gethash (first p) xs) 1)
        (return-from func-valuesp nil)))
    t))


(defun main ()
  (let ((tests (input1)))
    (do ((i 1 (+ i 1)))
        ((> i tests) 'done)
      (let* ((n (input1))
             (pairs (input n)))
        (if (func-valuesp pairs)
          (format t "YES~%")
          (format t "NO~%"))))))

(main)

