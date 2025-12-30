;;;

;;  "the words are here" == (list "the" "words" "are" "here")
(defun split-string-to-words (str)
  (with-input-from-string (stream str)
    (loop
      :for word = (handler-case 
                    (read stream nil nil)
                    (end-of-file () nil))
      :while word
      :collect word)))


(defun input-line ()
  (split-string-to-words (read-line)))

(defun input-pair ()
  "parse 2 objects with read from *stdin*"
  (list (read) (read)))

(defun input-pair-list (n)
  "read a list of pairs"
  (let ( (pairs '()) )
    (do ( (i 1 (+ i 1)) )
        ( (> i n) 'done )
      ; append concats two lists!
      ; take a list and concat with a list of pairs
      (setf pairs (append pairs (list (input2)))))
    pairs))

(defun input1 ()
  "read one value"
  (read))


;;  output
(defun output (lst)
  "print elements of any list"
  (dolist (elem lst)
    (format t "~A~%" elem)))
