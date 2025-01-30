(defun randir ()
  (nth (random 4) +directions+))

;;; use random walk algorithim to generate a cave of given cell count
(defun generate-cave (size)
  (labels ((walk (pos board board-size)
	     (if (= size board-size)
		 board
		 (let ((newpos (add-pos pos (randir))))
		   (if (member newpos board :test #'equal)
		       (walk newpos board board-size)
		       (walk newpos (cons newpos board) (1+ board-size)))))))
    (let* ((cave (walk +zero+ nil 0))
	   (cave-top-left (loop for pos in cave
				minimize (car pos) into min-x
				minimize (cdr pos) into min-y
				finally (return (cons min-x min-y)))))
      ;; normalize the cave so that the top-left corner is at (0, 0)
      (loop for pos in cave
	    collect (sub-pos pos cave-top-left)))))

(defun main (size)
  (let* ((board (generate-cave size))
	 (board-size (loop for pos in board
			   maximize (car pos) into max-x
			   maximize (cdr pos) into max-y
			   finally (return (cons max-x max-y)))))
    (format t "~{~{~c~}~%~}"
	    (loop for y below (1+ (cdr board-size))
		  collect (loop for x below (1+ (car board-size))
				collect (if (member (cons x y) board :test #'equal)
					    #\#
					    #\space))))
    (unless (eq (read-char) #\q)
      (format t "~20~~%")
      (main size))))
