(load "./utils.lisp")

(defparameter *board-size* '(40 . 20))
(defparameter *section-size* 10)

(defun mul-pos (pos scalar)
  (cons (round (* (car pos) scalar)) (round (* (cdr pos) scalar))))

(defun div-pos (pos scalar)
  (mul-pos pos (/ 1 scalar)))

(defun generate-board ()
  (let ((board '())
	(slices (div-pos *board-size* *section-size*))
	(room-positions (make-hash-table :test 'equal)))
    (loop for chamber-x below (car slices)
	  do  (loop for chamber-y below (cdr slices)
		    do (let* ((size (cons (+ 3 (random (- *section-size*
							  3)))
					  (+ 3 (random (- *section-size*
							  3)))))
			      (offset (add-pos (cons (random (- *section-size*
								(car size)))
						     (random (- *section-size*
								(cdr size))))
					       (mul-pos (cons chamber-x
							      chamber-y)
							*section-size*))))
			 (setf (gethash (cons chamber-x chamber-y)
					room-positions)
			       (add-pos offset (mul-pos size 0.5)))
			 (loop for x below (car size)
			       do (loop for y below (cdr size)
					do (push (add-pos (cons x y)
							  offset)
						 board))))))
    (loop for chamber-x below (car slices)
	  do (loop for chamber-y below (cdr slices)
		   do (let* ((room-pos (gethash (cons chamber-x chamber-y)
						room-positions))
			     (lower-room (gethash (add-pos
						   (cons chamber-x chamber-y)
						   '(0 . 1))
						  room-positions))
			     (right-room (gethash (add-pos
						   (cons chamber-x chamber-y)
						   '(1 . 0))
						  room-positions)))
			(when lower-room
			  (loop for x below (+ (random 2) 1)
				do (loop for y below (cdr (sub-pos lower-room
								   room-pos))
					 do (let ((pos (add-pos room-pos
								(cons x y))))
					      (unless (member pos board
							      :test 'equal)
						(push pos board))))))
			(when right-room
			  (loop for y below (+ (random 2) 1)
				do (loop for x below (car (sub-pos right-room
								   room-pos))
					 do (let ((pos (add-pos room-pos
								(cons x y))))
					      (unless (member pos board
							      :test 'equal)
						(push pos board)))))))))
    board))
