(load "./utils.lisp")

(defclass dungeon ()
  ((board :initform '())
   (actors :initform (make-hash-table :test 'equal))))

(defun randval (v)
  (if (<= v 1)
      0
      (random (round v))))

(defun populate (region functions board)
  (let ((priority (random 3)))
    (loop for pos in region
	  when (= (random 8) 0)
	    do (setf (gethash pos (slot-value board 'actors))
		     (let ((r (random 6)))
		       (cond ((<= r 3)
			      (car (eval-weighted-list
				    (nth priority functions))))
			     ((= r 4)
			      (car (eval-weighted-list
				    (nth (mod (1+ priority) 3) functions))))
			     ((= r 5)
			      (car (eval-weighted-list
				    (nth (mod (1- priority) 3) functions))))))))
    region))

(defun partial-fill (offset size)
  (let* ((fill-size (cons (+ 3 (randval (- (car size) 4)))
			  (+ 3 (randval (- (cdr size) 4)))))
	 (fill-offset (cons (round (+ 1 (car offset)
				      (randval (- (car size) (car fill-size) 1))))
			    (round (+ 1 (cdr offset)
				      (randval (- (cdr size) (cdr fill-size) 1)))))))
    (apply #'append
	   (loop for y below (cdr fill-size)
		 collect (loop for x below (car fill-size)
			       collect (cons (+ (car fill-offset) x)
					     (+ (cdr fill-offset) y)))))))

(defun connect (r1 r2)
  (let ((region (append r1 r2)))
    (when (and r1 r2)
      (let* ((cp1 (randnth r1))
	     (px (loop for point in r2
		      with min-x = 1000
		      when (< (abs (- (car cp1) (car point))) min-x)
			do (setf min-x (car point))
		      finally (return min-x)))
	     (py (loop for point in r2
		       with min-y = 1000
		       when (and (eq (car point) px)
				 (< (abs (- (cdr cp1) (cdr point))) min-y))
			 do (setf min-y (cdr point))
		      finally (return min-y)))
	     (dx (- px (car cp1)))
	     (dy (- py (cdr cp1)))
	     (cp2 (cons px py)))
	(unless (eq (cdr cp1) (cdr cp2))
	  (loop for y from 0 to (abs dy)
		do (push (cons (car cp1)
			       (if (< dy 0)
				   (- (cdr cp1) y)
				   (+ (cdr cp1) y)))
			 region)
		finally (setf cp1 (cons (car cp1) (cdr cp2)))))
	(unless (eq (car cp1) (car cp2))
	  (loop for x from 0 to (abs dx)
		do (push (cons (if (< dx 0)
				   (- (car cp1) x)
				   (+ (car cp1) x))
			       (cdr cp1))
			 region)))))
    region))

(defun vary-from (val)
  (if (<= val 8)
      4
      (+ val (round (* (1- (random 2.0))
		       (/ val 2))))))

(defun partition (offset size max-depth functions board &optional (cur-depth 0))
  (if (< cur-depth max-depth)
      (let* ((split-direction (cond ((> 8 (car size)) 'h)
				    ((> 8 (cdr size)) 'v)
				    (t (if (= (random 2) 0) 'h 'v))))
	     (split (if (eq split-direction 'h)
			(vary-from (/ (cdr size) 2))
			(vary-from (/ (car size) 2)))))
	(if (eq split-direction 'v)
	    (connect (partition offset
				(cons split (cdr size))
				max-depth functions board (1+ cur-depth))
		     (partition (cons (+ (car offset) split) (cdr offset))
				(cons (- (car size) split) (cdr size))
				 max-depth functions board (1+ cur-depth)))
	    (connect (partition offset
				(cons (car size) split)
				max-depth functions board (1+ cur-depth))
		     (partition (cons (car offset) (+ (cdr offset) split))
				(cons (car size) (- (cdr size) split))
				max-depth functions board (1+ cur-depth)))))
      (populate (partial-fill offset size) functions board)))

(defun generate-dungeon (size depth functions)
  (let ((dungeon (make-instance 'dungeon)))
    (setf (slot-value dungeon 'board)
	  (partition '(0 . 0) size depth functions dungeon))
    dungeon))

;; a function for testing the boards
(defun main (size depth)
  (let ((board (generate-dungeon size depth ())))
    (format t "~{~{~c~}~%~}"
	    (loop for y below (cdr size)
		  collect (loop for x below (car size)
				collect (if (member (cons x y)
						    board
						    :test 'equal)
					    #\#
					    #\space)))))
  (unless (eq (read-char) #\q)
    (format t "-------------------------------------------------~%")
    (main size depth)))
