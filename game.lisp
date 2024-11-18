(defvar *actors* '())

(defclass actor ()
  ((pos
    :initform '(4 . 4)
    :initarg :pos
    :accessor pos)
   (display-char
    :initform #\C
    :initarg :display-char
    :accessor display-char)))

(defun make-actor (&key pos display-char)
  (let ((new-actor (make-instance 'actor :pos pos :display-char display-char)))
    (push new-actor *actors)
    new-actor))

(defvar player (make-actor :pos '(4 . 4)
			   :display-char #\@))

(defmethod move ((obj actor) distance)
  (incf (car (pos obj)) (car distance))
  (incf (cdr (pos obj)) (cdr distance)))

(defun print-board ()
  (let ((actor-chars (make-hash-table)))
    (loop for actor in *actors*
	  do (setf (gethash (pos actor) actor-chars) (display-char actor)))
    (let ((board (loop for y below 9
		       collect (loop for x below 9
				     collect (let ((c (gethash (cons x y)
							       actor-chars)))
					       (if c
						   c
						   #\.))))))
      (format t "~29,,,'-a~%~{|~{ ~c ~}|~%~}~29,,,'-a~%"
	      ""
	      board
	      ""))))

(defun game ()
  (print-board)
  (let ((cmd (read-line)))
    (cond ((equal cmd "a")
	   (move player '(-1 . 0)))
	  ((equal cmd "d")
	   (move player '(1 . 0)))
	  ((equal cmd "w")
	   (move player '(0 . -1)))
	  ((equal cmd "s")
	   (move player '(0 . 1))))
    (unless (equal cmd "quit")
      (game))))
      
(game)
