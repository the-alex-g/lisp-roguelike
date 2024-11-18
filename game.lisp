(defparameter *actors* '())
(defparameter *board-size* 9)

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
    (push new-actor *actors*)
    new-actor))

(defparameter *player* (make-actor :pos '(4 . 4)
				   :display-char #\@))
(make-actor :pos '(6 . 5)
	    :display-char #\C)

(defun add-pos (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defmethod move ((obj actor) distance)
  (let ((newpos (add-pos (pos obj) distance)))
    (unless (loop for actor in *actors*
		  unless (equal obj actor)
		    when (equal newpos (pos actor))
		      return t)
      (when (< (car newpos) 0)
	(setf (car newpos) 0))
      (when (> (car newpos) (- *board-size* 1))
	(setf (car newpos) (- *board-size* 1)))
      (when (< (cdr newpos) 0)
	(setf (cdr newpos) 0))
      (when (> (cdr newpos) (- *board-size* 1))
	(setf (cdr newpos) (- *board-size* 1)))
      (setf (pos obj) newpos))))

(defun print-board ()
  (let ((actor-chars (make-hash-table :test 'equal)))
    (loop for actor in *actors*
	  do (setf (gethash (pos actor) actor-chars) (display-char actor)))
    (let ((board (loop for y below *board-size*
		       collect (loop for x below *board-size*
				     collect (let ((c (gethash (cons x y)
							       actor-chars)))
					       (if c c #\.)))))
	  (hline (format nil "~v,,,'-a~%" (+ (* *board-size* 3) 2) "")))
      (format t "~a~{|~{ ~c ~}|~%~}~a"
	      hline
	      board
	      hline))))

(defun game ()
  (print-board)
  (let ((cmd (read-line)))
    (cond ((equal cmd "a")
	   (move *player* '(-1 . 0)))
	  ((equal cmd "d")
	   (move *player* '(1 . 0)))
	  ((equal cmd "w")
	   (move *player* '(0 . -1)))
	  ((equal cmd "s")
	   (move *player* '(0 . 1))))
    (unless (equal cmd "quit")
      (game))))

(game)
