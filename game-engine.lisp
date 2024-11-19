(defparameter *actors* '())
(defparameter *board* '())
(defparameter *board-size* '(15 . 9))
(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))

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

(defun square (number)
  (* number number))

(defun add-pos (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defun distance (p1 p2)
  (sqrt (+ (square (- (car p2) (car p1)))
	   (square (- (cdr p2) (cdr p1))))))

(defmethod move ((obj actor) distance)
  (let ((newpos (add-pos (pos obj) distance)))
    (unless (or (loop for actor in *actors*
		  unless (equal obj actor)
		    when (equal newpos (pos actor))
		      return t)
		(not (member newpos *board* :test 'equal)))
      (setf (pos obj) newpos))))

(defun print-board (board)
  (let ((actor-chars (make-hash-table :test 'equal)))
    (loop for actor in *actors*
	  do (setf (gethash (pos actor) actor-chars) (display-char actor)))
    (labels ((on-board-p (pos) (member pos board :test 'equal))
	     (get-char (pos)
	       (if (on-board-p pos)
		   (let ((c (gethash pos actor-chars)))
		     (if c c #\.))
		   (cond ((or (on-board-p (add-pos pos +left+))
			      (on-board-p (add-pos pos +right+)))
			  #\|)
			 ((or (on-board-p (add-pos pos +up+))
			      (on-board-p (add-pos pos +down+)))
			  #\-)
			 (t #\space)))))
      (loop for y from -1 to (+ (cdr *board-size*) 1)
	    do (progn (loop for x from -1 to (+ (car *board-size*) 1)
			    do (princ (get-char (cons x y))))
		      (fresh-line))))))

(defmethod input (text))

(defun game-loop ()
  (print-board *board*)
  (let ((cmd (read-line)))
    (unless (equal cmd "quit")
      (input cmd)
      (game-loop))))

(defun start ()
  (game-loop))
