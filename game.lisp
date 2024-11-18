(defparameter *actors* '())
(defparameter *board-size* '(15 . 9))
(defparameter *board* '())
(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))

;; generate a sample board
(loop for x below (car *board-size*)
      do (loop for y below (cdr *board-size*)
	       when (or (< x 7) (> x 8) (= y 4))
		 do (push (cons x y) *board*)))

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

(defun get-char-at (pos actor-chars)
  (flet ((board-member (at) (member at *board* :test 'equal)))
    (if (board-member pos)
	(let ((c (gethash pos actor-chars)))
	  (if c c #\.))
	(cond ((or (board-member (add-pos pos +left+))
		   (board-member (add-pos pos +right+)))
	       #\|)
	      ((or (board-member (add-pos pos +up+))
		   (board-member (add-pos pos +down+)))
	       #\-)
	      (t #\space)))))

(defun print-board ()
  (let ((actor-chars (make-hash-table :test 'equal)))
    (loop for actor in *actors*
	  do (setf (gethash (pos actor) actor-chars) (display-char actor)))
    (let ((board (loop for y from -1 to (+ (cdr *board-size*) 1)
		       collect (loop for x from -1 to (+ (car *board-size*) 1)
				     collect (get-char-at (cons x y)
							  actor-chars)))))
      (format t "~{~{~c~}~&~}" board))))

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
