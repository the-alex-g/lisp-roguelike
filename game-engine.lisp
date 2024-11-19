(defparameter *actors* '())
(setf *actors* '()) ;; not sure why this is necessary
(defparameter *board* (make-hash-table :test 'equal))
(defparameter *board-size* '(15 . 9))
(defparameter *sight-distance* 4)
(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))

(defclass actor ()
  ((pos
    :initarg :pos
    :accessor pos)
   (display-char
    :initarg :display-char
    :accessor display-char)
   (name
    :initarg :name
    :accessor name)
   (solid
    :initarg :solid
    :accessor solid)))

(defun make-actor (name display-char pos &key (solid t))
  (let ((new-actor (make-instance 'actor :pos pos
					 :display-char display-char
					 :name name
					 :solid solid)))
    (push new-actor *actors*)
    new-actor))

(defparameter *player* (make-actor "player" #\@ '(4 . 4)))

(defun square (number)
  (* number number))

(defun add-pos (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defun distance (p1 p2)
  (sqrt (+ (square (- (car p2) (car p1)))
	   (square (- (cdr p2) (cdr p1))))))

(defmethod interact ((obj actor))
  (princ (concatenate 'string "You interacted with a " (name obj)))
  (fresh-line))

(defmethod move ((obj actor) distance)
  (let* ((newpos (add-pos (pos obj) distance))
	 (collider (loop for actor in *actors*
		      unless (equal obj actor)
			when (and (equal newpos (pos actor))
				  (solid actor))
			  return actor)))
    (if collider
	(interact collider)
	(when (gethash newpos *board*)
	  (setf (pos obj) newpos)))))

(defun print-board ()
  (let ((actor-chars (make-hash-table :test 'equal)))
    (loop for actor in *actors*
	  do (setf (gethash (pos actor) actor-chars) (display-char actor)))
    (labels ((on-board (pos) (gethash pos *board*))
	     (foundp (pos) (eq (on-board pos) 'found))
	     (get-char (pos)
	       (if (on-board pos) ;; is the cell on the board?
		   ;; if so, check if it is in sight OR *sight-distance* is -1
		   (if (or (<= (distance (pos *player*) pos) *sight-distance*)
			   (= *sight-distance* -1))
		       ;; if it is, populate it
		       (progn (setf (gethash pos *board*) 'found)
			      (let ((c (gethash pos actor-chars)))
				(if c c #\.)))
		       ;; otherwise, return an empty space
		       #\space)
		   ;; if the cell is not on the board,
		   ;; check all adjacent cells and add walls
		   ;; as necessary.
		   (cond ((or (foundp (add-pos pos +left+))
			      (foundp (add-pos pos +right+)))
			  #\|) ;; vertical wall
			 ((or (foundp (add-pos pos +up+))
			      (foundp (add-pos pos +down+)))
			  #\-) ;; horizontal wall
			 (t #\space))))) ;; nothing
      ;; print the board
      (loop for y from -1 to (+ (cdr *board-size*) 1)
	    do (progn (loop for x from -1 to (+ (car *board-size*) 1)
			    do (princ (get-char (cons x y))))
		      (fresh-line))))))

(defmethod input (text))

(defun game-loop ()
  (print-board)
  (let ((cmd (read-line)))
    (unless (equal cmd "quit")
      (input cmd)
      (game-loop))))

(defun start ()
  (game-loop))
