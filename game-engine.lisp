(defparameter *actors* '())
(defparameter *dynamic-actors* '())
(defparameter *player-actions* 0)
(setf *actors* '()) ;; not sure why this is necessary
(defparameter *board* (make-hash-table :test 'equal))
(defparameter *board-size* '(15 . 9))
(defparameter *sight-distance* 3)
(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))
(defparameter *actions* (make-hash-table :test 'equal))

(defmacro defaction (key &body body)
  `(setf (gethash ,key *actions*) (lambda () (incf *player-actions*) ,@body)))

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
    :initform t
    :initarg :solid
    :accessor solid)))

(defclass enemy (actor)
  ((spd ;; speed of 1 is the same as the player
        ;; speed of 2 is half as fast as the player
    :initform 1.2
    :initarg :spd
    :accessor spd)
   (enabled
    :initform nil
    :accessor enabled)))

(defun make-actor (name display-char pos &key (solid t))
  (let ((new-actor (make-instance 'actor :pos pos
					 :display-char display-char
					 :name name
					 :solid solid)))
    (push new-actor *actors*)
    new-actor))

(defun make-enemy (name display-char pos &key (spd 1.2))
  (let ((new-enemy (make-instance 'enemy :pos pos
				         :display-char display-char
					 :name name
					 :spd spd)))
    (push new-enemy *actors*)
    (push new-enemy *dynamic-actors*)
    new-enemy))

(defparameter *player* (make-actor "player" #\@ '(4 . 4)))

(defun square (number)
  (* number number))

(defun add-pos (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defun sub-pos (a b)
  (cons (- (car a) (car b)) (- (cdr a) (cdr b))))

(defun distance (p1 p2)
  (sqrt (+ (square (- (car p2) (car p1)))
	   (square (- (cdr p2) (cdr p1))))))

(defmethod interact ((obj actor))
  (princ (concatenate 'string "You interacted with a " (name obj)))
  (fresh-line))

(defun find-actor-at (actor &key pos)
  (unless pos
    (setf pos (pos actor)))
  (loop for a2 in *actors*
	unless (equal a2 actor)
	  when (equal pos (pos a2))
	    return a2))

(defmethod move ((obj actor) distance)
  (let* ((newpos (add-pos (pos obj) distance))
	 (collider (find-actor-at obj :pos newpos)))
    (if (and collider (solid collider))
	(interact collider)
	(when (gethash newpos *board*)
	  (setf (pos obj) newpos)))))

(defun find-path (from to) ;; using breadth-first search
  ;; TODO: implement early exit
  ;; TODO: avoid other solid characters
  (let ((came-from (make-hash-table :test 'equal)))
    (setf (gethash from came-from) t)
    (labels ((neighbors (pos)
	       (loop for direction in (list +left+ +right+ +up+ +down+)
		     collect (if (gethash (add-pos pos direction)
					  *board*)
				 (add-pos pos direction)
				 nil)))
	     (iterate (frontier)
	       (let ((current (car frontier)))
		 (when current
		   (loop for neighbor in (neighbors current)
			 when neighbor
			   do (unless (gethash neighbor came-from)
				(setf (gethash neighbor came-from)
				      current)
				(setf frontier
				      (append frontier
					      (list neighbor)))))
		   (iterate (cdr frontier)))))
	     (get-path (path pos)
	       (if (equal pos from)
		   path
		   (get-path (cons pos path) (gethash pos came-from)))))
      (iterate (list from))
      (get-path '() to))))

(defun step-towards (to from)
  (sub-pos (car (find-path (pos from) (pos to))) (pos from)))

(defmethod update ((obj enemy))
  (when (< (mod *player-actions* (spd obj)) 1)
    (when (or (<= (distance (pos obj) (pos *player*)) *sight-distance*)
	      (= *sight-distance* -1))
      (setf (enabled obj) t))
    (when (enabled obj)
      (move obj (step-towards *player* obj)))))

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

(defmethod input (cmd)
  (let ((action (gethash cmd *actions*)))
    (when action
      (funcall action))))

(defun game-loop ()
  (print-board)
  (let ((cmd (read-line)))
    (unless (equal cmd "quit")
      (input cmd)
      (loop for actor in *dynamic-actors*
	    do (update actor))
      (game-loop))))

(defun start ()
  (game-loop))

(defaction "a" (move *player* +left+))
(defaction "d" (move *player* +right+))
(defaction "w" (move *player* +up+))
(defaction "s" (move *player* +down+))
(defaction "i" (let ((actor (find-actor-at *player*)))
		 (when actor
		   (interact actor))))
