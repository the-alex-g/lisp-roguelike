(defparameter *actors* '())
(defparameter *dynamic-actors* '())
(defparameter *player-actions* 0)
(setf *actors* '()) ; not sure why this is necessary
(defparameter *board* (make-hash-table :test 'equal))
(defparameter *board-size* '(15 . 9))
(defparameter *sight-distance* 3)
(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))
(defparameter +zero+ '(0 . 0))
(defparameter *actions* (make-hash-table :test 'equal))

(defmacro defaction (key &body body)
  `(setf (gethash ,key *actions*) (lambda () ,@body (incf *player-actions*) t)))

(defclass equipment ()
  ((def :initform 0 :accessor def :initarg :def)
   (dmg :initform 0 :accessor dmg :initarg :dmg)
   (str :initform 0 :accessor str :initarg :str)
   (dex :initform 0 :accessor dex :initarg :dex)
   (health :initform 0 :accessor health :initarg :health)
   (equip-slot :initform 'any :accessor equip-slot :initarg :equip-slot)))

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
    :accessor solid)
   (consumable
    :initform nil
    :initarg :consumable
    :accessor consumable)))

(defclass combat-entity (actor)
  ((def :initform 0 :initarg :def)
   (dmg :initform 0 :initarg :dmg)
   (str :initform 0 :initarg :str)
   (dex :initform 0 :initarg :dex)
   (equips :initform (make-hash-table) :accessor equips)
   (health :initform 6 :initarg :health)))

;; generate setters and getters for combat-entity stats
(mapc (lambda (name)
	(eval `(progn
		 (defmethod ,name ((obj combat-entity))
		   (+ (slot-value obj (quote ,name)) 
		      (loop for eq being the hash-values of (equips obj)
			    sum (slot-value eq (quote ,name)))))
		 (defmethod (setf ,name) (new-val (obj combat-entity))
		   (setf (slot-value obj (quote ,name)) new-val)))))	
      '(def str health dmg dex)) ; this is the list of stats

(defclass player (combat-entity) ())

(defclass enemy (combat-entity)
  ((spd ;; speed of 1 is the same as the player
        ;; speed of 2 is half as fast as the player
    :initform 1.2
    :initarg :spd
    :accessor spd)
   (enabled
    :initform nil
    :accessor enabled)))

(defun make-actor (name display-char pos &key (solid t) (consumable nil))
  (let ((new-actor (make-instance 'actor :pos pos
					 :display-char display-char
					 :name name
					 :solid solid
					 :consumable consumable)))
    (push new-actor *actors*)
    new-actor))

(defun make-player (name display-char pos)
  (let ((new-player (make-instance 'player :name name
				           :display-char display-char
					   :pos pos)))
    (push new-player *actors*)
    new-player))
  
(defparameter *player* (make-player "player" #\@ '(4 . 4)))

;; define new monster class and matching constructor function
(defmacro defenemy (name display-char new-slots
		    &rest keys
		    &key (inherit 'enemy inheritp)
		    &allow-other-keys)
  ;; remove :inherit from key list to prevent odd behavior
  (when inheritp
    (setf keys (remove inherit (remove :inherit keys))))
  ;; define helper functions
  (labels ((build-slot (slt) ; creates slot information for new slots
	     (list (if (listp slt)
		       `(,(car slt) :accessor ,(car slt)
				    :initform ,(cadr slt)
				    :initarg ,(intern (symbol-name (car slt))
						      "KEYWORD"))
		       `(,slt :accessor ,slt
			      :initform nil
			      :initarg ,(intern (symbol-name slt) "KEYWORD")))))
	   (reinit-slots (args slotlist slotname) ; updates slot information
	     (if (car args)
		 (if slotname
		     (reinit-slots (cdr args)
				   (cons (list (read-from-string slotname)
					       :initform
					       (car args))
					 slotlist)
				   nil)
		     (reinit-slots (cdr args)
				   slotlist
				   (symbol-name (car args))))
		 slotlist))
	 (constructor-name () ; get the name of the constructor function
	   (read-from-string (concatenate 'string "make-"
					  (symbol-name name)))))
    `(progn
       ;; declare new monster class, including new keys and setting initform of
       ;; old values
       (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil nil)))
       ;; define constructor for new class
       (defun ,(constructor-name) (pos)
	 (let ((new-enemy (make-instance (quote ,name)
					 :pos pos
					 :display-char ,display-char
					 :name (quote ,name))))
	   (push new-enemy *actors*)
	   (push new-enemy *dynamic-actors*)
	   new-enemy)))))

(defun make-equipment (equip-slot &key (def 0) (str 0) (dmg 0)
				    (dex 0) (health 0))
  (make-instance 'equipment :def def :str str :dmg dmg
			    :dex dex :health health :equip-slot equip-slot))

(defmethod destroy ((obj actor))
  (setf *actors* (remove obj *actors* :test 'equal)))

(defmethod destroy ((obj enemy))
  (setf *actors* (remove obj *actors* :test 'equal))
  (setf *dynamic-actors* (remove obj *dynamic-actors* :test 'equal)))

(defmethod equip ((item equipment) (obj combat-entity))
  (setf (gethash (equip-slot item) (equips obj)) item))

(defun square (number)
  (* number number))

(defun add-pos (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defun sub-pos (a b)
  (cons (- (car a) (car b)) (- (cdr a) (cdr b))))

(defun distance (p1 p2)
  (sqrt (+ (square (- (car p2) (car p1)))
	   (square (- (cdr p2) (cdr p1))))))

;; generate a random number between 1 and d
(defun roll (d)
  (1+ (random (max 1 d))))

(defun attack (a d)
  (let ((accuracy (roll 20)))
    (if (>= (+ accuracy (dex a)) (+ (- 11 (def d)) (dex d)))
	(let ((damage (- (+ (roll (dmg a)) (str a)) (def d)))
	      (result "")
	      (crit nil))
	  (when (= accuracy 20)
	    (setf crit t)
	    (incf damage (roll (dmg a))))
	  (decf (health d) (max 1 damage))
	  (when (<= (health d) 0)
	    (setf result ", killing it")
	    (destroy d))
	  (format t "~a~a hit a ~a for ~d damage~a~&"
		  (if crit "CRITICAL! " "")
		  (name a)
		  (name d)
		  damage
		  result))
	(format t "~a missed~&" (name a)))))

(defgeneric interact (a b))

(defmethod interact ((a actor) (b actor))
  (princ (concatenate 'string (name a) " interacted with a " (name b)))
  (if (consumable b)
      (destroy b))
  (fresh-line))

(defmethod interact ((a player) (b enemy))
  (attack a b))

(defmethod interact ((a enemy) (b player))
  (attack a b))

;; Return an item, chosen by the player, from the given list
;; If the list items are not printable, pass a naming-function that gets a
;; printable name from the list item.
(defun get-item-from-list (lst &optional (naming-function (lambda (x) x)))
  (labels ((print-list (l i)
	     (when (car l)
	       (format t "~d) ~a~%" i (funcall naming-function (car l)))
	       (print-list (cdr l) (1+ i))))
	   (pick-item ()
	     (fresh-line)
	     (princ "Choose an object: ")
	     (let ((choice (read-from-string (read-line))))
	       (if (and (numberp choice) (< choice (length lst)))
		   (nth choice lst)
		   (progn (princ "That was an invalid choice")
			  (pick-item))))))
    (print-list lst 0)
    (pick-item)))

;; returns a direction value pair chosen by the user.
(defun get-direction ()
  (fresh-line)
  (princ "Pick a direction (w, a, s, d): ")
  (let ((input (read-line)))
    (cond ((equal input "a")
	   +left+)
	  ((equal input "d")
	   +right+)
	  ((equal input "w")
	   +up+)
	  ((equal input "s")
	   +down+)
	  (t
	   (princ "That was not a direction")
	   (get-direction)))))

(defun find-actor-at (&key pos actor)
  (unless pos
    (when actor
      (setf pos (pos actor))))
  (loop for a2 in *actors*
	unless (equal a2 actor)
	  when (equal pos (pos a2))
	    return a2))

(defmethod move ((obj actor) distance)
  (let* ((newpos (add-pos (pos obj) distance))
	 (collider (find-actor-at :actor obj :pos newpos)))
    (if (and collider (solid collider))
	(interact obj collider)
	(when (gethash newpos *board*)
	  (setf (pos obj) newpos)))))

;;; Use breadth-first search to find shortest path between the two input points
(defun find-path (from to)
  (let ((came-from (make-hash-table :test 'equal)))
    (setf (gethash from came-from) t)
    (labels ((neighbors (pos)
	       (loop for direction in (list +left+ +right+ +up+ +down+)
		     collect (let* ((newpos (add-pos pos direction))
				    (actor (find-actor-at :pos newpos)))
			       (if (gethash newpos *board*)
				   (if (and actor (solid actor)
					    (not (equal newpos to)))
				       nil
				       newpos)
				   nil))))
	     (iterate (frontier)
	       (let ((current (car frontier)))
		 (when (and current
			    (not (equal current to)))
		   (loop for neighbor in (neighbors current)
			 when neighbor
			   do (unless (gethash neighbor came-from)
				(setf (gethash neighbor came-from)
				      current)
				(setf frontier
				      (append frontier
					      (list neighbor)))))
		   (iterate (cdr frontier)))))
	     (build-path (path pos)
	       (if (equal pos from)
		   path
		   (build-path (cons pos path) (gethash pos came-from)))))
      (iterate (list from))
      (if (gethash to came-from)
	  (build-path '() to)
	  (list from)))))

(defun step-towards (to from)
  (sub-pos (car (find-path (pos from) (pos to))) (pos from)))

(defmethod update ((obj enemy))
  (move obj (step-towards *player* obj)))

(defun print-board ()
  (let ((actor-chars (make-hash-table :test 'equal)))
    (loop for actor in *actors*
	  do (setf (gethash (pos actor) actor-chars) (display-char actor)))
    (labels ((on-board (pos) (gethash pos *board*))
	     (foundp (pos) (eq (on-board pos) 'found))
	     (get-char (pos)
	       (if (on-board pos) ; is the cell on the board?
		   ;; if so, check if it is in sight OR *sight-distance* is -1
		   (if (or (<= (distance (pos *player*) pos) *sight-distance*)
			   (= *sight-distance* -1))
		       ;; if it is, populate it
		       (progn (setf (gethash pos *board*) 'found)
			      (let ((c (gethash pos actor-chars)))
				(if c c #\.)))
		       ;; otherwise, return an empty space
		       #\space)
		   ;; if the cell is not on the board, check all
		   ;; adjacent cells and add walls as necessary.
		   (cond ((or (foundp (add-pos pos +left+))
			      (foundp (add-pos pos +right+)))
			  #\|) ; vertical wall
			 ((or (foundp (add-pos pos +up+))
			      (foundp (add-pos pos +down+)))
			  #\-) ; horizontal wall
			 (t #\space))))) ; nothing
      ;; print the board
      (loop for y from -1 to (+ (cdr *board-size*) 1)
	    do (progn (loop for x from -1 to (+ (car *board-size*) 1)
			    do (princ (get-char (cons x y))))
		      (fresh-line))))))

(defmethod input (cmd)
  (let ((action (gethash cmd *actions*)))
    (when action
      (funcall action))))

;;; iterate through *dynamic-actors* and update them if applicable
(defun update-all-actors ()
  (mapc (lambda (actor)
	  ;; enable the actor if it's in sight
	  (when (or (<= (distance (pos actor) (pos *player*))
			*sight-distance*)
		    (= *sight-distance* -1))
	    (setf (enabled actor) t))
	  ;; update the actor if it's enabled and *player-actions* lines
	  ;; up to speed
	  (when (and (enabled actor)
		     (< (mod *player-actions* (spd actor)) 1))
	    (update actor)))
	*dynamic-actors*))

(defun game-loop ()
  (print-board)
  (let ((cmd (read-line)))
    (unless (equal cmd "quit")
      ;; only update if input was a valid command
      (when (input cmd)
	(update-all-actors))
      (game-loop))))

(defun start ()
  (game-loop))

(defaction "a" (move *player* +left+))
(defaction "d" (move *player* +right+))
(defaction "w" (move *player* +up+))
(defaction "s" (move *player* +down+))
(defaction "i" (let ((actor (find-actor-at :actor *player*)))
		 (when actor
		   (interact *player* actor))))
