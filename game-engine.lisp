(load "./utils.lisp")
(load "./colors.lisp")

(load "~/quicklisp/setup.lisp")
(ql:quickload :trivial-raw-io)

(defparameter *layers* '())
(defparameter *current-layer* nil)
(defparameter *layer-index* 0)
(defparameter *player-actions* 0)
(defparameter *sight-distance* 3)
(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))
(defparameter +zero+ '(0 . 0))
(defparameter *actions* (make-hash-table :test 'equal))
(defparameter *inventory* '())
(defparameter *light-zone* '())
(defparameter *show-found-spaces* nil)
(defparameter *in-terminal* (handler-case (sb-posix:tcgetattr 0)
			      (error () nil)))

(defun custom-read-char ()
  (force-output)
  (if *in-terminal*
      (trivial-raw-io:read-char)
      (read-char)))

(defmacro defaction (key &body body)
  `(setf (gethash ,key *actions*) (lambda ()
				    ,@body
				    (incf *player-actions*) t)))

(defclass equipment ()
  ((def :initform 0 :accessor def :initarg :def)
   (dmg :initform 0 :accessor dmg :initarg :dmg)
   (str :initform 0 :accessor str :initarg :str)
   (dex :initform 0 :accessor dex :initarg :dex)
   (health :initform 0 :accessor health :initarg :health)
   (name :initform "" :accessor name :initarg :name)
   (description :accessor description :initarg :description)
   (consumable :initform nil :accessor consumable :initarg :consumable)
   (equip-slot :initform 'any :accessor equip-slot :initarg :equip-slot)))

(defmethod description ((obj equipment))
  (if (slot-boundp obj 'description)
      (slot-value obj 'description)
      (log-to-string "a ~a" (name obj))))

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
   (description
    :initform ""
    :initarg :description
    :accessor description)
   (solid
    :initform t
    :initarg :solid
    :accessor solid)
   (interact-action-only
    :initform nil
    :initarg :interact-action-only
    :accessor interact-action-only)
   (color
    :initform 0
    :initarg :color
    :accessor color)
   (destructible :initform t
		 :initarg :destructible
		 :accessor destructible)
   (health :initform 1
	   :initarg :health
	   :accessor health)
   (consumable
    :initform nil
    :initarg :consumable
    :accessor consumable)))

(defgeneric get-ascii (obj)
  (:method ((obj actor))
    (if *in-terminal*
	(apply-color (display-char obj) (color obj))
	(display-char obj))))

(defclass pickup (actor)
  ((consumable :initform t)
   (display-char :initform #\*)
   (solid :initform nil)
   (equipment :initarg :equipment :accessor equipment)))

(defmethod name ((obj pickup))
  (name (equipment obj)))

(defmethod description ((obj pickup))
  (description (equipment obj)))

(defclass combat-entity (actor)
  ((def :initform 0 :initarg :def)
   (dmg :initform 0 :initarg :dmg)
   (str :initform 0 :initarg :str)
   (dex :initform 0 :initarg :dex)
   (equips :initform (make-hash-table) :accessor equips)
   (health :initform 6)))

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

(defparameter *player* (make-instance 'player :name 'player
					      :color +red+
					      :display-char #\@))

(defclass layer ()
  ((board :initarg :board)
   (dynamic-actors :initform '())
   (actors :initform '())
   (up-ladder-pos :initarg :up-ladder-pos :accessor up-ladder-pos)
   (down-ladder-pos :initarg :down-ladder-pos :accessor down-ladder-pos)
   (board-size :initarg :board-size)))

(defun dynamic-actors ()
  (slot-value *current-layer* 'dynamic-actors))

(defun static-actors ()
  (slot-value *current-layer* 'actors))

(defun (setf static-actors) (value)
  (setf (slot-value *current-layer* 'actors) value))

(defun (setf dynamic-actors) (value)
  (setf (slot-value *current-layer* 'dynamic-actors) value))

(defun actors ()
  (append (slot-value *current-layer* 'actors) (dynamic-actors) (list *player*)))

(defun board ()
  (slot-value *current-layer* 'board))

(defun board-size ()
  (slot-value *current-layer* 'board-size))

(defclass enemy (combat-entity)
  ((spd ;; speed of 1 is the same as the player
        ;; speed of 2 is half as fast as the player
    :initform 1.2
    :initarg :spd
    :accessor spd)
   (enabled
    :initform nil
    :accessor enabled)))

(defun make-actor (name display-char pos &key (solid t) (consumable nil) (color 0)
					   (description "")
					   (interact-action-only nil interact-action-p))
  (let ((new-actor (make-instance 'actor :pos pos
					 :display-char display-char
					 :name name
					 :solid solid
					 :interact-action-only (if interact-action-p
								   interact-action-only
								   (not solid))
					 :color color
					 :description description
					 :consumable consumable)))
    (push new-actor (static-actors))
    new-actor))

(defun make-pickup (equipment pos)
  (let ((pickup (make-instance 'pickup :equipment equipment :pos pos
				       :interact-action-only t)))
    (push pickup (static-actors))
    pickup))

;; initialize helper functions for macros
(labels ((constructor (name)
	   (read-from-string (concatenate 'string "make-"
					  (symbol-name name))))
	 (build-slot (slt) ; creates slot information for new slots
	     (list (if (listp slt)
		       `(,(car slt) :accessor ,(car slt)
				    :initform ,(cadr slt)
				    :initarg ,(intern (symbol-name (car slt))
						      "KEYWORD"))
		       `(,slt :accessor ,slt
			      :initform nil
			      :initarg ,(intern (symbol-name slt) "KEYWORD")))))
	   (reinit-slots (args slotlist &key (slotname nil slotnamep))
	     (if (> (length args) 0) ; cannot check for (car arg) because it might be nil
		 (if slotnamep
		     (reinit-slots (cdr args)
				   (cons (list (read-from-string slotname)
					       :initform
					       (car args))
					 slotlist))
		     (reinit-slots (cdr args)
				   slotlist
				   :slotname (symbol-name (car args))))
		 slotlist)))
  ;; define new monster class and matching constructor function
  (defmacro defenemy (name display-char new-slots
		      &rest keys
		      &key (inherit 'enemy inheritp)
		      &allow-other-keys)
    ;; remove :inherit from key list to prevent odd behavior
    (when inheritp
      (setf keys (remove inherit (remove :inherit keys))))
    `(progn
       ;; declare new monster class, including new keys and setting initform of
       ;; old values
       (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil)))
       ;; define constructor for new class
       (defun ,(constructor name) (pos)
	 (let ((new-enemy (make-instance (quote ,name)
					 :pos pos
					 :display-char ,display-char
					 :name (quote ,name))))
	   (push new-enemy (dynamic-actors))
	   new-enemy))))

  ;; define class and constructor function for actor
  (defmacro defactor (name display-char new-slots
		      &rest keys
		      &key (inherit 'actor inheritp)
		      &allow-other-keys)
    (when inheritp
      (setf keys (remove inherit (remove :inherit keys))))
    `(progn (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					     ,@(reinit-slots keys nil)
					     (name :initform (quote ,name))
					     (display-char :initform ,display-char)))
	    (defun ,(constructor name) (pos)
	      (let ((new-actor (make-instance (quote ,name) :pos pos)))
		,(unless (member :interact-action-only keys)
		  '(setf (interact-action-only new-actor) (not (solid new-actor))))
		(push new-actor (static-actors))
		new-actor))))
  
  ;; define class, constructor function, and pickup generator function for equipment
  (defmacro defequipment (name new-slots
			  &rest keys
			  &key (inherit 'equipment inheritp)
			  &allow-other-keys)
    (when inheritp
      (setf keys (remove inherit (remove :inherit keys))))
    `(progn
       ;; define equipment class
       (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil)
					(name :initform (quote ,name))))
       ;; define constructor function
       (defun ,(constructor name) (&rest keys &key &allow-other-keys)
	 (apply #'make-instance (quote ,name) keys))
       ;; define pickup constructor function
       (defun ,(read-from-string (concatenate 'string "make-" (symbol-name name) "-pickup"))
	   (pos)
	 (make-pickup (,(constructor name)) pos)))))

(defactor ladder #\# (direction) :destructible nil :solid nil :description "a ladder")

(defun make-equipment (equip-slot &key (def 0) (str 0) (dmg 0)
				    (dex 0) (health 0) (name ""))
  (make-instance 'equipment :def def :str str :dmg dmg :name name
			    :dex dex :health health :equip-slot equip-slot))

(defun make-layer (dungeon)
  (let* ((dungeon-board (slot-value dungeon 'board))
	 (up-ladder-pos (nth (random (1- (length dungeon-board))) (cdr dungeon-board)))
	 (down-ladder-pos (car dungeon-board))
	 (layer (make-instance
		 'layer
		 :up-ladder-pos up-ladder-pos
		 :down-ladder-pos down-ladder-pos
		 :board (loop for pos in dungeon-board
			      with table = (make-hash-table :test #'equal)
			      do (setf (gethash pos table) 'hidden)
			      finally (return table))
		 :board-size (loop for pos in dungeon-board
				   maximize (car pos) into x
				   maximize (cdr pos) into y
				   finally (return (cons x y))))))
    (setf *current-layer* layer)
    (setf (pos *player*) up-ladder-pos)
    (setf (direction (make-ladder down-ladder-pos)) 1)
    (setf (direction (make-ladder up-ladder-pos)) -1)
    (loop for pos being the hash-keys of (slot-value dungeon 'actors)
	  unless (or (equal pos up-ladder-pos) (equal pos down-ladder-pos))
	    do (let ((fxn (gethash pos (slot-value dungeon 'actors))))
		 (when fxn
		   (funcall fxn pos))))
    (push layer *layers*)
    layer))

(defun remove-from-inventory (item)
  (setf *inventory* (remove item *inventory* :test #'equal)))

(defun add-to-inventory (item)
  (if (< (length *inventory*) 10)
      (push item *inventory*)
      (progn (print-to-log "your inventory is full")
	     (make-pickup item (pos *player*))
	     nil)))

(defgeneric destroy (obj)
  (:method (obj)
    (print-to-log "~a destroyed~&" obj))
  (:method ((obj actor))
    (setf (static-actors) (remove obj (static-actors) :test 'equal)))
  (:method ((obj enemy))
    (setf (dynamic-actors) (remove obj (dynamic-actors) :test 'equal)))
  (:method ((obj equipment))
    (remove-from-inventory obj)))

(defgeneric equip (item obj)
  (:method ((item equipment) (obj combat-entity))
    (let ((old-item (gethash (equip-slot item) (equips obj))))
      (setf (gethash (equip-slot item) (equips obj)) item)
      old-item))
  (:method :around ((item equipment) (obj combat-entity))
    (if (eq (equip-slot item) 'none)
	(progn (print-to-log "That cannot be equipped")
	       'failed)
	(when (next-method-p)
	  (call-next-method)))))

(defgeneric display (obj &key as-lines fields headers)
  (:method (obj &key as-lines fields headers)
    (declare (ignore fields headers))
    (if as-lines
	(list (log-to-string "~a" obj))
	(print-to-log "~a~%" obj)))
  (:method ((obj actor) &key as-lines (fields '(description)) (headers t))
    (let ((lines (mapcar (lambda (field)
			   (if headers
			       (log-to-string
				"~a: ~a" field
				(funcall field obj))
			       (log-to-string
				"~a" (funcall field obj))))
			 fields)))
      (if as-lines
	  lines
	  (log-to-string "~{~a~%~}" lines)))))

(defgeneric use (item target)
  (:method :after ((item equipment) target)
    (when (consumable item)
      (destroy item)))
  (:method (item target)
    (print-to-log "That cannot be used")))

(defgeneric damage (target amount)
  (:method ((target actor) amount)
    (decf (health target) (max 1 amount))
    amount)
  (:method :after ((target actor) amount)
    (when (and (<= (health target) 0)
	       (destructible target))
      (destroy target)))
  (:method ((target combat-entity) amount)
    (decf (health target) (max 1 (- amount (def target))))
    amount))

(defgeneric attack (a d)
  (:method ((a combat-entity) (d combat-entity))
    (let ((accuracy (roll 20)))
      (if (>= (+ accuracy (dex a)) (+ (- 6 (def d)) (dex d)))
	  (let ((damage-dealt (+ (roll (dmg a)) (str a)))
		(crit nil))
	    (when (= accuracy 20)
	      (setf crit t)
	      (incf damage-dealt (roll (dmg a))))
	    (unless (= accuracy 1)
	      (setf damage-dealt (damage d damage-dealt)))
	    (print-to-log "~a~a hit a ~a for ~d damage~a~&"
			  (if crit "CRITICAL! " "")
			  (name a)
			  (name d)
			  damage-dealt
			  (if (<= (health d) 0) ", killing it" "")))
	  (print-to-log "~a missed~&" (name a)))))
  (:method ((a combat-entity) (d actor))
    (when (destructible d)
      (print-to-log "~a hit a ~a for ~d damage~a~&"
		    (name a)
		    (name d)
		    (damage d (roll (dmg a)))
		    (if (<= (health d) 0)
			", destroying it"
			"")))))

;; Return an item, chosen by the player, from the given list
;; If the list items are not printable, pass a naming-function that gets a
;; printable name from the list item.
(defun get-item-from-list (lst &key
				 (naming-function (lambda (x) x))
				 (exit-option t))
  (labels ((print-list (l i)
	     (when (car l)
	       (print-to-screen "~d) ~a~%" i (funcall naming-function (car l)))
	       (print-list (cdr l) (1+ i))))
	   (pick-item ()
	     (fresh-line)
	     (print-to-screen "Choose an object: ")
	     (let ((choice (digit-char-p (custom-read-char))))
	       (cond ((and choice (< choice (length lst)))
		      (nth choice lst))
		     ((and choice
			   (= choice (length lst))
			   exit-option)
		      nil)
		     (t
		      (print-to-screen "That was an invalid choice")
		      (pick-item))))))
    (print-list lst 0)
    (when exit-option
      (print-to-screen "~d) cancel" (length lst)))
    (pick-item)))

(defun get-item-from-inventory ()
  (get-item-from-list *inventory* :naming-function #'name))

(defmacro with-item-from-inventory (&body body)
  `(if (= (length *inventory*) 0)
       (print-to-log "you have nothing in your inventory")
       (let ((item (get-item-from-inventory)))
	 ,@body)))

(defun change-layer (direction)
  (unless (or (and (= *layer-index* 0) (= direction -1))
	      (and (= (+ *layer-index* direction) (length *layers*))))
    (incf *layer-index* direction)
    (setf *current-layer* (nth *layer-index* *layers*))
    (if (= direction -1)
	(setf (pos *player*) (down-ladder-pos *current-layer*))
	(setf (pos *player*) (up-ladder-pos *current-layer*)))
    (update-los)
    t))

(defgeneric interact (a b)
  (:method (a b)) ; do nothing by default
  (:method :after (a (b actor))
    (if (consumable b)
	(destroy b)))
  (:method ((a player) (b enemy))
    (attack a b))
  (:method ((a player) (b ladder))
    (when (change-layer (direction b))
      (if (= (direction b) -1)
	  (print-to-log "you climb up the ladder to the previous dungeon level")
	  (print-to-log "you climb down the ladder to the next dungeon level"))))
  (:method ((a player) (b pickup))
    (when (add-to-inventory (equipment b))
      (print-to-log "You have picked up a ~a~&" (name b))))
  (:method ((a enemy) (b player))
    (attack a b)))

(defgeneric visiblep (obj)
  (:method :around (obj)
    (if (= *sight-distance* -1)
	t
	(call-next-method)))
  (:method ((obj list))
    (member obj *light-zone* :test 'equal))
  (:method ((obj actor))
    (visiblep (pos obj))))

;; returns a direction value pair chosen by the user.
(defun get-direction (&key include-zero (cancel t))
  (print-to-screen "Pick a direction (w, a, s, d~a~a): "
		(if include-zero ", (h)ere" "")
		(if cancel ", (c)ancel" ""))
  (let ((input (custom-read-char)))
    (cond ((equal input #\a)
	   +left+)
	  ((equal input #\d)
	   +right+)
	  ((equal input #\w)
	   +up+)
	  ((equal input #\s)
	   +down+)
	  ((and (equal input #\h) include-zero)
	   +zero+)
	  ((and (equal input #\c) cancel)
	   nil)
	  (t
	   (print-to-screen "That was not a direction")
	   (get-direction)))))

(defgeneric find-actor-at (a &rest actors-to-ignore)
  (:method ((a list) &rest actors-to-ignore)
    (loop for actor in (actors)
	  unless (member actor actors-to-ignore :test #'equal)
	    when (equal a (pos actor))
	      return actor))
  (:method ((a actor) &rest actors-to-ignore)
    (apply #'find-actor-at (pos a) a actors-to-ignore)))

(defgeneric find-all-actors-at (a &rest actors-to-ignore)
  (:method ((a list) &rest actors-to-ignore)
    (loop for actor in (actors)
	  unless (member actor actors-to-ignore :test #'equal)
	    when (equal a (pos actor))
	      collect actor))
  (:method ((a actor) &rest actors-to-ignore)
    (apply #'find-all-actors-at (pos a) a actors-to-ignore)))

(defmethod find-solid-actor-at (a &rest actors-to-ignore)
  (loop for actor in (apply #'find-all-actors-at a actors-to-ignore)
	when (solid actor)
	  return actor))

(defun update-spaces-found ()
  (mapc (lambda (pos)
	  (setf (gethash pos (board)) 'found))
	*light-zone*))

;; use flood-fill algorithm to determine where the player can see
(defun update-los ()
  (let ((reached (list (pos *player*))))
    (labels ((neighbors (pos)
	       (loop for direction in (list +left+ +right+ +up+ +down+)
		     collect (let ((newpos (add-pos pos direction)))
			       (if (gethash newpos (board))
				   newpos
				   nil))))
	     (iterate (frontier)
	       (let ((current (car frontier)))
		 (when (and current
			    (< (distance (pos *player*) current)
				*sight-distance*))
		   (loop for neighbor in (neighbors current)
			 when neighbor
			   do (unless (member neighbor reached :test 'equal)
				(push neighbor reached)
				(setf frontier
				      (append frontier
					      (list neighbor)))))
		   (iterate (cdr frontier))))))
      (iterate (list (pos *player*))))
    (setf *light-zone* reached))
  (update-spaces-found))

(defgeneric move (obj distance)
  (:method ((obj actor) (distance list))
    (let* ((newpos (add-pos (pos obj) distance))
	   (collider (find-solid-actor-at newpos obj)))
      (if collider
	  (unless (interact-action-only collider)
	      (interact obj collider))
	  (when (gethash newpos (board))
	    (setf (pos obj) newpos)
	    (loop for actor in (find-all-actors-at obj)
		  unless (interact-action-only actor)
		    do (interact obj actor))))))
  (:method ((obj player) (distance list))
    (call-next-method)
    (update-los)))

;;; Use breadth-first search to find shortest path between the two input points
(defun find-path (from to)
  (let ((came-from (make-hash-table :test 'equal)))
    (setf (gethash from came-from) t)
    (labels ((neighbors (pos)
	       (loop for direction in (list +left+ +right+ +up+ +down+)
		     collect (let ((newpos (add-pos pos direction)))
			       (if (gethash newpos (board))
				   (if (and (find-solid-actor-at newpos)
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
    (loop for actor in (actors)
	  do (setf (gethash (pos actor) actor-chars) (get-ascii actor)))
    (labels ((on-board (pos) (gethash pos (board)))
	     (foundp (pos) (eq (on-board pos) 'found))
	     (get-char (pos)
	       (if (on-board pos) ; is the cell on the board?
		   ;; if so, check if it is in sight OR *sight-distance* is -1
		   (if (visiblep pos)
		       ;; if it is, populate it
		       (let ((c (gethash pos actor-chars)))
			 (if c c #\.))
		       ;; otherwise, return an empty space
		       (if (and *show-found-spaces* *in-terminal* (foundp pos))
			   (apply-color #\. +grey+)
			   #\space))
		   ;; if the cell is not on the board, check all
		   ;; adjacent cells and add walls as necessary.
		   (cond ((or (foundp (add-pos pos +left+))
			      (foundp (add-pos pos +right+)))
			  #\|) ; vertical wall
			 ((or (foundp (add-pos pos +up+))
			      (foundp (add-pos pos +down+)))
			  #\-) ; horizontal wall
			 (t #\space)))))
      ;; print the board
      (let ((player-info (display *player* :as-lines t
					   :fields '(health str dex def dmg))))
	(loop for y from -1 to (+ (cdr (board-size)) 1)
	      do (format t "~{~a~} ~a~%"
			 (loop for x from -1 to (+ (car (board-size)) 1)
			       collect (get-char (cons x y)))
			 (if (and (>= y 0) (< y (length player-info)))
			     (nth y player-info)
			     "")))))))

(defun print-inventory ()
  (mapc (lambda (item)
	  (print-to-log "~a" (name item)))
	*inventory*))

(defun input (cmd)
  (let ((action (gethash cmd *actions*)))
    (when action
      (funcall action))))

;;; iterate through *dynamic-actors* and update them if applicable
(defun update-all-actors ()
  (mapc (lambda (actor)
	  ;; enable the actor if it's in sight
	  (when (visiblep actor)
	    (setf (enabled actor) t))
	  ;; update the actor if it's enabled and *player-actions* lines
	  ;; up to speed
	  (when (and (enabled actor)
		     (< (mod *player-actions* (spd actor)) 1))
	    (update actor)))
	(dynamic-actors)))

;; clears the terminal if possible
(defun clear-terminal ()
  (when *in-terminal*
    (format t "~cc" #\esc)))

(defun print-log (&optional (clear t))
  (format t "~{~a~&~}" *log*)
  (when clear
    (setf *log* '())))

(defun game-loop (&optional cmd)
  (unless (equal cmd #\q)
    ;; only update if input was a valid command
    (when (input cmd)
      (update-all-actors))
    (clear-terminal)
    (print-board)
    (print-log)
    (game-loop (custom-read-char))))

(defun start ()
  (setf *current-layer* (car *layers*))
  (update-los)
  (game-loop))
