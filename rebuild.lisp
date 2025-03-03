(load "~/quicklisp/setup.lisp")
(ql:quickload :trivial-raw-io)

(load "utils.lisp")

(defstruct attack dmg to-hit source types)
(defclass actor ()
  ((display-char :initform #\? :initarg :display-char :writer (setf display-char))
   (pos :initform +zero+ :initarg :pos :accessor pos)
   (name :initform "" :initarg :name :accessor name)
   (color :initform 30 :initarg :color :accessor color)
   (hiddenp :initform nil :initarg :hiddenp :accessor hiddenp)))
(defclass creature (actor)
  ((dex :initform 0 :initarg :dex :accessor dex)
   (str :initform 0 :initarg :str :accessor str)
   (spd :initform 1 :initarg :spd :accessor spd)
   (health :initform 1 :initarg :health :reader health)
   (armor :initform 0 :initarg :armor :accessor armor)
   (evasion :initform 0 :initarg :evd :writer (setf evasion))
   max-health
   (equipment :initform (make-hash-table) :accessor equipment)
   (slot-nums :initform '((hand 2) (misc 3) (body 1)) :initarg :slot-nums :accessor slot-nums)
   (resistances :initform '() :initarg :resist :accessor resistances)
   (immunities :initform '() :initarg :immune :accessor immunities)
   (vulnerablities :initform '() :initarg :vulnerable :accessor vulnerabilities)
   (absorbances :initform '() :initarg :absorb :accessor absorbances)))
(defclass enemy (creature)
  ((energy :initform 0 :accessor energy)))
(defclass equipment (actor)
  ((attack :initform '(1 3 0 0 bludgeoning) :initarg :atk :accessor atk)
   (range :initform 1 :initarg :range :accessor range)
   (size :initform 1 :initarg :size :accessor size)
   (weaponp :initform nil :initarg :weaponp :accessor weaponp)
   (equip-slot :initform 'hand :initarg :slot :accessor equip-slot)))

(load "terminal.lisp")
(load "inventory.lisp")
(load "bsp-dungeon.lisp")

(defparameter *solid-actors* (make-hash-table :test #'equal))
(defparameter *non-solid-actors* (make-hash-table :test #'equal))
(defparameter *board-size* '(60 . 20))
(defparameter *player* (make-instance 'creature :health 10 :name "player" :pos '(5 . 5) :color 31))
(defparameter *sight-distance* 10)
(defparameter *actions* (make-hash-table))
(defparameter *action-descriptions* (make-hash-table))
(defparameter *print-surroundings-mode* 'non-walls)

(defgeneric unequip (item actor)
  (:method (item actor))
  (:method :after ((item equipment) (actor (eql *player*)))
    (declare (ignore actor))
    (add-to-inventory item))
  (:method :after ((item equipment) (actor creature))
    (setf (gethash (equip-slot item) (equipment actor))
	  (remove item (gethash (equip-slot item) (equipment actor)) :test #'equal))))

(defgeneric equip (item actor)
  (:method (item actor))
  (:method :around ((item equipment) (actor (eql *player*)))
    (let ((result (call-next-method)))
      (when result
	(remove-from-inventory item))
      result))
  (:method :around ((item equipment) (actor creature))
    (let* ((current-equips (gethash (equip-slot item) (equipment actor)))
	   (equips-size (loop for i in current-equips
			      sum (size i)))
	   (max-equips (cadr (assoc (equip-slot item) (slot-nums actor)))))
      (flet ((equip-item ()
	       (push item (gethash (equip-slot item) (equipment actor)))
	       (when (next-method-p)
		 (call-next-method))
	       t))
	(when max-equips
	  (if (<= (+ equips-size (size item)) max-equips)
	      (equip-item)
	      (labels ((get-items-to-unequip (&optional item-list (size 0))
			 (if (>= size (size item))
			     item-list
			     (let ((item-to-replace (get-item-from-list (gethash (equip-slot item)
										 (equipment actor))
									:naming-function #'name
									:ignoring item-list
									:test #'equal
									:what "item to replace")))
			       (if item-to-replace
				   (get-items-to-unequip (cons item-to-replace item-list)
							 (+ size (size item-to-replace)))
				   nil)))))
		(let ((items-to-unequip (get-items-to-unequip)))
		  (when items-to-unequip
		    (mapc (lambda (i) (unequip i actor)) items-to-unequip)
		    (equip-item)
		    items-to-unequip)))))))))

(defmacro defaction ((&rest keys) description &body body)
  (let ((key (gensym))
	(key-list (gensym)))
    `(let ((,key-list (if (listp ',keys)
			  ',keys
			  '(,keys))))
       (loop for ,key in ,key-list
	     when (gethash ,key *action-descriptions*)
	       do (print-to-log "You're declaring the ~a action twice!" ,key)
	     do (setf (gethash ,key *actions*) (lambda () ,@body)))
       (setf (gethash (format nil "~{~c~#[~; or ~;, ~]~}" ,key-list)
		      *action-descriptions*)
	     ,description))))

(defun solid (pos)
  (gethash pos *solid-actors*))

(defun (setf solid) (value pos)
  (setf (gethash pos *solid-actors*) value))

(defun remove-solid (pos)
  (remhash pos *solid-actors*))

(defun non-solid (pos)
  (gethash pos *non-solid-actors*))

(defun (setf non-solid) (value pos)
  (setf (gethash pos *non-solid-actors*) value))

(defun remove-non-solid (pos)
  (remhash pos *non-solid-actors*))

(defun contents (pos)
  (or (solid pos) (non-solid pos)))

(defun emptyp (pos)
  (not (contents pos)))

(defun remove-all (pos)
  (remove-non-solid pos)
  (remove-solid pos))

(defgeneric wallp (obj)
  (:method (obj) nil)
  (:method ((obj list)) (wallp (solid obj)))
  (:method ((obj symbol)) (eq obj 'wall))
  (:method ((obj character)) t))

(defmethod name ((obj character)) "wall")
(defmethod name ((obj symbol)) obj)

(defun apply-default-colors ()
  (format t "~c[40;37m" #\esc))

(defun apply-colors (char colors)
  (format nil "~c[~{~d~^;~}m~c~0@*~c[40;37m"
	  #\esc (if (listp colors)
		    colors
		    (list colors))
	  char))

(defgeneric display-char (obj)
  (:method ((obj actor))
    (if (eq (color obj) 30)
	(slot-value obj 'display-char)
	(apply-colors (slot-value obj 'display-char) (color obj))))
  (:method ((pos list))
    (if (and (wallp (vec+ pos +up+))
	     (wallp (vec+ pos +down+)))
	(setf (solid pos) #\|)
	(setf (solid pos) #\-)))
  (:method ((obj character)) obj))

(defmacro flood-fill (start (value-to-store exit-condition
			     &key (solid t) (stop-for-occupied t) (go-until nil))
		      &body body)
  `(let ((cells (make-hash-table :test #'equal)))
     (setf (gethash ,start cells) t)
     (labels ((occupiedp (pos)
		(if ,solid
		    (solid pos)
		    (or (non-solid pos) (wallp (solid pos)))))
	      (neighbors (pos)
		(loop for direction in +directions+
		      unless (let ((cell-pos (vec+ pos direction)))
			       (or (gethash cell-pos cells)
				   (wallp (solid cell-pos))
				   (and (occupiedp cell-pos)
					(not (equal cell-pos ,go-until))
					,stop-for-occupied)))
			collect (vec+ pos direction)))
	      (iterate (frontier)
		(when (car frontier)
		  (let* ((current (car frontier))
			 (neighbors (neighbors current))
			 (exit-condition ,exit-condition))
		    (if exit-condition
			exit-condition
			(progn
			  (mapc (lambda (n) (setf (gethash n cells) ,value-to-store)) neighbors)
			  (iterate (append (cdr frontier) neighbors))))))))
       (let ((result (iterate (list ,start))))
	 ,@body))))

(defun find-path (from to)
  (flood-fill from (current (if (equal current to) t) :go-until to)
	      (if result
		  (labels ((build-path (pos &optional (path nil))
			     (if (equal pos from)
				 path
				 (build-path (gethash pos cells) (cons pos path)))))
		    (build-path to))
		  (list from))))

(defun place (obj pos &key (solid t))
  (flood-fill pos (t (unless (occupiedp current) current) 
		     :stop-for-occupied nil
		     :solid solid)
    (when result
      (if solid
	  (setf (solid result) obj)
	  (setf (non-solid result) obj))
      (setf (pos obj) result))))

(defun initialize-board ()
  (let* ((dungeon (generate-dungeon '(60 . 20) 4))
	 (cells (pos-flatten dungeon)))
    (loop for x from -1 to 61
	  do (loop for y from -1 to 21
		   ;; cell is not on board
		   unless (member (cons x y) cells :test #'equal)
		     ;; cell is next to board
		     when (loop for direction in +directions+
				  thereis (member (vec+ (cons x y) direction) cells :test #'equal))
		       ;; put a wall down
		       do (setf (solid (cons x y)) 'wall)))
    cells))

(defgeneric corpse (obj)
  (:method ((obj creature))
    (make-instance 'actor :name (log-to-string "~a corpse" (name obj)) :display-char #\c)))

(defmethod evasion ((obj creature))
  (+ 10 (dex obj) (slot-value obj 'evasion)))

(defgeneric deadp (obj)
  (:method ((obj creature))
    (= (health obj) 0)))

(defgeneric kill (obj)
  (:method (obj))
  (:method ((obj enemy))
    (remove-solid (pos obj))
    (place (corpse obj) (pos obj) :solid nil)))

(defmethod (setf health) (value (obj creature))
  (if (> value 0)
      (if (slot-boundp obj 'max-health)
	  (setf (slot-value obj 'health) (min value (slot-value obj 'max-health)))
	  (progn (setf (slot-value obj 'max-health) (health obj))
		 (setf (slot-value obj 'health) value)))
      (progn (setf (slot-value obj 'health) 0)
	     (kill obj))))
  
(defgeneric death (obj)
  (:method ((obj creature))
    "killing it"))

(defgeneric resistp (obj type)
  (:method ((obj creature) type)
    (member type (resistances obj))))

(defgeneric immunep (obj type)
  (:method ((obj creature) type)
    (member type (immunities obj))))

(defgeneric vulnerablep (obj type)
  (:method ((obj creature) type)
    (member type (vulnerabilities obj))))

(defgeneric absorbp (obj type)
  (:method ((obj creature) type)
    (member type (absorbances obj))))

(defun roll (num die &rest modifiers)
  (+ (loop repeat num
	   sum (1+ (random die)))
     (loop for m in modifiers
	   sum m)))

(defgeneric weapons (obj)
  (:method ((obj creature))
    (let ((held-items (gethash 'hand (equipment obj))))
      (if (<= (length held-items) 1)
	  held-items
	  (loop for equipment in held-items
		when (weaponp equipment)
		  collect equipment)))))

(flet ((generate-attack (attacker num die dmg-bonus to-hit &rest types)
	 (make-attack :dmg (roll num die (str attacker) dmg-bonus)
		      :to-hit (roll 1 20 to-hit (dex attacker))
		      :source (name attacker)
		      :types types)))
  (defgeneric get-attack (weapon attacker)
    (:method ((weapon equipment) (attacker creature))
      (apply #'generate-attack attacker (atk weapon)))
    (:method ((weapon list) (attacker creature))
      (apply #'generate-attack attacker weapon))))

(defun damage-modifier (defender damage-types)
  (labels ((calculate-damage-modifier (mod types)
	     (if types
		 (calculate-damage-modifier
		  (* mod (cond ((absorbp defender (car types)) -1)
			       ((immunep defender (car types)) 0)
			       ((resistp defender (car types)) 0.5)
			       ((vulnerablep defender (car types)) 2)
			       (t 1)))
		  (cdr types))
		 mod)))
    (calculate-damage-modifier 1 damage-types)))

(defun damage (defender attack)
  (let* ((base-damage (max 1 (- (attack-dmg attack) (armor defender))))
	 (mod-damage (round (* base-damage (damage-modifier defender (attack-types attack)))))
	 (real-damage (cond ((= mod-damage 0) 0)
			    ((< mod-damage 0) mod-damage)
			    (t (max 1 mod-damage)))))
    (decf (health defender) real-damage)
    (print-to-log "~a hit ~a for ~d damage~:[~;, ~a~]"
		  (attack-source attack)
		  (name defender)
		  (max 0 real-damage)
		  (deadp defender)
		  (death defender))))

(defgeneric attack (defender attacker)
  (:method :around ((defender creature) attacker)
    (unless (deadp defender)
      (call-next-method)))
  (:method ((defender creature) (attacker creature))
    ;; one attack per equipped hand item
    (mapc (lambda (weapon)
	    (when (<= (distance (pos defender) (pos attacker)) (range weapon))
	      (attack defender (get-attack weapon attacker))))
	  (weapons attacker)))
  (:method ((defender creature) (attack attack))
    (if (>= (attack-to-hit attack) (evasion defender))
	(damage defender attack)
	(print-to-log "~a missed ~a" (attack-source attack) (name defender)))))

(defun show-colors ()
  (loop for code below 111
	do (format t "~c[~dmCODE ~:*~d~0@*~c[0m~%" #\esc code)))

;;; memoized has-los function
(let ((memos (make-hash-table :test #'equal)))
  (labels ((calculate-los (to from)
	     (let ((dx (- (car to) (car from)))
		   (dy (- (cdr to) (cdr from))))
	       (labels ((get-pos-on-line (m)
			  (cons (round (+ (car from) (* m dx)))
				(round (+ (cdr from) (* m dy)))))
			(pos-opaquep (m)
			  (let* ((pos (get-pos-on-line m))
				 (actor (solid pos)))
			    (if actor
				(wallp actor)
				nil))))
		 (and (loop for x below (abs dx)
			    never (pos-opaquep (/ x (abs dx))))
		      (loop for y below (abs dy)
			    never (pos-opaquep (/ y (abs dy)))))))))
    (defun has-los (to from distance)
      (let ((key (list to from)))
	(multiple-value-bind (memo memo-existsp) (gethash key memos)
	     (if (if memo-existsp
		     memo
		     (let ((new-val (calculate-los to from)))
		       (setf (gethash key memos) new-val)
		       new-val))
		 ;; check if point is within distance
		 (or (< distance 0)
		     (>= distance (vec-length (vec- to from))))
		 nil))))))

(defgeneric pickup (item)
  (:method (item))
  (:method :after ((item equipment))
    (remove-non-solid (pos item))
    (add-to-inventory item)
    (print-to-log "you picked up a ~a" (name item))))

(defgeneric interact (object actor)
  (:method (object actor))
  (:method ((item equipment) (actor (eql *player*)))
    (pickup item)))

(defgeneric move-into (passive active)
  (:method :after (a (b (eql *player*)))
    (print-to-log "you moved into ~a" (name a)))
  (:method ((passive creature) (active creature))
    (attack passive active))
  (:method (passive active))) ; default case: do nothing

(defun reposition (obj new-pos)
  (let ((collider (solid new-pos)))
    (if collider
	(move-into collider obj)
	(progn (remove-solid (pos obj))
	       (setf (solid new-pos) obj)
	       (setf (pos obj) new-pos)
	       (move-into (non-solid new-pos) obj)))))

(defgeneric move (obj direction)
  (:method ((obj actor) direction)
    (reposition obj (vec+ (pos obj) direction)))
  (:method ((pos list) direction)
    (let ((newpos (vec+ pos direction)))
      (if (wallp (solid newpos))
	  pos
	  newpos))))

(defun step-towards (target obj)
  (reposition obj (car (find-path (pos obj) (pos target)))))

(defun flee-direction (source obj)
  (vec- (pos obj) (car (find-path (pos obj) (pos source)))))

(defun flee (source obj)
  (move obj (flee-direction source obj)))

(defgeneric act (obj)
  (:method ((obj enemy))
    (when (>= (energy obj) 1)
      (when (has-los (pos obj) (pos *player*) -1)
	(let ((primary (car (weapons obj))))
	  (cond ((and (<= (distance (pos obj) (pos *player*))
			  (/ (range primary) 2))
		      (not (solid (vec+ (pos obj) (flee-direction *player* obj)))))
		 (flee *player* obj))
		((<= (distance (pos obj) (pos *player*))
		     (range primary))
		 (attack *player* obj))
		(t
		 (step-towards *player* obj)))))
      (decf (energy obj))
      (act obj))))

(defgeneric update (obj)
  (:method (obj))
  (:method ((obj enemy))
    (incf (energy obj) (/ (spd obj) (spd *player*)))
    (act obj)))

(defun choose-target (initial-mode range &key (can-switch-p t))
  (let ((target-position (pos *player*))
	(target-list
	  (apply #'append
		 (loop for y from (- range) to range
		       collect (loop for x from (- range) to range
				     with pos = +zero+
				     do (setf pos (vec+ (cons x y)
							(pos *player*)))
				     when (and (has-los pos (pos *player*) range)
					       (<= (distance (pos *player*) pos)
						   range)
					       (solid pos)
					       (not (wallp (solid pos)))
					       (not (equal (solid pos) *player*)))
				       collect (solid pos))))))
    (if (or target-list
	    (eq initial-mode 'free-form))
	(labels ((two-key-targeting ()
		   (let ((i 0))
		     (if (and target-list
			      (<= (length target-list) 10))
			 (progn (with-cursor-saved
				    (mapc (lambda (obj)
					    (position-cursor-list (pos obj))
					    (escape-code #\m (color obj))
					    (print-to-screen "~d" i)
					    (incf i))
					  target-list))
				(labels ((undraw-numbers ()
					   (with-cursor-saved
					       (mapc (lambda (obj)
						       (position-cursor-list (pos obj))
						       (print-to-screen (display-char obj)))
						     target-list)))
					 (get-target ()
					   (let* ((input (custom-read-char))
						  (value (digit-char-p input)))
					     (cond (value
						    (if (< value i)
							(progn (undraw-numbers)
							       (nth value target-list))
							(get-target)))
						   ((and (eq input #\S)
							 can-switch-p)
						    (undraw-numbers)
						    (free-form-targeting))
						   ((eq input #\q)
						    (undraw-numbers)
						    nil)
						   (t
						    (get-target))))))
				  (get-target)))
			 (free-form-targeting))))
		 (free-form-targeting ()
		   (labels ((draw-cursor ()
			      (with-cursor-saved
				  (position-cursor-list target-position)
				(with-color (31 5)
				  (print-to-screen "X"))))
			    (undraw-cursor ()
			      (with-cursor-saved
				  (position-cursor-list target-position)
				(if (contents target-position)
				    (print-to-screen "~a" (display-char (contents target-position)))
				    (print-to-screen "."))))
			    (move-cursor (direction)
			      (when (<= (distance (vec+ target-position direction) (pos *player*))
					range)
				(undraw-cursor)
				(setf target-position (move target-position direction))))
			    (input-loop ()
			      (draw-cursor)
			      (let ((input (custom-read-char)))
				(cond ((eq input #\a)
				       (move-cursor +left+)
				       (input-loop))
				      ((eq input #\d)
				       (move-cursor +right+)
				       (input-loop))
				      ((eq input #\w)
				       (move-cursor +up+)
				       (input-loop))
				      ((eq input #\s)
				       (move-cursor +down+)
				       (input-loop))
				      ((and (eq input #\S)
					    can-switch-p)
				       (undraw-cursor)
				       (two-key-targeting))
				      ((eq input #\newline)
				       (undraw-cursor)
				       (or (contents target-position)
					   target-position))
				      ((eq input #\q)
				       (undraw-cursor)
				       nil)
				      (t
				       (input-loop))))))
		     (input-loop))))
	  (if (eq initial-mode 'two-key)
	      (two-key-targeting)
	      (free-form-targeting)))
	(progn (print-to-log "there are no targets in range")
	       nil))))

(defgeneric throw-at (target obj thrower)
  (:method :before (target (item equipment) thrower) ;; FIXME thrower should be typed as player
    (declare (ignore target)
	     (ignore thrower))
    (remove-from-inventory item))
  (:method (target obj thrower))
  (:method ((target creature) (item equipment) (thrower creature))
    (attack target (get-attack item thrower)))
  (:method :after ((target actor) (item equipment) thrower)
    (place item (pos target) :solid nil))
  (:method :after ((target list) (item equipment) thrower)
    (place item target :solid nil)))

(defgeneric visiblep (obj)
  (:method ((pos list))
    (has-los pos (pos *player*) *sight-distance*))
  (:method ((n symbol)) nil)
  (:method ((c character)) t)
  (:method ((obj actor))
    (if (hiddenp obj)
	nil
	(visiblep (pos obj)))))

(defun print-board ()
  (apply-default-colors)
  (loop for y from -1 to (1+ (cdr *board-size*))
	do (format t "~{~a~}~%"
		   (loop for x from -1 to (1+ (car *board-size*))
			 collect (let* ((pos (cons x y))
					(actor (contents pos)))
				   (cond ((characterp actor)
					  actor)
					 ((and (wallp actor)
					       (or (visiblep pos)
						   (visiblep actor)))
					  (display-char pos))
					 ((and actor (visiblep actor))
					  (display-char actor))
					 ((visiblep pos) #\.)
					 (t #\space)))))))

(defun print-surroundings ()
  (flet ((printp (obj)
	   (not (or (eq *print-surroundings-mode* 'none)
		    (not obj)
		    ;; things that are walls but not characters (secret doors) should be printed
		    (and (characterp obj) (eq *print-surroundings-mode* 'non-walls))))))
    (print-to-screen "~:[~;you see ~]~:*~{~:[~;a ~:*~a ~a~#[~;~; and ~:;, ~]~]~}~%"
		     (append
		      (loop for direction in +directions+
			    when (printp (contents (vec+ (pos *player*) direction)))
		    	      collect (name (contents (vec+ (pos *player*) direction)))
			      and collect (concatenate 'string "to the "
						       (gethash direction +direction-names+)))
		      (let ((obj (non-solid (pos *player*))))
			(when (printp obj)
			  (list (name obj) "in your space")))))))

(defun print-game ()
  (clear-screen)
  (print-board)
  (print-surroundings)
  (print-log))

(defun start ()
  (labels ((process-round (input)
	     (unless (eq input #\q)
	       (let ((action (gethash input *actions*)))
		 (when action
		   (funcall action)
		   (loop for actor being the hash-values of *solid-actors*
			 do (update actor))
		   (loop for actor being the hash-values of *non-solid-actors*
			 do (update actor))))
	       (print-game)
	       (process-round (custom-read-char)))))
    (process-round #\space))
  (format t "~c[0m" #\esc))

(defaction (#\4 #\l) "move left" (move *player* +left+))
(defaction (#\6 #\') "move right" (move *player* +right+))
(defaction (#\8 #\p) "move up" (move *player* +up+))
(defaction (#\2 #\. #\;) "move down" (move *player* +down+))
(defaction (#\9 #\[) "move up-right" (move *player* '(1 . -1)))
(defaction (#\7 #\o) "move up-left" (move *player* '(-1 . -1)))
(defaction (#\3 #\/) "move down-right" (move *player* '(1 . 1)))
(defaction (#\1 #\,) "move down-left" (move *player* '(-1 . 1)))
(defaction #\P "wait" t)
(defaction #\i "interact"
  (interact (non-solid (pos *player*)) *player*))
(defaction #\v "print inventory"
  (print-inventory))
(defaction #\D "drop an item"
  (with-item-from-inventory
      (remove-from-inventory item)
    (place item (pos *player*) :solid nil)))
(defaction #\e "equip an item"
  (with-item-from-inventory
    (let ((result (equip item *player*)))
      (cond ((listp result)
	     (print-to-log "you equipped ~a instead of ~{~a~#[~; and ~:;, ~]~}"
			   (name item) (mapcar #'name result)))
	    (result
	     (print-to-log "you equipped ~a" (name item)))))))
(defaction #\u "unequip an item"
  (let ((item-list (apply #'append (loop for i-list being the hash-values of (equipment *player*)
					 collect i-list))))
    (if item-list
	(let ((item (get-item-from-list
		     item-list
		     :naming-function (lambda (i) (log-to-string "~a (~a)" (name i) (equip-slot i)))
		     :what "item to unequip")))
	  (unequip item *player*)
	  (print-to-log "you have unequipped ~a" (name item)))
	(print-to-log "you have nothing equipped"))))
(defaction #\t "throw an item"
  (with-item-from-inventory
    (let ((target (choose-target 'free-form
				 (if (= (size item) 1)
				     3
				     1))))
      (when target
	(throw-at target item *player*)))))
(defaction #\A "attack"
  (let ((target (choose-target 'two-key (range (car (weapons *player*))))))
    (when (and target (not (listp target)))
      (attack target *player*))))
(defaction #\# "open a REPL"
  (labels ((read-and-eval (previous-input)
	     (let ((input (if previous-input
			      (concatenate 'string
					   previous-input
					   " "
					   (read-line))
			      (read-line))))
	       (if (string= "q" input)
		   'exit-repl
		   (handler-case (eval (read-from-string input))
		     (end-of-file () (read-and-eval input))))))
	   (my-repl ()
	     (format t "~&>>> ")
	     (force-output)
	     (let ((result (read-and-eval nil)))
	       (unless (eq result 'exit-repl)
		 (print result)
		 (my-repl)))))
    (my-repl)))
(defaction #\h "help"
  (loop for action being the hash-keys of *action-descriptions*
	do (print-to-log "~a: ~a" action (gethash action *action-descriptions*)))
  (print-to-log "q: quit"))

(place *player* (car (initialize-board)))
(equip (make-instance 'equipment :atk '(1 6 0 0 slashing) :name "sword" :weaponp t) *player*)
(let ((foe (make-instance 'enemy :display-char #\g :color 32 :name "goblin" :health 10)))
  (equip (make-instance 'equipment :atk '(1 4 0 0 piercing) :name 'bow :range 4 :weaponp t) foe)
  (place foe '(2 . 2)))
(place (make-instance 'equipment :name "cheese") '(4 . 6) :solid nil)

(start)
