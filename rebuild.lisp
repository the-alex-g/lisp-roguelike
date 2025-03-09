(load "~/quicklisp/setup.lisp")
(ql:quickload :trivial-raw-io)

(load "utils.lisp")
(load "class-definitions.lisp")
(load "definition-macros.lisp")
(load "new-codex.lisp")
(load "terminal.lisp")
(load "inventory.lisp")
(load "bsp-dungeon.lisp")

(defparameter *solid-actors* (make-hash-table :test #'equal))
(defparameter *non-solid-actors* (make-hash-table :test #'equal))
(defparameter *glowing-actors* nil)
(defparameter *board-size* '(60 . 20))
(defparameter *player* (make-instance 'player :health 100 :name "player" :color 31 :illumination 5))
(defparameter *sight-distance* 10)
(defparameter *actions* (make-hash-table))
(defparameter *action-descriptions* (make-hash-table))
(defparameter *print-surroundings-mode* 'none)

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

(defun add-glowing (actor)
  (push actor *glowing-actors*))

(defun remove-glowing (actor)
  (setf *glowing-actors* (remove actor *glowing-actors*)))

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

(defgeneric remove-status (status)
  (:method :after ((status status))
    (setf (statuses (target status))
	  (remove status
		  (statuses (target status)))))
  (:method (status)))

(defgeneric wallp (obj)
  (:method (obj) nil)
  (:method ((obj list)) (wallp (solid obj)))
  (:method ((obj symbol)) (eq obj 'wall))
  (:method ((obj character)) t))

(defmethod name ((obj character)) "wall")
(defmethod name ((obj symbol)) obj)

(defun apply-default-colors ()
  (format t "~c[40;37m" #\esc))

(defun apply-colors (arg colors)
  (format nil "~c[~{~d~^;~}m~a~0@*~c[40;37m"
	  #\esc (ensure-list colors)
	  arg))

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
      (setf (pos obj) result)))
  (when (> (illumination obj) 0)
    (add-glowing obj)))

(defun get-spawn-list ()
  '((50 ((75 make-goblin)
	 (25 make-kobold)))
    (50 make-food-pickup)))

(defun spawn-object (pos)
  (funcall (car (eval-weighted-list (get-spawn-list))) pos))

(defun populate-dungeon (region sparseness)
  (loop for cell in region
	when (= (random sparseness) 0)
	  do (spawn-object cell)))

(defun initialize-board ()
  (setf *solid-actors* (make-hash-table :test #'equal))
  (setf *non-solid-actors* (make-hash-table :test #'equal))
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
    (populate-dungeon cells 12)
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
  (:method :before ((obj creature))
    (remove-solid (pos obj))
    (remove-glowing obj))
  (:method ((obj enemy))
    (place (corpse obj) (pos obj) :solid nil)))

(defmethod (setf health) (value (obj creature))
  (if (> value 0)
      (if (slot-boundp obj 'max-health)
	  (setf (slot-value obj 'health) (min value (max-health obj)))
	  (progn (setf (slot-value obj 'max-health) (health obj))
		 (setf (slot-value obj 'health) value)))
      (progn (setf (slot-value obj 'health) 0)
	     (kill obj))))

(defmethod (setf con) (value (obj creature))
  (let ((dhealth (* (- value (con obj)) (ash (hd obj) -2))))
    (incf (slot-value obj 'max-health)
	  dhealth)
    (incf (health obj) dhealth)))

(defmethod (setf hunger) (value (obj player))
  (cond ((> value (max-hunger obj))
	 (setf (slot-value obj 'hunger) (max-hunger obj)))
	((< value 0)
	 (setf (slot-value obj 'hunger) 20)
	 (decf (health obj)))
	(t
	 (setf (slot-value obj 'hunger) value))))

(defmethod (setf illumination) (value (obj actor))
  (let ((current-value (illumination obj)))
    (when (and (<= current-value 0)
	       (> value 0))
      (add-glowing obj))
    (when (and (> current-value 0)
	       (<= value 0))
      (remove-glowing obj)))
  (setf (slot-value obj 'illumination) value))
  
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

(defgeneric apply-to (subj obj)
  (:method :before ((subj creature) (obj status))
    (setf (target obj) subj)
    (push obj (statuses subj)))
  (:method (subj obj)))

(defun roll (num die &rest modifiers)
  (+ (loop repeat num
	   sum (1+ (random die)))
     (loop for m in modifiers
	   sum m)))

(defequipment fist () :atk '(1 3 -1 0 bludgeoning) :weaponp t)

(defgeneric weapons (obj)
  (:method ((obj creature))
    (let ((held-items (gethash 'hand (equipment obj))))
      (cond ((= (length held-items) 1)
	     held-items)
	    ((= (length held-items) 0)
	     (list (make-fist)))
	    (t
	     (loop for equipment in held-items
		   when (weaponp equipment)
		     collect equipment))))))

(flet ((generate-attack (attacker num die &optional dmg-bonus to-hit types statuses)
	 (make-attack :dmg (roll num die (str attacker) dmg-bonus)
		      :to-hit (roll 1 20 to-hit (dex attacker))
		      :source (name attacker)
		      :types (ensure-list types)
		      :statuses (ensure-list statuses))))
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
    (loop for status in (attack-statuses attack)
	  do (apply-to defender status))
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
    (defun has-los (to from)
      (let ((key (list to from)))
	(multiple-value-bind (memo memo-existsp) (gethash key memos)
	  (if memo-existsp
	      memo
	      (let ((new-val (calculate-los to from)))
		(setf (gethash key memos) new-val)
		new-val)))))))

(defgeneric pickup (item)
  (:method (item))
  (:method :after ((item equipment))
    (remove-non-solid (pos item))
    (when (> (illumination item) 0)
      (remove-glowing item))
    (add-to-inventory item)
    (print-to-log "you picked up a ~a" (name item))))

(defgeneric interact (object actor)
  (:method (object actor))
  (:method ((item equipment) (actor (eql *player*)))
    (pickup item)))

(defgeneric move-into (passive active)
  (:method ((passive player) (active enemy))
    (attack passive active))
  (:method ((passive enemy) (active player))
    (attack passive active))
  (:method (passive active))) ; default case: do nothing

(defun has-status-p (obj status-name)
  (member status-name (mapcar #'type-of (statuses obj))))

(defstatus frightened)
(defstatus brave)
(defstatus immobilized)

(defgeneric reposition (obj new-pos)
  (:method :around ((obj creature) new-pos)
    (unless (has-status-p obj 'immobilized)
      (call-next-method)))
  (:method ((obj actor) new-pos)
    (let ((collider (solid new-pos)))
      (if collider
	  (move-into collider obj)
	  (progn (remove-solid (pos obj))
		 (setf (solid new-pos) obj)
		 (setf (pos obj) new-pos)
		 (move-into (non-solid new-pos) obj))))))

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
  (:method (obj))
  (:method :around ((obj status))
    (if (= (duration obj) 0) ;; = 0 so that a status with negative duration is permanent
	(remove-status obj)
	(when (>= (energy obj) 1)
	  (decf (energy obj))
	  (when (next-method-p)
	    (call-next-method))
	  (decf (duration obj))
	  (act obj))))
  (:method :around ((obj enemy))
    (when (>= (energy obj) 1)
      (decf (energy obj))
      (when (visiblep (pos *player*) (pos obj))
	(setf (target-pos obj) (pos *player*)))
      (call-next-method)
      (act obj)))
  (:method ((obj enemy))
    (unless (equal (pos obj) (target-pos obj))
      (let ((primary (car (weapons obj)))
	    (bravep (has-status-p obj 'brave))
	    (afraidp (has-status-p obj 'frightened)))
	(cond ((and (<= (health obj) (/ (max-health obj) 2))
		    (not afraidp)
		    (not bravep))
	       (let ((morale-roll (roll 3 6)))
		 (if (>= (morale obj) morale-roll)
		     (apply-to obj (make-brave-status :duration morale-roll))
		     (apply-to obj (make-frightened-status :duration morale-roll)))))
	      (afraidp
	       (flee *player* obj))
	      ((and (<= (distance (pos obj) (pos *player*))
			(/ (range primary) 2))
		    (not (solid (vec+ (pos obj) (flee-direction *player* obj)))))
	       (flee *player* obj))
	      ((<= (distance (pos obj) (pos *player*))
		   (range primary))
	       (attack *player* obj))
	      (t
	       (step-towards *player* obj)))))))

(defgeneric update (obj)
  (:method (obj))
  (:method ((obj status))
    (incf (energy obj) (/ (spd obj) (spd *player*)))
    (act obj))
  (:method :before ((obj creature))
    (mapc #'update (statuses obj)))
  (:method ((obj player))
    (decf (hunger obj)))
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
				     when (and (visiblep pos (pos *player*))
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

(defun illuminatedp (pos)
  (loop for light-source in *glowing-actors*
	  thereis (<= (distance pos (pos light-source) :exactp t)
		      (illumination light-source))))

(defgeneric visiblep (pos from)
  (:method ((pos list) (from list))
    (and (has-los pos from)
	 (illuminatedp pos)))
  (:method ((n symbol) from) nil)
  (:method ((c character) from) t)
  (:method ((obj actor) from)
    (if (hiddenp obj)
	nil
	(visiblep (pos obj) from))))

(defun get-player-lines ()
  (flatten
   (list (log-to-string "STR ~@d  DEX ~@d  CON ~@d"
			(str *player*) (dex *player*) (con *player*))
	 (log-to-string "INT ~@d  PER ~@d  CHA ~@d"
			(intl *player*) (per *player*) (cha *player*))
	 (mapcar (lambda (weapon)
		   (let ((atk (atk weapon)))
		     (setf (nth 4 atk) (ensure-list (nth 4 atk)))
		     (apply #'log-to-string
			    "~@:(~a~): ~dd~d~[~:;~:*~@d~]~5@*~{ ~a~} damage~
                             ~4@*~[~:;~:*, ~@d to hit~]"
			    (name weapon)
			    atk)))
		 (weapons *player*))
	 (log-to-string "HEALTH ~d/~d"
			(health *player*)
			(max-health *player*))
	 (log-to-string "FOOD ~d/~d"
			(hunger *player*)
			(max-hunger *player*)))))

(defun print-board ()
  (apply-default-colors)
  (let ((player-lines (get-player-lines)))
    (loop for y from -1 to (1+ (cdr *board-size*))
	  do (format t "~{~a~}~a~%"
		     (loop for x from -1 to (1+ (car *board-size*))
			   collect (let* ((pos (cons x y))
					  (actor (contents pos)))
				     (cond ((characterp actor)
					    actor)
					   ((and (wallp actor)
						 (or (visiblep pos (pos *player*))
						     (visiblep actor (pos *player*))))
					    (display-char pos))
					   ((and actor (visiblep actor (pos *player*)))
					    (display-char actor))
					   ((visiblep pos (pos *player*)) #\.)
					   (t #\space))))
		     (if (nth (1+ y) player-lines)
			 (nth (1+ y) player-lines)
			 "")))))

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
  (print-game)
  (labels ((process-round (input)
	     (unless (eq input #\q)
	       (let ((action (gethash input *actions*)))
		 (when action
		   (funcall action)
		   (update *player*)
		   (loop for actor being the hash-values of *solid-actors*
			 do (update actor))
		   (loop for actor being the hash-values of *non-solid-actors*
			 do (update actor))))
	       (print-game)
	       (not (deadp *player*)))))
    (loop while (process-round (custom-read-char))))
  (when (deadp *player*)
    (format t "~a has died.~c[0m~%~%" (name *player*) #\esc)))

(place *player* (car (initialize-board)))
(equip (make-sword) *player*)

(load "action-definitions.lisp")

(start)
