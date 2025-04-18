(load "~/quicklisp/setup.lisp")
(ql:quickload :trivial-raw-io)

(load "utils.lisp")
(load "class-definitions.lisp")

(defparameter *player*
  (make-instance 'player :health 20 :name "player" :color 31 :illumination 5 :char #\@))

(load "class-methods.lisp")
(load "bsp-dungeon.lisp")
(load "dungeon.lisp")
(load "terminal.lisp")
(load "inventory.lisp")
(load "definition-macros.lisp")
(load "shops.lisp")
(load "codex.lisp")
(load "action-definitions.lisp")

(defparameter *print-surroundings-mode* 'my-space)
(defparameter *experience* 0)
(defparameter *level* 1)

(setf (slot-value *player* 'max-health) (health *player*))

(defmethod hostilep ((obj shopkeeper))
  (enragedp obj))

(defun increase-health ()
  (let ((health-increase (max 1 (roll 1 10 (con *player*)))))
    (incf (slot-value *player* 'max-health) health-increase)
    (incf (health *player*) health-increase)))

(defun increase-stat ()
  (let ((ability (get-item-from-list '(str con dex spd int per cha det)
				     :what "ability to increase"
				     :exit-option nil)))
    (eval `(incf (,ability *player*)))))

;; triangular numbers times 10
(defun xp-for-next-level ()
  (* *level* (1+ *level*) 5))

(defun level-up ()
  (decf *experience* (xp-for-next-level))
  (incf *level*)
  (increase-health)
  (increase-stat)
  (print-to-log "you leveled up to level ~d" *level*)
  (if (>= *experience* (xp-for-next-level))
      (level-up)))

(defun gain-experience (amount)
  (incf *experience* amount)
  (if (>= *experience* (xp-for-next-level))
      (level-up)))

(defun get-loot (obj)
  (let ((loot '()))
    (when (meat obj)
      (push (meat obj) loot))
    (loop for item in (loot obj)
	  do (push item loot))
    loot))

(defmethod drop-corpse ((obj enemy))
  (let ((corpse (make-corpse (pos obj))))
    (setf (name corpse) (log-to-string "~a corpse" (name obj)))
    (setf (loot corpse) (get-loot obj))))

(defmethod kill :before ((obj creature))
  (remove-solid (pos obj))
  (remove-glowing obj))

(defmethod kill ((obj enemy))
  (drop-corpse obj))

(defmethod kill :after ((obj enemy))
  (gain-experience (xp obj)))

(defgeneric remove-status (status)
  (:method :after ((status status))
    (setf (statuses (target status))
	  (remove status
		  (statuses (target status)))))
  (:method (status)))

(defun apply-default-colors ()
  (format t "~c[40;37m" #\esc))

(defmethod hiddenp ((obj ladder))
  (or (and (= *current-depth* 0)
	   (= -1 (direction obj)))
      (and (= *current-depth* (1- (length *layers*)))
	   (= 1 (direction obj)))))

(defmethod name ((obj ladder))
  (cond ((= -1 (direction obj))
	 'ladder-leading-up)
	((= 1 (direction obj))
	 'ladder-leading-down)))

(defun find-path (from to)
  (flood-fill from (current (if (equal current to) t) :go-until to)
	      (if result
		  (labels ((build-path (pos &optional (path nil))
			     (if (equal pos from)
				 path
				 (build-path (gethash pos cells) (cons pos path)))))
		    (build-path to))
		  (list from))))

(defgeneric apply-to (subj obj)
  (:method :before ((subj creature) (obj status))
    (setf (target obj) subj)
    (push obj (statuses subj)))
  (:method (subj obj)))

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
    (calculate-damage-modifier 1 (ensure-list damage-types))))

(defun damage (defender amount types &optional statuses)
  (let* ((base-damage (max 1 (- amount (armor defender))))
	 (mod-damage (round (* base-damage (damage-modifier defender types))))
	 (real-damage (cond ((= mod-damage 0) 0)
			    ((< mod-damage 0) mod-damage)
			    (t (max 1 mod-damage)))))
    (decf (health defender) real-damage)
    (loop for status in statuses
	  do (apply-to defender status))
    (max 0 real-damage)))

(defgeneric attack (defender attacker)
  (:method :after ((defender shopkeeper) (attacker player))
    (setf (enragedp defender) t))
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
	(print-to-log "~a hit ~a for ~d damage~:[~;, ~a~]"
		      (attack-source attack)
		      (name defender)
		      (damage defender (attack-dmg attack) (attack-types attack) (attack-statuses attack))
		      (deadp defender)
		      (death defender))
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

(defun illuminatedp (pos)
  (loop for light-source in (glowing-actors)
	  thereis (<= (distance pos (pos light-source) :exactp t)
		      (illumination light-source))))

(defgeneric visiblep (pos from)
  (:method ((pos list) (from list))
    (and (has-los pos from)
	 (illuminatedp pos)))
  (:method ((n symbol) from) nil)
  (:method ((c character) from) t)
  (:method ((pos list) (from enemy))
    (and (has-los pos (pos from))
	 (or (darkvisionp from)
	     (illuminatedp pos))))
  (:method ((obj actor) from)
    (if (hiddenp obj)
	nil
	(visiblep (pos obj) from))))

(defgeneric pickup (item)
  (:method (item))
  (:method :around ((item gold))
    (remove-non-solid (pos item))
    (incf *gold* (amount item))
    (print-to-log "you picked up ~d gold" (amount item)))
  (:method :after ((item equipment))
    (remove-non-solid (pos item))
    (when (> (illumination item) 0)
      (remove-glowing item))
    (add-to-inventory item)
    (print-to-log "you picked up a ~a" (name item))))

(defgeneric interact (object actor)
  (:method (object actor))
  (:method ((object corpse) (actor (eql *player*)))
    (mapc (lambda (i) (interact i actor)) (loot object))
    (remove-non-solid (pos object)))
  (:method ((item equipment) (actor (eql *player*)))
    (pickup item))
  (:method ((item ladder) (actor (eql *player*)))
    (unless (hiddenp item)
      (remove-solid (pos *player*))
      (remove-glowing (pos *player*))
      (incf *current-depth* (direction item))
      (setf *current-layer* (nth *current-depth* *layers*))
      (place *player* (if (= (direction item) -1)
			  (down-ladder-pos)
			  (up-ladder-pos))))))

(defgeneric heal (actor amount &key to-print)
  (:method ((actor player) amount &key to-print)
    (let ((previous-health (health actor)))
      (call-next-method)
      (if to-print
	  (print-to-log "you healed ~d" (- (health actor) previous-health))
	  (log-to-string "healed ~d" (- (health actor) previous-health)))))
  (:method ((actor creature) amount &key to-print)
    (declare (ignore to-print))
    (incf (health actor) amount)))

(defconsume quaff potion)
(defconsume eat)

(defmethod quaff ((item healing-potion) (actor player))
  (print-to-log "you quaffed ~a and ~a" (name item) (heal actor (healing item))))

(defmethod eat ((item food) (actor player))
  (incf (hunger actor) (sustenance item))
  (print-to-log "you ate ~a and recovered ~d hunger"
		(name item)
		(sustenance item)))

(defgeneric trigger (object activator)
  (:method (object activator))
  (:method :around ((trap trap) activator)
    (setf (hiddenp trap) nil)
    (if (>= (roll 1 20 (dex activator)) (avoid-dc trap))
	(print-to-log "you triggered a ~a but dodged out of the way"
		      (name trap))
	(call-next-method)))
  (:method ((trap pit-trap) (activator creature))
    (let ((damage (damage activator (roll 1 6) 'bludgeoning)))
      (when (playerp activator)
	(print-to-log "you triggered a ~a and took ~d damage"
		      (name trap)
		      damage)))))

(defgeneric move-into (passive active)
  (:method ((passive player) (active enemy))
    (attack passive active))
  (:method ((passive enemy) (active player))
    (attack passive active))
  (:method ((passive trap) (active player))
    (when (< (random 100) (trigger-chance passive))
      (trigger passive active)))
  (:method ((passive shopkeeper) (active player))
    (if (enragedp passive)
	(attack passive active)
	(let ((action (get-item-from-list '(attack sell buy) :what 'option)))
	  (cond ((eq action 'attack)
		 (attack passive active))
		((eq action 'sell)
		 (sell-item passive))
		((eq action 'buy)
		 (checkout))))))
  (:method (passive active))) ; default case: do nothing

(defun has-status-p (obj status-name)
  (member status-name (mapcar #'type-of (statuses obj))))

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
  (:method :around ((obj player) direction)
    (if *has-store-item-p*
	(let ((shopkeeper (get-shopkeeper))
	      (new-pos (vec+ (pos obj) direction)))
	  (if (and (not (solid new-pos))
		   (or (> (distance new-pos (pos shopkeeper))
			  (domain shopkeeper))
		       (not (visiblep (pos shopkeeper) new-pos))))
	      (progn (print-to-screen "if you move there, you will be stealing from the shopkeeper")
		     (when (confirm-action "move anyway")
		       (steal-items)
		       (call-next-method)))
	      (call-next-method)))
	(call-next-method)))
  (:method ((obj actor) direction)
    (reposition obj (vec+ (pos obj) direction)))
  (:method ((pos list) direction)
    (let ((newpos (vec+ pos direction)))
      (if (wallp (solid newpos))
	  pos
	  newpos))))

(defun step-towards (target obj)
  (reposition obj (car (find-path (pos obj) target))))

(defgeneric flee-direction (source obj)
  (:method ((source list) (pos list))
    (vec- pos (car (find-path pos source))))
  (:method ((source actor) (obj actor))
    (flee-direction (pos source) (pos obj))))

(defun flee (source obj)
  (move obj (flee-direction source obj)))

(defun target-too-close-p (a b range)
  (<= (distance (pos a) (pos b)) (/ range 2)))

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
  (:method ((obj resting))
    (incf (hunger (target obj)))
    (incf (health (target obj))))
  (:method :around ((obj enemy))
    (when (visiblep (pos *player*) (pos obj))
      (setf (target-pos obj) (pos *player*)))
    (loop while (>= (energy obj) 1)
	  do (decf (energy obj))
	  when (target-pos obj)
	    do (call-next-method)))
  (:method ((obj shopkeeper))
    (when (enragedp obj)
      (call-next-method)))
  (:method ((obj enemy))
    (unless (equal (pos obj) (target-pos obj))
      (let ((primary (car (weapons obj)))
	    (bravep (has-status-p obj 'brave))
	    (afraidp (has-status-p obj 'frightened))
	    (can-flee (not (solid (vec+ (pos obj) (flee-direction (target-pos obj) (pos obj)))))))
	(cond ((and (<= (health obj) (/ (max-health obj) 2))
		    (not afraidp)
		    (not bravep))
	       (let ((morale-roll (roll 3 6)))
		 (if (>= (morale obj) morale-roll)
		     (apply-to obj (make-brave-status :duration morale-roll))
		     (apply-to obj (make-frightened-status :duration morale-roll)))))
	      ((and can-flee
		    (or (target-too-close-p obj *player* (range primary))
			afraidp))
	       (flee *player* obj))
	      ((<= (distance (pos obj) (pos *player*))
		   (range primary))
	       (attack *player* obj))
	      ((not afraidp)
	       (step-towards (target-pos obj) obj)))))))

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
				(let ((contents (contents target-position)))
				  (if (and contents (not (hiddenp contents)))
				      (print-to-screen "~a" (display-char contents))
				      (print-to-screen ".")))))
			    (move-cursor (direction)
			      (when (<= (distance (vec+ target-position direction) (pos *player*))
					range)
				(undraw-cursor)
				(setf target-position (move target-position direction))))
			    (input-loop ()
			      (draw-cursor)
			      (let ((input (custom-read-char)))
				(cond ((and (eq input #\s)
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
				       (move-cursor (get-direction input))
				       (input-loop))))))
		     (input-loop))))
	  (if (eq initial-mode 'two-key)
	      (two-key-targeting)
	      (free-form-targeting)))
	(progn (print-to-log "there are no targets in range")
	       nil))))

(defgeneric throw-at (target obj thrower)
  (:method :before (target (item equipment) (thrower player))
    (declare (ignore target))
    (when (equippedp item thrower)
      (unequip item thrower))
    (remove-from-inventory item))
  (:method (target obj thrower))
  (:method ((target creature) (item equipment) (thrower creature))
    (attack target (get-attack item thrower)))
  (:method :after ((target actor) (item equipment) thrower)
    (place item (pos target) :solid nil))
  (:method :after ((target list) (item equipment) thrower)
    (place item target :solid nil)))

(defgeneric look-at (object)
  (:method (object)
    (declare (ignore object))
    (print-to-log "you see nothing there"))
  (:method ((pos list))
    (if pos
	(look-at (contents pos))
	(call-next-method)))
  (:method ((object actor))
    (if (visiblep (pos object) (pos *player*))
	(print-to-log "you see a ~a" (name object))
	(call-next-method)))
  (:method ((object trap))
    (cond ((searchedp object)
	   (call-next-method))
	  ((>= (roll 1 20 (per *player*)) (find-dc object))
	   (print-to-log "you found a ~a" (name object))
	   (setf (hiddenp object) nil))
	  (t
	   (call-next-method)))
    (setf (searchedp object) t))
  (:method ((object character))
    (if (wallp object)
	(print-to-log "you see a wall")
	(call-next-method))))    

(defun get-player-lines ()
  (flatten
   (list (log-to-string "STR ~@d  DEX ~@d  CON ~@d"
			(str *player*) (dex *player*) (con *player*))
	 (log-to-string "INT ~@d  PER ~@d  CHA ~@d"
			(intl *player*) (per *player*) (cha *player*))
	 (mapcar (lambda (weapon)
		   (log-to-string "~@:(~a~): ~a"
				  (name weapon)
				  (damage-string (atk weapon))))
		 (weapons *player*))
	 (log-to-string "HEALTH ~d/~d"
			(health *player*)
			(max-health *player*))
	 (log-to-string "EXPERIENCE ~d/~d"
			*experience*
			(xp-for-next-level))
	 (log-to-string "FOOD ~d/~d"
			(hunger *player*)
			(max-hunger *player*)))))

(defun print-surroundings ()
  (if (eq *print-surroundings-mode* 'my-space)
      (let ((obj (non-solid (pos *player*))))
	(when (and obj (not (hiddenp obj)))
	  (print-to-screen "you are standing over a ~a~%" (name obj))))
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
			      (list (name obj) "in your space"))))))))

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

(defun print-game ()
  (clear-screen)
  (print-board)
  (print-surroundings)
  (print-log))

(defun update-actors ()
  (loop for actor being the hash-values of (solid-actors)
	do (update actor))
  (loop for actor being the hash-values of (non-solid-actors)
	do (update actor)))

(defun game-over-p ()
  (or (deadp *player*)
      *game-over-p*))

(defun start ()
  (print-game)
  (labels ((process-round (input)
	     (loop repeat (resolve-action input)
		   do (update-actors))
	     (print-game)))
    (loop until (game-over-p)
	  do (process-round (custom-read-char))))
  (when (deadp *player*)
    (format t "~a has died.~c[0m~%~%" (name *player*) #\esc)))

(add-layer '((75 ((75 ((75 ((75 make-goblin)
			    (25 make-goblin-archer)))
		       (25 make-kobold)))
		  (25 make-troll)))
	     (25 make-pit-trap)))

(let ((cells (add-layer '((75 ((75 ((25 make-goblin-archer)
				    (75 make-goblin)))
			       (25 make-kobold)))
			  (25 make-pit-trap)))))
  (setf *current-layer* (car *layers*))
  (make-shopkeeper (randnth cells))
  (place *player* (randnth cells)))
(equip (make-sword) *player*)

(start)
