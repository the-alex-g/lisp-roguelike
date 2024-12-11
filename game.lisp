(load "./game-engine.lisp")
(load "./bsp-dungeon.lisp")
(load "./codex.lisp")

(defmethod description ((obj trap))
  (if (hiddenp obj)
      (cond ((and (discoverable obj) (>= (roll 20) 11))
	     (print-to-log "you discovered ~a" (slot-value obj 'description))
	     (setf (hiddenp obj) nil))
	    (t (setf (discoverable obj) nil) nil))
      (slot-value obj 'description)))

(defmethod update ((obj fire))
  (decf (burn-time obj))
  (loop for actor in (find-all-actors-at obj)
	do (if (slot-exists-p actor 'equipment)
	       (when (>= (burn-time (equipment actor)) 0)
		 (incf (burn-time obj) (burn-time (equipment actor)))
		 (destroy actor)
		 (print-to-log "the fire has consumed ~a" (name actor)))
	       (progn (damage actor (roll (dmg obj)))
		      (when (deadp actor)
			(print-to-log "the fire has consumed ~a" (name actor))))))
  (when (<= (burn-time obj) 0)
    (when (visiblep obj)
      (print-to-log "the fire has gone out"))
    (destroy obj)))

(defgeneric death-verb (obj)
  (:method ((obj actor))
    "destroying")
  (:method ((obj combat-entity))
    "killing"))

;;; custom eat function for food
(defmethod eat ((item food) (target actor))
  (if (poisonp item)
      (progn
	(print-to-log "You ate ~a, lost ~a health, and recovered ~d hunger~%"
		      (name item) (health item) (hunger item))
	(decf (health target) (health item)))
      (progn
	(print-to-log "You ate ~a~a and recovered ~d hunger~%"
		      (name item)
		      (if (> (health item) 0)
			  (log-to-string ", regained ~d health, " (health item))
			  "")
		      (hunger item))
	(incf (health target) (health item))))
  (incf (hunger target) (hunger item)))

(defmethod eat :before ((item herb) (target player))
  (incf (hunger target) (hunger item)))

(defmethod eat ((item healing-herb) (target actor))
  (incf (health target) (health item))
  (print-to-log "~a ate ~a and regained ~a health"
		(name target) (name item) (health item)))

(defmethod eat ((item poison-herb) (target actor))
  (damage target (health item))
  (print-to-log "~a ate ~a and lost ~a health" (name target) (name item) (health item)))

(defmethod throw-at :around ((item bomb) (target actor))
  (call-next-method item (pos target)))

(defmethod throw-at ((item bomb) target)
  (print-to-log "it explodes for ~d damage~%" (explode-damage item))
  (for-each-adjacent-actor target
			   (save 12 dex actor
				 (damage actor (explode-damage item))
				 (damage actor (ash (explode-damage item) -1)))
			   (when (and (deadp actor)
				      (destructible actor))
			     (print-to-log "~a a ~a~%"
					   (death-verb actor)
					   (name actor)))))

(defmethod apply-to ((item herb) (target actor))
  (eat item target))

(defmethod apply-to ((item poison-herb) (to weapon))
  (print-to-log "you have applied ~a to ~a" (name item) (name to))
  (push (make-status
	 0
	 :on-applied (let ((already-dead (deadp target)))
		       (damage target (health item))
		       (if (identifiedp item)
			   (print-to-log "the ~a on the ~a deals ~d damage~a"
					 (name item)
					 (name to)
					 (health item)
					 (if (and (deadp target) (not already-dead))
					     (log-to-string ", killing the ~a"
							    (name target))
					     ""))
			   (if (and (deadp target) (not already-dead))
			       (print-to-log "the ~a dies" (name target))))))
	 (onetime-effects to)))

(defmethod interact ((a player) (b trap))
  (when (= 0 (random 2))
    (setf (hiddenp b) nil)
    (save (save-dc b) dex a
	  (print-to-log "you triggered ~a but dodged out of the way" (description b))
	  (let ((attack (eval-attack (atk b))))
	    (print-to-log "you triggered ~a and took ~d damage"
			  (description b) (damage a (cadr (assoc 'dmg attack))
						  :damage-types (cdr (assoc 'dmg-types attack))))))))

(defmethod damage :after ((target ooze) dmg &key unblockable damage-types)
  (declare (ignore dmg unblockable))
  (when (and (>= (health target) 2)
	     (loop for x in (split-damage-types target)
		     thereis (member x damage-types)))
    (let ((new-health (ash (health target) -1))
	  (new-ooze (loop for p in +directions+
			  unless (find-solid-actor-at p)
			    return (funcall (build-function target)
					    (add-pos p (pos target))))))
      (setf (health target) new-health)
      (when new-ooze
	(print-to-log "the ~a splits into two smaller oozes" (name target))
	(setf (health new-ooze) new-health)))))
 
(defmethod interact ((a combat-entity) (b fire))
  (print-to-log "~a walked into fire and took ~a damage~a" (name a)
		(damage a (roll (dmg b)))
		(if (deadp a) ", killing it" "")))

;; generate a sample board
(make-layer (generate-dungeon '(50 . 20) 3
			      (list (list #'make-ogre #'make-goblin
					  #'make-goblin #'make-rat #'make-grey-slime)
				    (list #'make-food-pickup
					  #'make-poison-herb-pickup
					  #'make-healing-herb-pickup)
				    #'make-trap)))
(make-layer (generate-dungeon '(50 . 20) 3
			      (list (list #'make-goblin #'make-goblin
					  #'make-rat #'make-grey-slime)
				    (list #'make-food-pickup
					  #'make-poison-herb-pickup
					  #'make-healing-herb-pickup)
				    #'make-trap)))

;; equip player
(equip (make-big-sword) *player*)
(equip (make-leather-armor) *player*)

;; put some stuff in the inventory
(loop repeat 3 do (push (make-faggot) *inventory*))
(push (make-bomb) *inventory*)
(push (make-bow) *inventory*)

(defgeneric cook (item)
  (:method (item)
    (print-to-log "you can't cook that"))
  (:method :around ((item food))
    (if (cookedp item)
	(print-to-log "you have already cooked that")
	(progn
	  (remove-from-inventory item)
	  (incf (hunger item) 10)
	  (setf (cookedp item) t)
	  (print-to-log "you have cooked ~a" (name item))
	  (call-next-method))))
  (:method :after ((item food))
    (add-to-inventory item))
  (:method ((item food))
    (setf (name item) (log-to-string "cooked ~a" (name item))))
  (:method ((item poison-rat-meat))
    (when (<= (health item) 2)
      (setf (health item) 0)
      (setf (name item) (log-to-string "cooked ~a" (name item)))
      (setf (poisonp item) nil))
    (setf (fake-name item) (log-to-string "cooked ~a" (name item)))))

(defun ranged-attack (direction)
  (loop for r from 1 to (1- (range (gethash 'hand (equips *player*))))
	do (let ((d (find-solid-actor-at
		     (add-pos (pos *player*)
			      (mul-pos direction r)))))
	     (when d
	       (attack *player* d)
	       (return nil)))))

;; define actions
(defaction #\a "move west" (move *player* +left+))
(defaction #\d "move east" (move *player* +right+))
(defaction #\w "move north" (move *player* +up+))
(defaction #\s "move south" (move *player* +down+))
(defaction #\A "attack without moving"
    (let ((direction (get-direction :include-zero t)))
      (when direction
	(if (slot-exists-p (gethash 'hand (equips *player*)) 'range)
	    (ranged-attack direction)
	    (let ((actor (choose-actor-at (add-pos (pos *player*)
						   direction))))
	      (when actor
		(attack *player* actor)))))))
(defaction #\D "drop an inventory item"
  (with-item-from-inventory
      (make-pickup item (pos *player*))
    (remove-from-inventory item)
    (print-to-log "you dropped ~a" (name item))))
(defaction #\i "interact with an object on your space"
  (let ((actor (choose-actor-at *player*)))
    (when actor
      (interact *player* actor))))
(defaction #\e "equip an inventory item"
  (with-item-from-inventory
      (let ((output "")
	    (old-item (equip item *player*)))
	(when (not (eq old-item 'failed))
	  (setf output (log-to-string "You have equipped ~a" (name item)))
	  (remove-from-inventory item)
	  (when old-item
	    (setf output (concatenate 'string output
				      (log-to-string " instead of ~a"
						     (name old-item))))
	    (add-to-inventory old-item)))
	(print-to-log output))))
(defaction #\m "eat an inventory item"
  (with-item-from-inventory (eat item *player*)))
(defaction #\v "print inventory" (print-inventory))
(defaction #\l "look"
    (let ((position (get-direction :include-zero t)))
      (when position
	(let ((actors (find-all-actors-at (add-pos (pos *player*) position)
					  *player*)))
	  (if actors
	      (let ((something-found-p nil))
		(mapc (lambda (actor)
			(let ((d (description actor)))
			  (unless (hiddenp actor)
			    (setf something-found-p t)
			    (when d
			      (print-to-log "You see ~a" d)))))
		      actors)
		(unless something-found-p
		  (print-to-log "there's nothing there")))
	      (print-to-log "there's nothing there"))))))
(defaction #\u "unequip an item"
  (let ((item (get-item-from-list
	       (loop for x being the hash-values of (equips *player*)
		     collect x)
	       :naming-function (lambda (x)
				  (log-to-string "~a: ~a"
						 (equip-slot x)
						 (name x))))))
    (when item
      (remhash (equip-slot item) (equips *player*))
      (add-to-inventory item)
      (print-to-log "you have unequipped ~a, freeing your ~a slot~%"
		    (name item)
		    (equip-slot item)))))
(defaction #\t "throw an item"
  (with-item-from-inventory
      (let* ((direction (get-direction :cancel nil))
	     (distance (get-number-input (throw-distance item)
					 "enter the distance you want to throw"))
	     (final-pos (loop for x downfrom distance to 0
			      when (gethash (add-pos (pos *player*)
						     (mul-pos direction x))
					    (board))
				return (add-pos (pos *player*) (mul-pos direction x))))
	     (target (choose-actor-at final-pos)))
	(throw-at item (if target target final-pos)))))
(defaction #\h "print help menu"
  (loop for k being the hash-keys of *action-descriptions*
	do (print-to-log "~c: ~a~%" k (gethash k *action-descriptions*)))
  (print-to-log "q: quit~%"))
(defaction #\b "burn an item"
  (with-item-from-inventory
      (if (> (burn-time item) 0)
	  (let ((fire-pos (add-pos (get-direction) (pos *player*))))
	    (unless (loop for actor in (find-all-actors-at fire-pos)
			  when (eq (name actor) 'fire)
			    do (progn (incf (burn-time actor) (burn-time item))
				      (print-to-log "you have fed the fire with ~a"
						    (name item))
				      (return t)))
	      (setf (burn-time (make-fire fire-pos)) (burn-time item))
	      (print-to-log "you have started a fire with ~a" (name item)))
	    (remove-from-inventory item))
	  (print-to-log "that doesn't burn"))))
(defaction #\c "cook an item"
  (if (loop for p in (list +left+ +right+ +zero+ +up+ +down+)
	      thereis (loop for actor in (find-all-actors-at (add-pos p (pos *player*)))
			      thereis (eq (name actor) 'fire)))
      (with-item-from-inventory (cook item))
      (print-to-log "there is no fire nearby")))
(defaction #\r "apply an item"
  (let* ((applied-item (with-item-from-inventory item))
	 (choice (get-item-from-list '(nearby-object equipped-item inventory-item)
				     :what "what to apply to"))
	 (item (cond ((eq choice 'inventory-item)
		      (with-item-from-inventory item))
		     ((eq choice 'equipped-item)
		      (get-item-from-list
		       (loop for item being the hash-values of (equips *player*)
			     collect item)
		       :what (log-to-string "item to apply ~a to" (name applied-item)) 
		       :naming-function #'name))
		     ((eq choice 'nearby-object)
		      (choose-actor-at (add-pos (pos *player*)
						(get-direction
						 :cancel nil :include-zero t)))))))
    (when (and applied-item item)
      (apply-to applied-item item))))
(defaction #\# "open a REPL"
  (labels ((my-repl ()
	     (fresh-line)
	     (princ ">>> ")
	     (force-output)
	     (let ((input (read-from-string (read-line))))
	       (unless (eq input 'q)
		 (print (eval input))
		 (my-repl)))))
    (my-repl)))

;; start game
(start)
