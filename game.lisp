(defparameter *notes* ())

(defmethod look ((at trap))
  (if (hiddenp at)
      (if (and (discoverable at) (check 11 #'per *player*))
	  (setf (hiddenp at) nil))
	  (setf (discoverable at) nil))
      (name at))

(defmethod add-to-inventory ((item gold))
  (incf *gold* (amount item))
  (print-to-log "you have picked up ~d gold coins" (amount item))
  nil)

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

(defmethod update ((obj glowing-mushroom-actor))
  (loop for p being the hash-keys of (board)
	when (and (has-los p (pos obj) (glow-radius obj))
		  (has-los (pos *player*) p -1))
	  do (push p *light-zone*))
  (update-spaces-found))

(defmethod get-ascii ((obj glowing-mushroom-actor))
  (if *in-terminal*
      (apply-color (display-char obj) (color obj) 'uline)
      (display-char obj)))

(defmethod get-ascii ((obj herb))
  (if *in-terminal*
      (apply-color (display-char obj) 'uline (color obj))
      (display-char obj)))

(defmethod interact ((a player) (b glowing-mushroom-actor))
  (print-to-log "you picked a clump of glowing mushrooms")
  (add-to-inventory (make-glowing-mushrooms)))

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
  (damage target (health item) :damage-types '(poison))
  (print-to-log "~a ate ~a and lost ~a health" (name target) (name item) (health item)))

(defmethod eat ((item healing-potion) (target actor))
  (incf (health target) (health item))
  (print-to-log "~a drank ~a and regained ~a health"
		(name target) (name item) (health item)))

(defmethod eat ((item poison-potion) (target actor))
  (damage target (health item) :damage-types '(poison))
  (print-to-log "~a drank ~a and lost ~a health"
		(name target) (name item) (health item)))

(defmethod eat ((item explosive-potion) (target actor))
  (damage target (explode-damage item) :damage-types '(fire))
  (print-to-log "the ~a explodes, dealing ~a damage to ~a"
		(name item) (explode-damage item) (name target)))

(defmethod eat :around ((item bottle) (target actor))
  (if (contents item)
      (progn (eat (contents item) target)
	     (setf (contents item) nil))
      (call-next-method)))

(defmethod (setf identifiedp) (new-val (obj bottle))
  (setf (identifiedp (contents obj)) new-val))

(defmethod (setf contents) ((new-val equipment) (obj bottle))
  (setf (slot-value obj 'contents) new-val)
  (setf (container new-val) obj))

(defmethod (setf contents) (new-val (obj bottle))
  (setf (slot-value obj 'contents) nil))

(defmethod color ((obj bottle))
  (if (contents obj)
      (color (contents obj))
      (slot-value obj 'color)))

(defmethod make-pickup ((item glowing-mushrooms) pos)
  (make-glowing-mushroom-actor pos))

(defgeneric bottle-name (item)
  (:method ((item equipment))
    (log-to-string "bottle of ~a" (name item)))
  (:method ((item potion))
    (name item)))

(defmethod name ((item bottle))
  (let ((base-name (if (contents item)
		       (bottle-name (contents item))
		       (slot-value item 'name))))
    (if (shopkeeper item)
	(log-to-string "~a (~d gold)" base-name (price (contents item)))
	base-name)))

(defmethod throw-at ((item bottle) target)
  (when (contents item)
    (throw-at (contents item) target)))

(defmethod throw-at :around ((item explosive-potion) (target actor))
  (call-next-method item (pos target)))

(defmethod throw-at ((item explosive-potion) target)
  (print-to-log "it explodes for ~d damage" (explode-damage item))
  (for-each-adjacent-actor target
			   (save 12 dex actor
				 (damage actor (explode-damage item))
				 (damage actor (ash (explode-damage item) -1)))
			   (when (and (deadp actor)
				      (destructible actor))
			     (print-to-log "~a a ~a"
					   (death-verb actor)
					   (name actor)))))

(defmethod throw-at ((item poison-potion) (target actor))
  (print-to-log "the potion splashes on ~a, dealing ~d damage"
		(name target)
		(save 15 dex target
		      (damage target (health item) :damage-types '(poison))
		      (damage target (ash (health item) -1) :damage-types '(poison)))))

(defmethod apply-to ((item herb) (target actor))
  (eat item target))

(defmethod apply-to ((item equipment) (to bottle))
  (cond ((contents to)
	 (print-to-log "the bottle is already full"))
	((not (consumable item))
	 (print-to-log "you can't bottle that"))
	(t
	 (setf (contents to) item)
	 (destroy item)
	 (print-to-log "you have bottled ~a" (name item)))))

(defmethod apply-to (item (to bottle))
  (print-to-log "you can't bottle that"))

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
					 (if (and (deadp target)
						  (not already-dead))
					     (log-to-string ", killing the ~a"
							    (name target))
					     ""))
			   (if (and (deadp target) (not already-dead))
			       (print-to-log "the ~a dies" (name target))))))
	 (onetime-effects to)))

(defmethod apply-to ((item identify-scroll) (to pickup))
  (apply-to item (equipment to)))

(defmethod apply-to ((item identify-scroll) (to equipment))
  (if (identifiedp to)
      (let ((old-name (name to)))
	(setf (identifiedp to) t)
	(print-to-log "the ~a has been revealed to be a ~a" old-name (name to)))
      (print-to-log "there is nothing hidden about that object")))

(defmethod interact ((a player) (b trap))
  (when (> (trigger-chance b) (random 100))
    (setf (hiddenp b) nil)
    (save (save-dc b) dex a
	  (let ((attack (eval-attack (atk b))))
	    (when (one-use-p b)
	      (destroy b))
	    (print-to-log "you ~a ~a and took ~d damage"
			  (verb b)
			  (name b)
			  (damage a (cadr (assoc 'dmg attack))
				  :damage-types (cdr (assoc 'dmg-types
							    attack)))))
	  (print-to-log "you avoided ~a" (name b)))))

(defmethod move ((obj spider) (distance list))
  (let* ((newpos (add-pos (pos obj) distance))
	 (collider (find-solid-actor-at newpos)))
    (flet ((move-and-collide ()
	     (mapc (lambda (actor) (unless (interact-action-only actor)
				     (interact obj actor)))
		   (find-all-actors-at newpos))
	     (setf (pos obj) newpos)))
      (cond ((slot-exists-p collider 'webbingp)
	     (move-and-collide))
	    (collider
	     (unless (interact-action-only collider)
	       (interact obj collider)))
	    (t
	     (move-and-collide))))))

(defmethod update :before ((obj spider))
  (when (enabled obj)
    (if (= (web-cooldown obj) 0)
	(progn (make-webbing (pos obj))
	       (setf (web-cooldown obj) 20))
	(decf (web-cooldown obj)))))

(defmethod make-corpse ((obj ooze))
  (setf (color (make-acid-pool (pos obj))) (color obj)))

(defmethod make-corpse ((obj skeleton))
  (setf (bone-type (make-bones (pos obj))) (name obj))
  (mapc (lambda (equipment)
	  (make-pickup equipment (pos obj)))
	(get-loot obj)))

(defmethod make-corpse ((obj wraith)))

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
  (print-damage-results a "~a walked into fire and took ~a damage"
			(name a)
			(damage a (roll (dmg b)))))

;; equip player
(equip (make-bow) *player*)
(equip (make-leather-armor) *player*)

;; put some stuff in the inventory
(loop repeat 3 do (push (make-food) *inventory*))
(push (make-bottle) *inventory*)

(defgeneric cook (item)
  (:method (item)
    (print-to-log "you can't cook that"))
  (:method :before ((item equipment))
    (unless (containedp item)
      (remove-from-inventory item)
      (add-to-inventory item)))
  (:method :before ((item food))
    (if (= (cookedp item) 0)
	(progn (incf (hunger item) 10)
	       (print-to-log "you have cooked ~a" (name item)))
	(progn (decf (hunger item) 15)
	       (if (> (hunger item) 0)
		   (print-to-log "you have burnt ~a" (name item))
		   (progn (print-to-log "you have incinerated ~a" (name item))
			  (destroy item)))))
    (incf (cookedp item)))
  (:method ((item food)))
  (:method ((item poison-rat-meat))
    (decf (health item))
    (when (<= (health item) 1)
      (setf (health item) 0)
      (setf (poisonp item) nil)))
  (:method ((item faggot))
    (decf (burn-time item) 5)
    (cond ((<= (burn-time item) 0)
	   (remove-from-inventory item)
	   (add-to-inventory (make-coal))
	   (print-to-log "you have turned the faggot into coal"))
	  (t
	   (setf (name item) "charred faggot")
	   (print-to-log "you have charred the faggot"))))
  (:method ((item bottle))
    (if (contents item)
	(cook (contents item))
	(print-to-log "you can't cook that"))))

(macrolet ((make-brewing-method (in out)
	     `(defmethod cook ((item ,in))
		(when (containedp item)
		  (let ((potion (,(constructor out) :without-bottle t)))
		    (print-to-log "you have brewed a ~a" (name potion))
		    (setf (contents (container item)) potion))))))
  (make-brewing-method healing-herb healing-potion)
  (make-brewing-method poison-herb poison-potion))

(defmethod name ((obj food))
  (log-to-string "~a~a"
		 (cond ((= (cookedp obj) 0) "")
		       ((= (cookedp obj) 1) "cooked ")
		       (t "burnt "))
		 (call-next-method)))

(defun ranged-attack ()
  (let ((target (get-target-within-range (pos *player*) (range (gethash 'hand (equips *player*))))))
    (when target
      (attack *player* target))))

;; define actions
(defaction #\a "move west" (move *player* +left+))
(defaction #\d "move east" (move *player* +right+))
(defaction #\w "move north" (move *player* +up+))
(defaction #\s "move south" (move *player* +down+))
(defaction #\A "attack without moving"
  (if (slot-exists-p (gethash 'hand (equips *player*)) 'range)
      (ranged-attack)
      (let ((direction (get-direction :include-zero t)))
	(when direction
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
      (look position))))
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
(defaction #\n "read or take notes"
  (let ((option (get-item-from-list '(read-notes write-note))))
    (cond ((eq option 'read-notes)
	   (mapc #'print-to-log (reverse *notes*)))
	  ((eq option 'write-note)
	   (print-to-screen "~&write your note~%")
	   (let ((note (with-color-applied 'grey
			 (read-line))))
	     (print-to-log "added note \"~a\"" note)
	     (push (format nil "~a] ~a" (game-date) note) *notes*))))))

;; start game
(start)
