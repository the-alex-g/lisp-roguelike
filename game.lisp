(load "./game-engine.lisp")
(load "./bsp-dungeon.lisp")

(defactor trap #\! ((dmg 4) (save-dc 10) (discoverable t))
  :interact-action-only nil :solid nil :destructible nil :color 'red :hiddenp t
  :description "a cunning trap")
(defactor fire #\^ (burn-time (dmg 6)) :interact-action-only nil
  :solid nil :destructible nil
  :color 'red :description "a fire" :dynamicp t)

(defmethod description ((obj trap))
  (if (hiddenp obj)
      (cond ((and (discoverable obj) (>= (roll 20) 11))
	     (print-to-log "you discovered ~a" (slot-value obj 'description))
	     (setf (hiddenp obj) nil))
	    (t (setf (discoverable obj) nil) nil))
      (slot-value obj 'description)))

(defequipment herb ((hunger (roll 5))) :consumable t :burn-time 1)

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

(defmacro defherb (real-name &rest slots)
  (let* ((herb-config (randnth '((herb #\v) (lichen #\_) (fungus #\f)
				 (wort #\w) (cress #\%))))
	 (herb-type (car herb-config))
	 (herb-char (cadr herb-config))
	 (herb-color (random-color)))
    `(defequipment ,real-name nil ,@slots
       :color (quote ,herb-color) :display-char ,herb-char
       :fake-name ,(log-to-string "~a ~a" herb-color herb-type)
       :inherit herb :identifiedp nil)))

;; define equipment types
(defequipment food ((hunger (+ 20 (random 11))) (poisonp nil) (cookedp nil))
  :health (if (= (random 5) 0) 1 0) :consumable t
  :description "food")
(defequipment rat-meat () :hunger (+ 10 (random 6)) :secretp t
  :inherit food :description "rat meat")
(defequipment poison-rat-meat () :poisonp t
  :health (roll 3) :hunger (+ 6 (random 4))
  :inherit rat-meat :fake-name "rat meat")
(let ((bomb-color (random-color)))
  (defequipment bomb ((explode-damage (+ (roll 4) (roll 4))))
    :identifiedp nil :fake-name (log-to-string "~a potion" bomb-color)
    :throw-distance 3 :breakable t))
(defequipment faggot () :burn-time (+ 10 (random 11)) :dmg 2 :weaponp t)
(defherb healing-herb :health (roll 4))
(defherb poison-herb :health (roll 4))
(defequipment ranged-weapon (range) :dex -2 :weaponp t)
(defequipment bow () :dmg 4 :range 4 :description "a bow" :inherit ranged-weapon)
(defequipment sword nil :dmg 6 :weaponp t :description "a sword")
(defequipment big-sword nil :dmg 8 :weaponp t :description "a big sword")
(defequipment leather-armor nil :def 1 :description "leather armor" :equip-slot 'body)

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
  (print-to-log "You ate ~a and regained ~a health" (name item) (health item)))

(defmethod eat ((item poison-herb) (target actor))
  (decf (health target) (health item))
  (print-to-log "You ate ~a and lost ~a health" (name item) (health item)))

(defmethod break-at (pos (item bomb))
  (print-to-log "it explodes for ~d damage~%" (explode-damage item))
  (for-each-adjacent-actor pos
			   (save 12 dex actor
				 (damage actor (explode-damage item))
				 (damage actor (ash (explode-damage item) -1)))
			   (when (and (deadp actor)
				      (destructible actor))
			     (print-to-log "~a a ~a~%"
					   (death-verb actor)
					   (name actor)))))

;; define monster types
(defenemy goblin #\g () :dmg 4 :health (1+ (roll 3)) :str -1 :dex 1 :color 'green
  :xp 3
  :description "a goblin with a sharp dagger")
(defenemy rat #\r () :dmg 2 :health (roll 2) :dex 2 :color 'dark-red
  :loot (list (list #'make-rat-meat 50)
	      (list #'make-poison-rat-meat 50))
  :description "a giant rat")
(defenemy ogre #\O () :dmg 6 :health (+ 4 (roll 2))
  :str 2 :dex -2 :color 'orange :speed 1.75
  :xp 8
  :description "a hulking ogre")

(defmethod interact ((a player) (b trap))
  (when (= 0 (random 2))
    (setf (hiddenp b) nil)
    (save (save-dc b) dex a
	(print-to-log "you triggered ~a but dodged out of the way" (description b))
	(print-to-log "you triggered ~a and took ~d damage"
		      (description b) (damage a (roll (dmg b)))))))

(defmethod interact ((a combat-entity) (b fire))
  (print-to-log "~a walked into fire and took ~a damage~a" (name a)
		(damage a (roll (dmg b)))
		(if (deadp a) ", killing it" "")))

;; generate a sample board
(make-layer (generate-dungeon '(50 . 20) 3
			      (list (list #'make-ogre #'make-goblin
					  #'make-goblin #'make-rat)
				    (list #'make-food-pickup
					  #'make-poison-herb-pickup
					  #'make-healing-herb-pickup)
				    #'make-trap)))
(make-layer (generate-dungeon '(50 . 20) 3
			      (list (list #'make-goblin #'make-goblin #'make-rat)
				    (list #'make-food-pickup
					  #'make-poison-herb-pickup
					  #'make-healing-herb-pickup)
				    #'make-trap)))

;; give player a weapon
(equip (make-big-sword) *player*)
(equip (make-leather-armor) *player*)

;; put some stuff in the inventory
(push (make-faggot) *inventory*)
(push (make-faggot) *inventory*)
(push (make-faggot) *inventory*)
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
				return (add-pos (pos *player*) (mul-pos direction x)))))
	(remove-from-inventory item)
	(print-to-log "you threw ~a~%" (description item))
	(if (breakable item)
	    (break-at final-pos item)
	    (make-pickup item final-pos)))))
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

;; start game
(start)
