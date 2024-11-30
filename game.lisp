(load "./game-engine.lisp")
(load "./bsp-dungeon.lisp")

(defactor trap #\! ((dmg 4) (save-dc 10) (discoverable t))
  :interact-action-only nil :solid nil :destructible nil :color +red+ :hiddenp t
  :description "a cunning trap")

(defmethod description ((obj trap))
  (if (hiddenp obj)
      (cond ((and (discoverable obj) (>= (roll 20) 11))
	     (print-to-log "you discovered ~a" (slot-value obj 'description))
	     (setf (hiddenp obj) nil))
	    (t (setf (discoverable obj) nil) nil))
      (slot-value obj 'description)))

;; define equipment types
(defequipment food () :health 2 :consumable t
  :description "food")
(defequipment ranged-weapon (range) :dex -2)
(defequipment bow () :dmg 4 :range 4 :description "a bow" :inherit ranged-weapon)

;;; custom use function for food
(defmethod use ((item food) (target actor))
  (print-to-log "You ate ~a and regained ~d health~%" (name item) (health item))
  (incf (health target) (health item)))

;; define monster types
(defenemy goblin #\g () :dmg 4 :health 4 :str -1 :dex 1 :color +green+
  :description "a goblin with a sharp dagger")
(defenemy rat #\r () :dmg 2 :health 2 :dex 2 :color +dark-red+
  :description "a giant rat")
(defenemy ogre #\O () :dmg 6 :health 6 :str 2 :dex -2 :color +orange+ :speed 1.75
  :description "a hulking ogre")

(defmethod interact ((a player) (b trap))
  (when (= 0 (random 2))
    (setf (hiddenp b) nil)
    (if (>= (+ (roll 20) (dex a)) (save-dc b))
	(print-to-log "you triggered ~a but dodged out of the way" (description b))
	(print-to-log "you triggered ~a and took ~d damage"
		      (description b) (damage a (roll (dmg b)))))))

;; generate a sample board
(make-layer (generate-dungeon '(50 . 20) 3
			      (list (list #'make-ogre #'make-goblin
					  #'make-goblin #'make-rat)
				    #'make-food-pickup #'make-trap)))
(make-layer (generate-dungeon '(50 . 20) 3
			      (list (list #'make-goblin #'make-goblin #'make-rat)
				    #'make-food-pickup
				    #'make-trap)))

;; give player a weapon
(equip (make-equipment 'hand :dmg 6 :name 'sword) *player*)

;; put some stuff in the inventory
(push (make-equipment 'hand :dmg 8 :name 'big-sword) *inventory*)
(push (make-food) *inventory*)
(push (make-bow) *inventory*)

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
	(when item
	  (make-pickup item (pos *player*))
	  (remove-from-inventory item)
	  (print-to-log "you dropped ~a" (name item)))))
(defaction #\i "interact with an object on your space"
  (let ((actor (choose-actor-at *player*)))
    (when actor
      (interact *player* actor))))
(defaction #\e "equip an inventory item"
  (with-item-from-inventory
      (let ((output "")
	    (old-item (when item
			(equip item *player*))))
	(when (and item (not (eq old-item 'failed)))
	  (setf output (log-to-string "You have equipped ~a" (name item)))
	  (remove-from-inventory item)
	  (when old-item
	    (setf output (concatenate 'string output
				      (log-to-string " instead of ~a"
						     (name old-item))))
	    (add-to-inventory old-item)))
	(print-to-log output))))
(defaction #\u "use an inventory item"
  (with-item-from-inventory
      (when item
	(use item *player*))))
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
(defaction #\h "print help menu"
  (loop for k being the hash-keys of *action-descriptions*
	do (print-to-log "~c: ~a~%" k (gethash k *action-descriptions*)))
  (print-to-log "q: quit~%"))

;; start game
(start)
