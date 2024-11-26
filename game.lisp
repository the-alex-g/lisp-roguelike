(load "./game-engine.lisp")
(load "./bsp-dungeon.lisp")

;; define monster types
(defenemy goblin #\g () :dmg 4 :health 4 :str -1 :dex 1 :color +green+
  :description "a goblin with a sharp dagger")
(defenemy ogre #\O () :dmg 6 :health 6 :str 2 :dex -2 :color +orange+ :speed 1.75
  :description "a hulking ogre")

(defactor banana #\( nil :solid nil :color +light-orange+ :description "a ripe banana")

;; define equipment types
(defequipment food () :equip-slot 'none :health 2 :consumable t
  :description "food")
(defequipment ranged-weapon (range) :equip-slot 'hand :dex -2)
(defequipment bow () :dmg 4 :range 4 :description "a bow" :inherit ranged-weapon)

;;; custom use function for food
(defmethod use ((item food) (target actor))
  (print-to-log "You ate ~a and regained ~d health~%" (name item) (health item))
  (incf (health target) (health item)))

;; generate a sample board
(make-layer (generate-dungeon '(50 . 20) 3
			      (list (list #'make-goblin #'make-ogre)
				    #'make-food-pickup
				    #'make-banana)))

;; give player a weapon
(equip (make-equipment 'hand :dmg 6 :name 'sword) *player*)

;; put some stuff in the inventory
(push (make-equipment 'hand :dmg 8 :name 'big-sword) *inventory*)
(push (make-food) *inventory*)
(push (make-bow) *inventory*)

;; define actions
(defaction #\a (move *player* +left+))
(defaction #\d (move *player* +right+))
(defaction #\w (move *player* +up+))
(defaction #\s (move *player* +down+))
(defaction #\D (with-item-from-inventory
		   (when item
		     (make-pickup item (pos *player*))
		     (remove-from-inventory item)
		     (print-to-log "you dropped ~a" (name item)))))
(defaction #\r (let ((weapon (gethash 'hand (equips *player*))))
		 (when (slot-exists-p weapon 'range)
		   (let ((direction (get-direction)))
		     (loop for r from 1 to (1- (range weapon))
			   do (let ((d (find-solid-actor-at
					(add-pos (pos *player*)
						 (mul-pos direction r)))))
				(when d
				  (attack *player* d)
				  (return nil))))))))
(defaction #\i (let ((actor (find-actor-at *player*)))
		 (when actor
		   (interact *player* actor))))
(defaction #\e
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
(defaction #\u (with-item-from-inventory
		   (when item
		     (use item *player*))))
(defaction #\v (print-inventory))
(defaction #\l
    (let ((position (get-direction :include-zero t)))
      (when position
	(let ((actors (find-all-actors-at (add-pos (pos *player*) position)
					  *player*)))
	  (if actors
	      (mapc (lambda (actor)
		      (print-to-log "You see ~a"
				    (display actor :fields '(description)
						   :headers nil)))
		    actors)
	      (print-to-log "there's nothing there"))))))

;; start game
(start)
