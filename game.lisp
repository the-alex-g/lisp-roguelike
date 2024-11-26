(load "./game-engine.lisp")
(load "./bsp-dungeon.lisp")

;; define monster types
(defenemy goblin #\g () :dmg 4 :health 4 :str -1 :dex 1 :color +green+
  :description "a small, green-skinned humanoid with a sharp dagger")

;; define equipment types
(defequipment food () :equip-slot 'none :health 2 :consumable t
  :description "food")

;;; custom use function for food
(defmethod use ((item food) (target actor))
  (print-to-log "You ate ~a and regained ~d health~%" (name item) (health item))
  (incf (health target) (health item)))

;; generate a sample board
(let ((board (generate-board '(60 . 20) 4)))
  (setf *current-layer* (make-layer board))
  (setf (pos *player*) (car board)))

;; give player a weapon
(equip (make-equipment 'hand :dmg 6 :name 'sword) *player*)

;; put some stuff in the inventory
(push (make-equipment 'hand :dmg 8 :name 'big-sword) *inventory*)
(push (make-food) *inventory*)

;; define actions
(defaction #\a (move *player* +left+))
(defaction #\d (move *player* +right+))
(defaction #\w (move *player* +up+))
(defaction #\s (move *player* +down+))
(defaction #\i (let ((actor (find-actor-at *player*)))
		 (when actor
		   (interact *player* actor))))
(defaction #\e
  (if (> (length *inventory*) 0)
      (let* ((new-item (get-item-from-list *inventory* :naming-function #'name))
	     (output "")
	     (old-item (when new-item
			 (equip new-item *player*))))
	(when (and new-item (not (eq old-item 'failed)))
	  (setf output (log-to-string "You have equipped ~a" (name new-item)))
	  (setf *inventory* (remove new-item *inventory* :test #'equal))
	  (when old-item
	    (setf output (concatenate 'string output
				      (log-to-string " instead of ~a"
						     (name old-item))))
	    (push old-item *inventory*)))
	(print-to-log output))
      (print-to-log "You have nothing to equip!")))
(defaction #\u
  (if (> (length *inventory*) 0)
      (let ((item (get-item-from-list *inventory* :naming-function #'name)))
	(when item
	  (use item *player*)))
      (print-to-log "You have nothing to use!")))
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
