(load "./game-engine.lisp")

;; define monster types
(defenemy goblin #\g () :dmg 4 :health 4)
(defenemy spawner #\S (spawn-function) :dmg 1 :def 2 :spd 10)
(defenemy goblin-spawner #\G nil :spawn-function #'make-goblin :inherit spawner)

;; define equipment types
(defequipment food () :equip-slot 'none :health 2 :consumable t)

;;; custom use function for food
(defmethod use ((item food) (target actor))
  (format t "You ate ~a and regained ~d health~%" (name item) (health item))
  (incf (health target) (health item)))

;;; custom update function for spawner class
(defmethod update ((obj spawner))
  (setf (enabled (funcall (spawn-function obj) (add-pos +up+ (pos obj)))) t))

;; generate a sample board
(loop for x below (car *board-size*)
      do (loop for y below (cdr *board-size*)
	       when (or (< x 7) (> x 8) (= y 4))
		 do (setf (gethash (cons x y) *board*) 'hidden)))

;; add actors to board
(make-goblin '(0 . 0))
(make-goblin-spawner '(10 . 4))

;; give player a weapon
(equip (make-equipment 'hand :dmg 6 :name 'sword) *player*)

;; put some stuff in the inventory
(push (make-equipment 'hand :dmg 8 :name "big sword") *inventory*)
(push (make-food) *inventory*)

;; define actions
(defaction "a" (move *player* +left+))
(defaction "d" (move *player* +right+))
(defaction "w" (move *player* +up+))
(defaction "s" (move *player* +down+))
(defaction "i" (let ((actor (find-actor-at :actor *player*)))
		 (when actor
		   (interact *player* actor))))
(defaction "e"
  (if (> (length *inventory*) 0)
      (let* ((new-item (get-item-from-list *inventory* :naming-function #'name))
	     (old-item (when new-item
			 (equip new-item *player*))))
	(when new-item
	  (format t "You have equipped ~a" (name new-item))
	  (setf *inventory* (remove new-item *inventory* :test #'equal))
	  (when old-item
	    (format t " instead of ~a" (name old-item))
	    (push old-item *inventory*))))
      (format t "You have nothing to equip!"))
  (fresh-line))
(defaction "u"
  (if (> (length *inventory*) 0)
      (let ((item (get-item-from-list *inventory* :naming-function #'name)))
	(when item
	  (use item *player*)))
      (format t "You have nothing to use!")))
(defaction "v" (print-inventory))

;; start game
(start)
