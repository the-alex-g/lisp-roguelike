(load "./game-engine.lisp")

(defenemy goblin #\g () :dmg 4 :health 4)
(defenemy spawner #\S (spawn-function) :dmg 1 :def 2 :spd 10)
(defenemy goblin-spawner #\G nil :spawn-function #'make-goblin :inherit spawner)

(defmethod update ((obj spawner))
  (funcall (spawn-function obj) (add-pos +up+ (pos obj))))

;; generate a sample board
(loop for x below (car *board-size*)
      do (loop for y below (cdr *board-size*)
	       when (or (< x 7) (> x 8) (= y 4))
		 do (setf (gethash (cons x y) *board*) 'hidden)))

(make-actor "foo" #\C '(6 . 5))

(make-goblin '(0 . 0))
(make-goblin-spawner '(10 . 4))

(equip (make-equipment 'hand :dmg 6) *player*)

(start)
