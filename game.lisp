(load "./game-engine.lisp")

;; generate a sample board
(loop for x below (car *board-size*)
      do (loop for y below (cdr *board-size*)
	       when (or (< x 7) (> x 8) (= y 4))
		 do (setf (gethash (cons x y) *board*) 'hidden)))

(make-actor "foo" #\C '(6 . 5))
(make-enemy "goblin" #\g '(0 . 3))

(start)
