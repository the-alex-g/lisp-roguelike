(load "./game-engine.lisp")

;; generate a sample board
(loop for x below (car *board-size*)
      do (loop for y below (cdr *board-size*)
	       when (or (< x 7) (> x 8) (= y 4))
		 do (setf (gethash (cons x y) *board*) 'hidden)))

(make-actor "foo" #\C '(6 . 5) :solid nil)

(defmethod input (cmd)
  (cond ((equal cmd "a")
	 (move *player* +left+))
	((equal cmd "d")
	 (move *player* +right+))
	((equal cmd "w")
	 (move *player* +up+))
	((equal cmd "s")
	 (move *player* +down+))))

(start)
