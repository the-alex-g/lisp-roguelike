(load "./game-engine.lisp")

(defparameter *board* '())

;; generate a sample board
(loop for x below (car *board-size*)
      do (loop for y below (cdr *board-size*)
	       when (or (< x 7) (> x 8) (= y 4))
		 do (push (cons x y) *board*)))

(make-actor :pos '(6 . 5)
	    :display-char #\C)

(defmethod input (cmd)
  (cond ((equal cmd "a")
	 (move *player* '(-1 . 0)))
	((equal cmd "d")
	 (move *player* '(1 . 0)))
	((equal cmd "w")
	 (move *player* '(0 . -1)))
	((equal cmd "s")
	 (move *player* '(0 . 1)))))

(start)
