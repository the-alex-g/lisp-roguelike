(defun wander (obj)
  (move obj (random-direction)))

(defun smart-wander (obj)
  (let ((initial-index (random 8)))
    (do ((index initial-index (mod (1+ index) 8))
	 (direction (nth initial-index +directions+)
		    (nth index +directions+)))
	((not (solid (vec+ (pos obj) direction)))
	 (move obj direction)))))

(defun no-idle (obj))
