(defun is-hostile-in-los-of-p (obj)
  (loop for actor being the hash-values of (solid-actors)
	  thereis (and (not (wallp actor))
		       (visiblep (pos actor) (pos obj))
		       (hostilep actor obj))))

(defun get-hostile-in-los-of (from)
  (let* ((hostiles (loop for actor being the hash-values of (solid-actors)
			 when (and (not (wallp actor))
				   (visiblep (pos actor) (pos from))
				   (hostilep actor from))
			   collect actor))
	 (min-distance (loop for hostile in hostiles
			     minimize (distance (pos from) (pos hostile)))))
    (loop for hostile in hostiles
	  when (= (distance (pos from) (pos hostile)) min-distance)
	    return hostile)))

(defmacro get-actors (obj range &rest filters)
  (let ((varnames (loop repeat (length filters) collect (gensym)))
	(blanket-condition (gensym)))
    `(let ,(loop for varname in varnames collect `(,varname nil))
       (loop-in-circle ,range
	 with actor = nil
	 with ,blanket-condition = nil
	 do (setf actor (contents (vec+ (cons x y) (pos ,obj))))
	 do (setf ,blanket-condition (and actor
					  (not (equal actor ,obj))
					  (visiblep actor ,obj)))
	 ,@(apply #'append
		  (loop for filter in filters
			for varname in varnames
			collect `(when (and ,blanket-condition ,filter)
				   do (push actor ,varname)))))
       (values ,@varnames))))

(defun get-closest-of-list (to list &optional (filter (lambda (x) t)))
  (do ((eligible-targets list (cdr eligible-targets))
       (min-distance 100)
       (closest-actor nil))
      ((not eligible-targets) closest-actor)
    (when (funcall filter (car eligible-targets))
      (let ((distance (distance (pos (car eligible-targets)) (pos to))))
	(when (< distance min-distance)
	  (setf min-distance distance)
	  (setf closest-actor (car eligible-targets)))))))

(defun get-closest-actor (to range filter)
  (get-closest-of-list to (get-actors to range filter)))
