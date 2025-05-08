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

(defmacro get-actors-in-los-of (obj search-solid search-non-solid range &rest filters)
  (let ((varnames (loop repeat (length filters) collect (gensym))))
    `(let ,(loop for varname in varnames collect `(,varname nil))
       (flet ((collect-actor (actor)
		(when (and (not (equal actor ,obj))
			   ,(if range
				`(<= (distance (pos ,obj) (pos actor)) ,range)
				t)
			   (visiblep actor ,obj))
		  (cond ,@(loop for filter in filters
				for varname in varnames
				collect `(,filter (push actor ,varname)))))))
	 ,(when search-solid
	    '(loop for actor being the hash-values of (solid-actors)
	      do (collect-actor actor)))
	 ,(when search-non-solid
	    '(loop for actor being the hash-values of (non-solid-actors)
	      do (collect-actor actor))))
       (values ,@varnames))))

(defun get-closest-of-list (to list &optional (filter (lambda (x) t)))
  (when list
    (do ((eligible-targets list (cdr eligible-targets))
	 (min-distance 100)
	 (closest-actor nil))
	((not eligible-targets) closest-actor)
      (when (funcall filter (car eligible-targets))
	(let ((distance (distance (pos (car eligible-targets)) (pos to))))
	  (when (< distance min-distance)
	    (setf min-distance distance)
	    (setf closest-actor (car eligible-targets))))))))

(defun get-closest-actor (to range filter)
  (get-closest-of-list to (get-actors to range filter)))
