(defun apply-alignment-status (obj)
  (let ((index (random 3)))
    (cond ((= index 0)
	   (apply-to obj (make-good-status))
	   'g)
	  ((= index 1)
	   (apply-to obj (make-neutral-status))
	   'n)
	  ((= index 2)
	   (apply-to obj (make-evil-status))
	   'e))))

(defun evilp (obj)
  (eq (alignment obj) 'e))

(defun goodp (obj)
  (eq (alignment obj) 'g))

(defun neutralp (obj)
  (eq (alignment obj) 'n))

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
