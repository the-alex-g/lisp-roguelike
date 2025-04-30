(defparameter *masks* (make-hash-table))

(defun define-mask-set (&rest keys)
  (do ((masks keys (cdr masks))
       (index 1 (* 2 index)))
      ((not masks) nil)
    (setf (gethash (car masks) *masks*) index)))

(defun make-mask (&rest values)
  (apply #'+ (loop for mask in values
		   collect (gethash mask *masks*))))

(defun maskp (a b &key (match :any))
  (cond ((eq match :all)
	 (= a b))
	((eq match :any)
	 (not (= 0 (logand a b))))
	(t t)))
