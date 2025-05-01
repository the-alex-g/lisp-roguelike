(defparameter *masks* (make-hash-table))

(defun define-mask-set (keys)
  (do ((masks keys (cdr masks))
       (index 1 (* 2 index)))
      ((not masks) nil)
    (setf (gethash (car masks) *masks*) index)))

(defun make-mask (values)
  (apply #'+ (loop for mask in values
		   collect (gethash mask *masks*))))

(defun mask (a b &key (match :any))
  (logcount (cond ((eq match :any)
		   (logand a b))
		  ((and (eq match :all)
			(= a b))
		   a)
		  (t 0))))
