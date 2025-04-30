(defparameter *shop-items* ())

(defun add-to-shop (&rest constructors)
  (dolist (constructor constructors)
    (push constructor *shop-items*))
  (when (= (length constructors) 1)
    (car constructors)))

(defun get-shop-item-constructor ()
  (randnth *shop-items*))

(defun place-shop-item (pos shopkeeper)
  (setf (shopkeeper (funcall (get-shop-item-constructor) pos)) shopkeeper))

(defun get-shop-position (cells &key start)
  (unless start
    (setf start (randnth cells)))
  (flet ((adjacent-empty-cells (pos)
	   (loop for direction in +directions+
		 when (let ((new-pos (vec+ direction pos)))
			(and (member new-pos cells :test #'equal)
			     (not (solid pos))))
		   sum 1)))
    (flood-fill start (t (if (= (adjacent-empty-cells current) 8) current))
      (or result
	  start))))

(defmethod make-shopkeeper :around (pos)
  (let ((shopkeeper (call-next-method)))
    (loop for x from (- (car pos) (domain shopkeeper)) to (+ (car pos) (domain shopkeeper))
	  do (loop for y from (- (cdr pos) (domain shopkeeper))
		     to (+ (cdr pos) (domain shopkeeper))
		   when (and (= (random 6) 0)
			     (not (equal (cons x y) pos))
			     (visiblep (cons x y) shopkeeper))
		     do (place-shop-item (cons x y) shopkeeper)))
    shopkeeper))
