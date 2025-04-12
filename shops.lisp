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

(defmethod make-shopkeeper :around (pos)
  (let ((shopkeeper (call-next-method)))
    (loop for x from (- (car pos) (domain shopkeeper)) to (+ (car pos) (domain shopkeeper))
	  do (loop for y from (- (cdr pos) (domain shopkeeper))
		     to (+ (cdr pos) (domain shopkeeper))
		   when (and (= (random 6) 0)
			     (visiblep (cons x y) shopkeeper))
		     do (place-shop-item (cons x y) shopkeeper)))
    (make-shopkeeper-pedestal (pos shopkeeper)) ;; pos shopkeeper because first pos may be occupied
    shopkeeper))
