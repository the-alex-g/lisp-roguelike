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
