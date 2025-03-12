(defparameter *inventory* '())

(defun names-equal-p (a b)
  (string= (log-to-string "~a" (name a))
	   (log-to-string "~a" (name b))))

(defun short-inventory ()
  (loop for item in *inventory*
	with used-names = nil
	with string-name = ""
	do (setf string-name (log-to-string "~a" (name item)))
	unless (member string-name used-names :test #'equal)
	  collect (progn (push string-name used-names)
			 item)))

(defun remove-from-inventory (item)
  (setf *inventory*
	(remove item *inventory* :test (lambda (a b)
					 (names-equal-p a b))
				 :count 1)))

(defun in-inventoryp (item)
  (loop for i in *inventory*
	  thereis (names-equal-p item i)))

(defun num-in-inventory (item)
  (loop for i in *inventory*
	count (names-equal-p i item)))

(defgeneric add-to-inventory (item)
  (:method ((item equipment))
    (if (in-inventoryp item)
	(setf *inventory*
	      ;; put the new item next to others with the same name
	      (loop for i in *inventory*
		    with needs-collecting = t
		    when (and needs-collecting (names-equal-p item i))
		      collect item
		      and do (setf needs-collecting nil)
		    collect i))
	(setf *inventory* (append *inventory* (list item))))))

(defun reorder-inventory ()
  ;; recreate the inventory to group like items
  (let ((old-inventory *inventory*))
    (setf *inventory* nil)
    (loop for item in old-inventory
	  do (add-to-inventory item))))

(defun print-inventory ()
  (if (= (length *inventory*) 0)
      (print-to-log "your inventory is empty")
      (loop for item in (short-inventory)
	    do (print-to-log "~[~;~:;~:*~dx ~]~a"
			     (num-in-inventory item)
			     (name item)))))

(defun get-item-from-inventory ()
  (get-item-from-list
   (short-inventory)
   :naming-function (lambda (i)
		      (log-to-string "~[~;~:;~:*~dx ~]~a"
				     (num-in-inventory i)
				     (name i)))))

(defmacro with-item-from-inventory (&body body)
  `(if (= (length *inventory*) 0)
       (print-to-log "you have nothing in your inventory")
       (let ((item (get-item-from-inventory)))
	 (when item
	   ,@body))))

(defgeneric unequip (item actor)
  (:method (item actor))
  (:method :after ((item equipment) (actor player))
    (declare (ignore actor))
    (add-to-inventory item))
  (:method :after ((item equipment) (actor creature))
    (setf (gethash (equip-slot item) (equipment actor))
	  (remove item (gethash (equip-slot item) (equipment actor)) :test #'equal))))

(defgeneric equip (item actor)
  (:method (item actor))
  (:method :around ((item equipment) (actor player))
    (let ((result (call-next-method)))
      (when result
	(remove-from-inventory item))
      result))
  (:method :around ((item equipment) (actor creature))
    (let* ((current-equips (gethash (equip-slot item) (equipment actor)))
	   (equips-size (loop for i in current-equips
			      sum (size i)))
	   (max-equips (cadr (assoc (equip-slot item) (slot-nums actor)))))
      (flet ((equip-item ()
	       (push item (gethash (equip-slot item) (equipment actor)))
	       (when (next-method-p)
		 (call-next-method))
	       t))
	(when max-equips
	  (if (<= (+ equips-size (size item)) max-equips)
	      (equip-item)
	      (labels ((get-items-to-unequip (&optional item-list (size 0))
			 (if (>= size (size item))
			     item-list
			     (let ((item-to-replace (get-item-from-list (gethash (equip-slot item)
										 (equipment actor))
									:naming-function #'name
									:ignoring item-list
									:test #'equal
									:what "item to replace")))
			       (if item-to-replace
				   (get-items-to-unequip (cons item-to-replace item-list)
							 (+ size (size item-to-replace)))
				   nil)))))
		(let ((items-to-unequip (get-items-to-unequip)))
		  (when items-to-unequip
		    (mapc (lambda (i) (unequip i actor)) items-to-unequip)
		    (equip-item)
		    items-to-unequip)))))))))
