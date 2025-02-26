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
