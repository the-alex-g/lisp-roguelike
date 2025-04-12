(defparameter *inventory* '())
(defparameter *gold* 0)
(defparameter *has-store-item-p* nil)

(defun names-equal-p (a b)
  (and (string= (log-to-string "~a" (name a))
		(log-to-string "~a" (name b)))
       (or (and (not (shopkeeper a))
		(not (shopkeeper b)))
	   (and (shopkeeper a)
		(shopkeeper b)))))

(defun short-inventory ()
  (loop for item in *inventory*
	with used-names = nil
	with string-name = ""
	do (setf string-name (log-to-string "~a" (name item)))
	unless (member string-name used-names :test #'equal)
	  collect (progn (push string-name used-names)
			 item)))

(defun update-has-store-item ()
  (setf *has-store-item-p*
	(loop for item in *inventory*
	      thereis (shopkeeper item))))

(defun remove-from-inventory (item)
  (setf *inventory*
	(remove item *inventory* :test (lambda (a b)
					 (names-equal-p a b))
				 :count 1))
  (update-has-store-item))

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
	(setf *inventory* (append *inventory* (list item))))
    (when (shopkeeper item)
      (setf *has-store-item-p* t))))

(defun reorder-inventory ()
  ;; recreate the inventory to group like items
  (let ((old-inventory *inventory*))
    (setf *inventory* nil)
    (loop for item in old-inventory
	  do (add-to-inventory item))))

(defgeneric sell-price (item)
  (:method ((item equipment))
    (ash (price item) -1)))

(defun inventory-name (item &key (sell-price-p nil))
  (log-to-string "~[~;~:;~:*~dx ~]~a~:[~; (~d gold)~]"
		 (num-in-inventory item)
		 (name item)
		 (shopkeeper item)
		 (if sell-price-p
		     (sell-price item)
		     (price item))))

(defun print-inventory ()
  (if (= (length *inventory*) 0)
      (print-to-log "your inventory is empty")
      (column-print (loop for item in (short-inventory)
			  collect (inventory-name item))
		    :print-function #'print-to-log)))

(defun owns-item-p ()
  (or (> (length *inventory*) 0)
      (loop for i being the hash-values of (equipment *player*)
	    thereis i)))

(defun get-owned-item ()
  (get-item-from-list
   (append (short-inventory)
	   (flatten (loop for item-list being the hash-values of (equipment *player*)
			  collect item-list)))
   :naming-function (lambda (i)
		      (log-to-string "~[~;~:;~:*~dx ~]~a~0@*~[ (equipped)~;~]~
                                      ~2@*~:[~; (~d gold)~]"
				     (num-in-inventory i)
				     (name i)
				     (shopkeeper i)
				     (price i)))))

(defun get-item-from-inventory ()
  (get-item-from-list
   (short-inventory)
   :naming-function #'inventory-name))

(defmacro with-item-from-inventory (&body body)
  `(if (= (length *inventory*) 0)
       (print-to-log "you have nothing in your inventory")
       (let ((item (get-item-from-inventory)))
	 (when item
	   ,@body))))

(defmacro with-owned-item (&body body)
  `(if (owns-item-p)
       (let ((item (get-owned-item)))
	 (when item
	   ,@body))
       (print-to-log "you have no items")))

(defmacro with-owned-item-if (no &body body)
  `(if (owns-item-p)
       (let ((item (get-owned-item)))
	 (if item (progn ,@body) ,no))
       (progn (print-to-log "you have no items")
	      ,no)))

(defmacro with-item-from-inventory-if (no &body body)
  `(if (= (length *inventory*) 0)
       (progn (print-to-log "you have nothing in your inventory")
	      ,no)
       (let ((item (get-item-from-inventory)))
	 (if item (progn ,@body) ,no))))

(defun equippedp (item creature)
  (member item (gethash (equip-slot item) (equipment creature)) :test #'equal))

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
    (if (shopkeeper item)
	(progn (print-to-log "you must buy that before equipping it")
	       nil)
	(let ((result (call-next-method)))
	  (when result
	    (remove-from-inventory item))
	  result)))
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

(defun sell-item (shopkeeper)
  (let ((sellable-items (loop for item in (short-inventory)
			      unless (shopkeeper item)
				collect item)))
    (if sellable-items
	(let ((item (get-item-from-list sellable-items
					:naming-function (lambda (item)
							   (inventory-name item :sell-price-p t)))))
	  (when item
	    (remove-from-inventory item)
	    (incf *gold* (sell-price item))
	    (print-to-log "you sold ~a for ~d gold"
			  (name item)
			  (sell-price item))
	    (setf (shopkeeper item) shopkeeper)
	    (place item (pos shopkeeper) :solid nil)))
	(print-to-log "you don't have any items to sell"))))

(defun checkout ()
  (let ((total-price (loop for item in *inventory*
			   when (shopkeeper item)
			     sum (price item)))
	(starting-gold *gold*)
	(min-price (loop for item in *inventory*
			 when (shopkeeper item)
			   minimize (price item)))
	(items-checked-out '()))
    (cond ((= total-price 0)
	   (print-to-log "you don't have anything to buy"))
	  ((>= *gold* total-price)
	   (loop for item in *inventory*
		 when (shopkeeper item)
		   do (setf (shopkeeper item) nil)
		   and do (push item items-checked-out))
	   (decf *gold* total-price))
	  ((>= *gold* min-price)
	   (labels ((checkout-item ()
		      (let ((buyable-items (loop for item in (short-inventory)
						 when (and (shopkeeper item)
							   (<= (price item) *gold*))
						   collect item)))
			(when buyable-items
			  (let ((item (get-item-from-list buyable-items
							  :naming-function #'inventory-name)))
			    (when item
			      (setf (shopkeeper item) nil)
			      (decf *gold* (price item))
			      (push item items-checked-out)
			      (checkout-item)))))))
	     (checkout-item)))
	  (t
	   (print-to-log "you don't have enough gold for any of those items")))
    (when items-checked-out
      (print-to-log "you bought~{ ~a~#[~; and~:;,~]~} for ~d gold"
		    (mapcar #'name items-checked-out)
		    (- starting-gold *gold*))
      (update-has-store-item))))

(defun steal-items ()
  (loop for item in *inventory*
	when (shopkeeper item)
	  do (setf (enragedp (shopkeeper item)) t)
	  and do (setf (shopkeeper item) nil))
  (setf *has-store-item-p* nil))

(defun get-shopkeeper ()
  (loop for item in *inventory*
	when (shopkeeper item)
	  return (shopkeeper item)))
