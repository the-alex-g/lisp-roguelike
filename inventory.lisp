(defparameter *inventory* '())
(defparameter *inventory-filter* nil)
(defparameter *gold* 0)
(defparameter *shopkeeper* nil)

(defmacro with-filter (filter &body body)
  `(let ((*inventory-filter* ,filter))
     ,@body))

(defun get-inventory ()
  (if *inventory-filter*
      (loop for item in *inventory*
	    when (funcall *inventory-filter* item)
	      collect item)
      *inventory*))

(defun get-equipped-items ()
  (let ((equipped-items
	  (flatten (loop for item-list being the hash-keys of (equipment *player*)
			 collect item-list))))
    (if *inventory-filter*
	(loop for item in equipped-items
	      when (funcall *inventory-filter* item)
		collect item)
	equipped-items)))

(defun get-owned-items ()
  (append (get-inventory) (get-equipped-items)))

(defun string-name (item)
  (let ((name (name item)))
    (if (stringp name)
	name
	(log-to-string "~a" name))))

(defun names-equal-p (a b)
  (and (string= (string-name a) (string-name b))
       (or (and (not (shopkeeper a))
		(not (shopkeeper b)))
	   (and (shopkeeper a)
		(shopkeeper b)))))

(defun short-inventory ()
  (do ((inventory (get-inventory) (cdr inventory))
       (used-names nil (cons (string-name (car inventory)) used-names))
       (items nil (if (member (string-name (car inventory)) used-names :test #'string=)
		      items
		      (cons (car inventory) items))))
      ((not inventory) (reverse items))))

(defun update-shopkeeper ()
  (setf *shopkeeper*
	(loop for item in *inventory*
	      thereis (shopkeeper item))))

(defgeneric remove-from-inventory (item &key &allow-other-keys)
  (:method ((item equipment) &key (exact-match-p t) &allow-other-keys)
    (if exact-match-p
	(progn (setf *inventory*
		     (remove item *inventory* :test #'equal))
	       (update-shopkeeper)
	       item)
	(do ((inventory *inventory* (cdr inventory))
	     (new-inventory nil (if (and (not removed-item)
					 (names-equal-p (car inventory) item))
				    (progn (setf removed-item (car inventory))
					   new-inventory)
				    (cons (car inventory) new-inventory)))
	     (removed-item nil))
	    ((not inventory)
	     (progn (setf *inventory* (reverse new-inventory))
		    (update-shopkeeper)
		    removed-item))))))

(defun in-inventoryp (item)
  (loop for i in (get-inventory)
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
      (setf *shopkeeper* (shopkeeper item)))))

(defun reorder-inventory ()
  ;; recreate the inventory to group like items
  (let ((old-inventory *inventory*))
    (setf *inventory* nil)
    (loop for item in old-inventory
	  do (add-to-inventory item))))

(defun inventory-name (item &key (sell-price-p nil))
  (log-to-string "~[~;~:;~:*~dx ~]~a~:[~; (~d gold)~]"
		 (num-in-inventory item)
		 (name item)
		 (shopkeeper item)
		 (if sell-price-p
		     (sell-price item)
		     (price item))))

(defun print-inventory ()
  (if (= (length (get-inventory)) 0)
      (print-to-log "your inventory is empty")
      (column-print (loop for item in (short-inventory)
			  collect (inventory-name item))
		    :print-function #'print-to-log)))

(defun owns-item-p ()
  (> (+ (length (get-inventory))
	(length (get-equipped-items)))
     0))

(defun get-owned-item ()
  (get-item-from-list (get-owned-items)
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

(defmacro with-item-from-inventory (fail-case &body body)
  `(if (= (length (get-inventory)) 0)
       (progn (print-to-log "you have nothing in your inventory")
	      ,fail-case)
       (let ((item (get-item-from-inventory)))
	 (if item
	     (progn ,@body)
	     ,fail-case))))

(defmacro with-owned-item (fail-case &body body)
  `(if (owns-item-p)
       (let ((item (get-owned-item)))
	 (if item
	     (progn ,@body)
	     ,fail-case))
       (progn (print-to-log "you have no items")
	      ,fail-case)))

(defun equippedp (item creature)
  (member item (gethash (equip-slot item) (equipment creature)) :test #'equal))

(defgeneric unequip (item actor &key to)
  (:method (item actor &key to))
  (:method :after ((item equipment) (actor player) &key (to 'inventory))
    (declare (ignore actor))
    (cond ((eq to 'inventory)
	   (add-to-inventory item))
	  ((eq to 'ground)
	   (place item (pos *player*) :solid nil))))
  (:method :after ((item equipment) (actor creature) &key to)
    (declare (ignore to))
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
			 (if (<= (+ (size item) equips-size (- size)) max-equips)
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

(defun get-items-with-name-of (item &key (limit -1))
  (do ((inventory *inventory* (cdr inventory))
       (collected-items nil))
      ((or (= (length collected-items) limit)
	   (not inventory))
       collected-items)
    (when (names-equal-p item (car inventory))
      (push (car inventory) collected-items))))

(defun sell-item (shopkeeper)
  (let ((sellable-items (loop for item in (short-inventory)
			      unless (shopkeeper item)
				collect item)))
    (if sellable-items
	(flet ((sell (i)
		 (remove-from-inventory i :selling t)
		 (incf *gold* (sell-price i))
		 (setf (shopkeeper i) shopkeeper)
		 (place i (pos shopkeeper) :solid nil)))
	  (let ((item (get-item-from-list sellable-items
					  :naming-function (lambda (item)
							     (inventory-name item :sell-price-p t))))
		(num 1))
	    (when item
	      (let ((qty (num-in-inventory item)))
		(when (> qty 1)
		  (print-to-screen "~%how many ~as would you like to sell?~%"
				   (name item))
		  (setf num (get-number-input :min 1 :max qty))))
	      (print-to-log "you sold ~d ~a~0@*~p for ~*~d gold"
			    num
			    (name item)
			    (* num (sell-price item)))
	      (mapc #'sell (get-items-with-name-of item :limit num)))))
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
      (reorder-inventory)
      (update-shopkeeper))))

(defun steal-items ()
  (loop for item in *inventory*
	when (shopkeeper item)
	  do (setf (enragedp (shopkeeper item)) t)
	  and do (setf (shopkeeper item) nil))
  (setf *shopkeeper* nil))
