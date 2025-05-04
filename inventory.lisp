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
	  (flatten (loop for item-list being the hash-values of (equipment *player*)
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


(defun in-inventoryp (item)
  (loop for i in (get-inventory)
	  thereis (names-equal-p item i)))

(defun num-in-inventory (item)
  (loop for i in *inventory*
	count (names-equal-p i item)))

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
