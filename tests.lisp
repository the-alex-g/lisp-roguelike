(defparameter *tests-failed* 0)

(defmacro with-clean-board (&body body)
  `(let ((*current-layer* (make-layer)))
     ,@body))

(defun print-test (string passedp &rest args)
  (format t "~&~:[FAIL~;PASS~]: ~?" passedp string args)
  (unless passedp
    (incf *tests-failed*)))

(defun flag (string)
  (format t "~2%~:@(~a~)~2%" string))

(defun test-masks ()
  (flag "testing masks")
  (define-mask-set '(a b c d))
  (print-test "matching any" (= (mask (make-mask '(a b))
				      (make-mask '(b c)))
				1))
  (print-test "match any failing" (= (mask (make-mask '(a b))
				       (make-mask '(c d)))
				     0))
  (print-test "matching multiple" (= (mask (make-mask '(a b))
					   (make-mask '(a b c)))
				     2))
  (print-test "matching all" (= (mask (make-mask '(a b))
				      (make-mask '(a b))
				      :match :all)
				2)))

(defun test-combat ()
  (flag "TESTING COMBAT")
  (let ((enemy (make-instance 'creature :name "enemy" :health 10))
	(attack1 (make-attack :dmg 5 :to-hit 10 :types '(slashing)))
	(attack2 (make-attack :dmg 5 :to-hit 5 :types '(slashing)))
	(attack3 (make-attack :dmg 10 :to-hit 10 :types '(bludgeoning)))
	(status-attack (make-attack :dmg 0 :to-hit 10 :types '(piercing)
				    :statuses (list (make-instance 'status)))))
    (flet ((my-attack (text atk val)
	     (format t "~&~:(~a~)" text)
	     (attack enemy atk)
	     (print-test "health remaining: ~d" 
			 (= (health enemy) val)
			 (health enemy))))
      (my-attack "basic attack" attack1 5)
      (setf (resistances enemy) '(slashing))
      (my-attack "resistances" attack1 3)
      (setf (resistances enemy) 0)
      (setf (absorbances enemy) '(slashing))
      (my-attack "absorbance" attack1 8)
      (my-attack "health overflow" attack1 10)
      (my-attack "missing" attack2 10)
      (my-attack "status" status-attack 9)
      (print-test "target has one status"
		  (= (length (statuses enemy)) 1))
      (my-attack "killing" attack3 0))))

(defun test-vector-math ()
  (flag "TESTING VECTOR MATH")
  (flet ((test (test value function &rest args)
	   (let ((result (apply function args)))
	     (print-test "(~a ~{~a~^ ~}) = ~a"
		     (funcall test result value) function args result))))
    (test #'equal '(1 . 1) 'vec+ '(1 . 0) '(0 . 1))
    (test #'equal '(0 . 1) 'vec- '(1 . 1) '(1 . 0))
    (test #'equal '(-1 . -1) 'vec- '(1 . 1))
    (test #'= 5.0 'vec-length '(3 . 4))))

(defun test-equipment ()
  (flag "TESTING EQUIPPING")
  (let ((actor (make-instance 'creature
			      :slot-nums '((hand 2) (body 1))))
	(two-hand-equipment (make-instance 'equipment
					   :name '2-hand
					   :size 2))
	(one-hand-equipment-1 (make-instance 'equipment
					     :name '1-hand-1))
	(one-hand-equipment-2 (make-instance 'equipment
					     :name '1-hand-2))
	(one-hand-equipment-3 (make-instance 'equipment
					     :name '1-hand-3))
	(body-equipment (make-instance 'equipment
				       :slot 'body
				       :name 'body))
	(unequippable-equipment (make-instance 'equipment
					       :slot 'foobar
					       :name 'no-equip)))
    (flet ((test (slot expected-value)
	     (let ((equips (mapcar #'name (gethash slot (equipment actor)))))
	       (print-test "items in ~a slot: ~a"
			   (equal equips expected-value) slot equips))))
      (equip one-hand-equipment-1 actor)
      (equip one-hand-equipment-2 actor)
      (test 'hand '(1-hand-2 1-hand-1))
      (with-fake-input #\0
	(equip one-hand-equipment-3 actor))
      (test 'hand '(1-hand-3 1-hand-1))
      (with-fake-input #\q
	(equip two-hand-equipment actor))
      (test 'hand '(1-hand-3 1-hand-1))
      (with-fake-input #\0
	(equip two-hand-equipment actor))
      (test 'hand '(2-hand))
      (unequip two-hand-equipment actor)
      (test 'hand nil)
      (equip body-equipment actor)
      (test 'body '(body))
      (equip unequippable-equipment actor)
      (test 'foobar nil))))

(defun test-movement ()
  (flag "TESTING MOVEMENT AND POSITIONING")
  (with-clean-board
      (let ((actor (make-instance 'creature :name 'actor))
	    (obstacle (make-instance 'actor :name 'obstacle))
	    (item-1 (make-instance 'equipment :name 'item-1))
	    (item-2 (make-instance 'equipment :name 'item-2)))
	(flet ((test (pos solid non-solid)
		 (let* ((sld (solid pos))
			(non-sld (non-solid pos))
			(sld-name (when sld (name sld)))
			(non-sld-name (when non-sld (name non-sld))))
		   (print-test "(solid ~a) = ~a"
			       (eq sld-name solid) pos sld-name)
		   (print-test "(non-solid ~a) = ~a"
			       (eq non-sld-name non-solid) pos non-sld-name)
		   (when sld
		     (print-test "(pos ~a) = ~a"
				 (equal pos (pos sld)) sld-name (pos sld)))
		   (when non-sld
		     (print-test "(pos ~a) = ~a"
				 (equal pos (pos non-sld)) non-sld-name (pos non-sld))))))
	  (place actor +zero+)
	  (test +zero+ 'actor nil)
	  (place item-1 +zero+ :solid nil)
	  (test +zero+ 'actor 'item-1)
	  (move actor +right+)
	  (test +right+ 'actor nil)
	  (test +zero+ nil 'item-1)
	  (place item-2 +zero+ :solid nil)
	  (test (car +directions+) nil 'item-2)
	  (place obstacle '(2 . 0))
	  (move actor +right+)
	  (test +right+ 'actor nil)
	  (test '(2 . 0) 'obstacle nil)))))

(defun test-priority-lists ()
  (flag "testing priority lists")
  (print-test "appending works"
	      (equal (priority-append '((2 . b) (4 . d)) '((1 . a) (3 . c)))
		     '((1 . a) (2 . b) (3 . c) (4 . d))))
  (print-test "adding works"
	      (equal (priority-add '((1 . a) (2 . b)) '(3 . c))
		     '((1 . a) (2 . b) (3 . c)))))

(defun test-a-star ()
  (flag "testing a*")
  (with-clean-board
    (labels ((print-explored-cells (cells &optional path)
	       (loop for y from -1 to 6
		     do (format t "~{~c~}~%"
				(loop for x from -1 to 11
				      collect (cond ((or (equal (cons x y) +zero+)
							 (equal (cons x y) '(10 . 5)))
						     #\*)
						    ((member (cons x y) path :test #'equal)
						     #\@)
						    ((gethash (cons x y) cells)
						     #\#)
						    (t
						     #\.)))))))
      (multiple-value-bind (path cells) (a-star +zero+ '(10 . 5) (lambda (pos)
								   (declare (ignore pos))
								   1))
	(print-explored-cells cells path)))))

(defun test-throw ()
  (flag "testing throwing")
  (with-clean-board
    (let ((*player* (make-instance 'player :name 'actor))
	  (*inventory* nil)
	  (item (make-instance 'equipment :name 'item)))
      (flet ((test-inventory (value)
	       (let ((inventory-names (mapcar #'name *inventory*)))
		 (print-test "*inventory* = ~a" (equal inventory-names value) inventory-names)))
	     (test-board (pos value solid)
	       (let* ((obj (if solid
			       (solid pos)
			       (non-solid pos)))
		      (obj-name (when obj (name obj))))
		 (print-test "(~:[non-solid~;solid~] ~a) = ~a"
			     (eq obj-name value) solid pos obj-name))))
	(add-to-inventory item)
	(test-inventory '(item))
	(place *player* +zero+)
	(test-board +zero+ 'actor t)
	(throw-at +right+ item *player*)
	(test-inventory nil)
	(test-board +right+ 'item nil)
	(test-board +right+ nil t)))))

(defun test-status ()
  (flag "testing status")
  (let ((*player* (make-instance 'creature :name 'player))
	(status (make-instance 'status)))
    (apply-to *player* status)
    (print-test "player has one status"
		(equal (statuses *player*) (list status)))
    (loop for x below 3
	  do (update *player*)
	  do (print-test "status duration = ~d"
			 (= (duration status) (- 2 x))
			 (duration status)))
    (print-test "player statuses = ~a"
		(not (statuses *player*))
		(statuses *player*))))

(defenemy test-creature #\T () :health 7)
(defun test-max-health ()
  (flag "testing max health")
  (let ((test-creature (make-test-creature +zero+)))
    (print-test "max health = starting health"
		(eq (health test-creature) (max-health test-creature)))
    (incf (health test-creature))
    (print-test "health caps at max health"
		(eq (health test-creature) (max-health test-creature)))))

(defun test-shops ()
  (flag "testing shops")
  (with-clean-board
    (let ((cells (append (loop for x below 3 collect (cons x 0))
			 (loop for x below 6 collect (cons x 1))
			 (loop for x below 3 collect (cons x 2))
			 (loop for y from 1 to 4 collect (cons 5 y)))))
      (loop for y from -1 to 5
	    do (loop for x from -1 to 6
		     unless (member (cons x y) cells :test #'equal)
		       do (setf (solid (cons x y)) t)))
      (loop for y below 5
	    do (loop for x below 6
		     do (format t "~:[ ~;#~]" (member (cons x y) cells :test #'equal)))
	    do (terpri))
      (let ((shop-pos (get-shop-position cells :start '(5 . 4))))
	(print-test "shop positions correctly (shop at ~a)"
		    (equal shop-pos '(1 . 1))
		    shop-pos))))
  (let* ((shopkeeper (make-shopkeeper +zero+))
	 (shop-sword (make-sword-+1))
	 (*inventory* (list (make-sword) shop-sword))
	 (*shop-items* (list #'make-sword))
	 (*gold* 1))
    (setf (shopkeeper shop-sword) shopkeeper)
    (checkout)
    (print-test "can't buy item that's too expensive"
		(shopkeeper shop-sword))
    (with-fake-input #\0
      (sell-item shopkeeper)
      (print-test "sold item"
		  (and (= *gold* 2)
		       (= (length *inventory*) 1))))
    (checkout)
    (print-test "bought item"
		(not (shopkeeper shop-sword)))))

(defsecretequipment test-equipment ((cover-name :color 34 :char #\t)) () :color 31
  :char #\f)
(defun test-secret-equipment ()
  (flag "testing secret equipment")
  (let ((equipment (make-test-equipment)))
    (print-test "secret name" (eq (name equipment) 'cover-name))
    (print-test "color updated" (= (color equipment) 34))
    (print-test "character correct" (eq (slot-value equipment 'display-char) #\t))
    (identify equipment)
    (print-test "identified" (eq (name equipment) 'test-equipment))))

(defun test ()
  (test-combat)
  (test-masks)
  (test-vector-math)
  (test-equipment)
  (test-movement)
  (test-status)
  (test-max-health)
  (test-shops)
  (test-throw)
  (test-secret-equipment)
  (test-priority-lists)
  (test-a-star)
  (flag "testing tests")
  (print-test "~[all tests passed~:;~:*~d test~:p failed~]"
	      (= *tests-failed* 0)
	      *tests-failed*)
  (setf *tests-failed* 0))
