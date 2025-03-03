(defmacro with-clean-board (&body body)
  `(let ((*solid-actors* (make-hash-table :test #'equal))
	 (*non-solid-actors* (make-hash-table :test #'equal)))
     ,@body))

(defun print-test (string passedp &rest args)
  (format t "~&~:[FAIL~;PASS~]: ~?" passedp string args))

(defun flag (string)
  (format t "~2%~:@(~a~)~2%" string))

(defun test-combat ()
  (flag "TESTING COMBAT")
  (let ((enemy (make-instance 'creature :name "enemy" :health 10))
	(attack1 (make-attack :dmg 5 :to-hit 10 :source "player" :types '(slashing)))
	(attack2 (make-attack :dmg 5 :to-hit 5 :source "player" :types '(slashing)))
	(attack3 (make-attack :dmg 10 :to-hit 10 :source "player" :types '(bludgeoning))))
    (flet ((my-attack (text atk val)
	     (format t "~&~:(~a~)" text)
	     (attack enemy atk)
	     (print-test "health remaining: ~d" 
			 (= (health enemy) val)
			 (health enemy))))
      (my-attack "basic attack" attack1 5)
      (setf (resistances enemy) '(slashing))
      (my-attack "resistances" attack1 3)
      (setf (absorbances enemy) '(slashing))
      (my-attack "absorbance" attack1 8)
      (my-attack "health overflow" attack1 10)
      (my-attack "missing" attack2 10)
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

(defun test-throw ()
  (flag "testing throwing")
  (with-clean-board
    (let ((*player* (make-instance 'creature :name 'actor))
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

(defun test ()
  (test-combat)
  (test-vector-math)
  (test-equipment)
  (test-movement)
  (test-throw))
