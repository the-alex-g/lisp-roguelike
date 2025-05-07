(defparameter *tests-failed* nil)
(defparameter *tests* nil)
(defparameter *scope* nil)

(defmacro with-clean-board (&body body)
  `(let ((*current-layer* (make-layer)))
     ,@body))

(defmacro deftest (name &body body)
  `(push
    (defun ,(read-from-string (format nil "test-~a" name)) ()
      (flag (log-to-string "testing ~a" ',name))
      (let ((*scope* ',name))
	,@body))
    *tests*))

;;; override default illuminatedp function
(defun illuminatedp (pos)
  t)

(defun print-test (string passedp &rest args)
  (format t "~&~:[~c[31mFAIL~;~c[32mPASS~]~:*~c[0m: ~?" passedp #\esc string args)
  (unless passedp
    (push *scope* *tests-failed*))
  passedp)

(defun flag (string)
  (format t "~2%~:@(~a~)~2%" string))

(defun subheader (string)
  (format t "~&~a~%" (apply-color string 'yellow-4)))

(deftest tests
  (print-test "~[all tests passed~:;~:*~d test~:p failed in scope~:p ~{~a~^ ~}~]"
	      (= (length *tests-failed*) 0)
	      (length *tests-failed*)
	      *tests-failed*)
  (setf *tests-failed* nil))

(deftest masks
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
				2))
  (with-clean-board
    (let ((creature (make-goblin +zero+)))
      (print-test "accessed value is a number" (numberp (allies creature)))
      (setf (allies creature) 10)
      (print-test "value can be changed to a number" (= (allies creature) 10))
      (setf (allies creature) '(kobold evil))
      (print-test "value can be changed to a list"
		  (= (allies creature) (make-mask '(kobold evil)))))))

(deftest combat
  (let ((enemy (make-instance 'creature :name "enemy" :health 10))
	(attack1 (make-attack :amount 5 :to-hit 10 :types '(slashing)))
	(attack2 (make-attack :amount 5 :to-hit 5 :types '(slashing)))
	(attack3 (make-attack :amount 10 :to-hit 10 :types '(bludgeoning)))
	(status-attack (make-attack :amount 0 :to-hit 10 :types '(piercing)
				    :statuses (list (make-instance 'status)))))
    (flet ((my-attack (text atk val)
	     (subheader text)
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

(deftest corpse-decays
  (with-clean-board
      (let ((corpse (drop-corpse (make-goblin '(-10 . -10)))))
	(loop repeat (decay-time corpse)
	      do (update corpse))
	(print-test "it decayed" (eq (type-of (non-solid (pos corpse))) 'bones))
	(print-test "it's named correctly"
		    (string= (name (non-solid (pos corpse))) "goblin bones")))))

(deftest spells
  (subheader "animate dead")
  (with-clean-board
      (place *player* +zero+)
    (make-corpse '(2 . 0))
    (make-corpse '(4 . 0))
    (make-bones '(3 . 0))
    (make-dagger-pickup '(-1 . 0))
    (animate-dead *player*)
    (print-test "animated corpse"
		(eq (name (solid '(2 . 0))) 'zombie))
    (print-test "corpse gone"
		(not (non-solid '(2 . 0))))
    (print-test "animated bones"
		(eq (name (solid '(3 . 0))) 'skeleton))
    (print-test "missed corpse"
		(not (solid '(4 . 0))))
    (print-test "dagger unharmed" (eq (name (contents '(-1 . 0))) 'dagger))
    (let ((zombie (solid '(2 . 0)))
	  (goblin (make-goblin '(1 . 1))))
      (when zombie
	(print-test "zombie aligned properly"
		    (= (enemies zombie) (enemies *player*)))
	(print-test "zombie hostile to goblin"
		    (hostilep zombie goblin))
	(print-test "goblin neutral to zombie"
		    (not (hostilep goblin zombie)))
	(print-test "zombie not hostile to player"
		    (not (hostilep zombie *player*))))))
  (subheader "life drain")
  (with-clean-board
    (let* ((caster (make-goblin +zero+))
	   (target (make-goblin +right+))
	   (target-initial-health (health target)))
      (setf (slot-value caster 'max-health) (1+ (health caster)))
      (let ((damage (life-drain caster target)))
	(print-test "target was damaged" (= (health target)
					    (max 0 (- target-initial-health damage))))
	(print-test "caster was healed"
		    (= (health caster) (max-health caster))))))
  (subheader "enervate")
  (with-clean-board
    (let* ((caster (make-goblin +zero+))
	   (target (make-goblin +right+))
	   (target-initial-health (health target)))
      (setf (intl caster) 1000)
      (let ((damage (enervate caster target)))
	(print-test "target was damaged ~d (from ~d to ~d)" (= (health target)
					       (max 0 (- target-initial-health damage)))
		    damage (health target) target-initial-health)
	(print-test "target was inflicted with a status"
		    (= (length (statuses target)) 1))))))

(deftest vector-math
  (flet ((test (test value function &rest args)
	   (let ((result (apply function args)))
	     (print-test "(~a ~{~a~^ ~}) = ~a"
		     (funcall test result value) function args result))))
    (test #'equal '(1 . 1) 'vec+ '(1 . 0) '(0 . 1))
    (test #'equal '(0 . 1) 'vec- '(1 . 1) '(1 . 0))
    (test #'equal '(-1 . -1) 'vec- '(1 . 1))
    (test #'= 5.0 (lambda (foo) (vec-length foo :exactp t)) '(3 . 4))
    (test #'= 4 #'vec-length '(3 . 4))))

(deftest equipment
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

(deftest movement
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
	  (test (car (last +directions+)) nil 'item-2)
	  (place obstacle '(2 . 0))
	  (move actor +right+)
	  (test +right+ 'actor nil)
	  (test '(2 . 0) 'obstacle nil)))))

(deftest priority-lists
  (print-test "appending works"
	      (equal (priority-append '((2 . b) (4 . d)) '((1 . a) (3 . c)))
		     '((1 . a) (2 . b) (3 . c) (4 . d))))
  (print-test "adding works"
	      (equal (priority-add '((1 . a) (2 . b)) '(3 . c))
		     '((1 . a) (2 . b) (3 . c)))))

(deftest a-star
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

(deftest throw
  (with-clean-board
    (let ((*player* (make-instance 'player :name 'actor))
	  (*inventory* nil)
	  (item (make-instance 'equipment :name 'item :break-chance -100)))
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

(deftest statuses
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
(deftest max-health
  (let ((test-creature (make-test-creature +zero+)))
    (print-test "max health = starting health"
		(eq (health test-creature) (max-health test-creature)))
    (incf (health test-creature))
    (print-test "health caps at max health"
		(eq (health test-creature) (max-health test-creature)))))

(deftest shops
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
      (let ((shop-pos (get-spawn-position cells 8 :start '(5 . 4))))
	(print-test "shop positions correctly (shop at ~a)"
		    (equal shop-pos '(1 . 1))
		    shop-pos))))
  (with-clean-board
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
		    (not (shopkeeper shop-sword)))
	(print-test "shopkeeper not hostile" (not (hostilep shopkeeper *player*)))
	(attack shopkeeper (make-attack :source *player* :to-hit 0 :types '(slashing)))
	(place *player* '(2 . 2))
	(print-test "shopkeeper becomes hostile even on a miss"
		    (hostilep shopkeeper *player*))
	(loop repeat 100 until (deadp *player*)
	      do (act shopkeeper)
	      finally (act shopkeeper))
	(print-test "shopkeeper kills player" (deadp *player*))
	(print-test "shopkeeper no longer hostile" (not (hostilep shopkeeper *player*)))
	(loop repeat 100 until (equal (pos shopkeeper) (home shopkeeper))
	      do (act shopkeeper))
	(print-test "shopkeeper goes home" (equal (pos shopkeeper) (home shopkeeper))))))

(defsecretequipment test-equipment ((cover-name :color 34 :char #\t)) () :color 31
  :char #\f)
(deftest secret-equipment
  (let ((equipment (make-test-equipment)))
    (print-test "secret name" (eq (name equipment) 'cover-name))
    (print-test "color updated" (= (color equipment) 34))
    (print-test "character correct" (eq (slot-value equipment 'display-char) #\t))
    (identify equipment)
    (print-test "identified" (eq (name equipment) 'test-equipment))))

(defun test ()
  (mapc #'funcall *tests*)
  (format t "~2%"))
