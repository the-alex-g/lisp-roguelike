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
  (let ((message (format nil "~:[~c[31mFAIL~;~c[32mPASS~]~:*~c[0m: ~?" passedp #\esc string args)))
    (format t "~&~a" message)
    (unless passedp
      (push (format nil "in ~a ~a" *scope* message)
	    *tests-failed*)))
  passedp)

(defun flag (string)
  (format t "~2%~:@(~a~)~2%" string))

(defun subheader (string)
  (format t "~&~a~%" (apply-color string 'yellow-4)))

(deftest tests
  (loop for test in (nreverse *tests-failed*)
	do (format t "~&~a" test))
  (print-test "~[all tests passed~:;~:*~d test~:p failed~]"
	      (= (length *tests-failed*) 0)
	      (length *tests-failed*))
  (setf *tests-failed* nil))

(deftest cooking
    (let ((food (make-food)))
      (setf (sustenance food) 10)
      (cook food)
      (print-test "food is cooked" (string= (name food) "cooked food"))
      (print-test "sustenance increased" (= (sustenance food) 15))
      (cook food)
      (print-test "food is burnt" (string= (name food) "burnt food"))
      (print-test "sustenance decreased" (= (sustenance food) 5))))

(deftest fire
  (with-clean-board
    (let ((fire (make-fire +zero+))
	  (goblin (make-goblin +zero+)))
      (make-faggot-pickup +zero+)
      (print-test "fire consumed faggot... ~d fuel" (= (fuel fire) 100)
		  (fuel fire))
      (loop repeat (fuel fire) do (update fire))
      (print-test "fire went out" (not (non-solid +zero+)))
      (print-test "goblin died" (deadp goblin)))))

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

(defenemy test-enemy #\Q () :health 10 :armor +no-armor+)
(deftest combat
  (let ((enemy (make-test-enemy +zero+))
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
      (my-attack "status" status-attack 10)
      (print-test "target has one status"
		  (= (length (statuses enemy)) 1))
      (my-attack "killing" attack3 0))))

(deftest movement-costs
  (with-clean-board
      (make-pit-trap +zero+)
    (setf (terrain +right+) 'standard)
    (setf (terrain +zero+) 'standard)
    (print-test "movement cost of nowhere is 1" (= 1 (movement-cost +right+)))
    (print-test "actual movement cost is low ~d" (= 1 (movement-cost +zero+ :actual t))
		(movement-cost +zero+ :actual t))
    (print-test "percieved movement cost is high ~d"
		(= 11 (movement-cost +zero+))
		(movement-cost +zero+))))

(deftest traps
  (with-clean-board
    (let* ((mark (make-goblin +zero+))
	   (initial-health (health mark))
	   (trap-1 (make-pit-trap '(1 . 0)))
	   (trap-2 (make-pit-trap '(2 . 0))))
      (setf (armor mark) +no-armor+)
      (setf (dex+ mark) -100)
      (move-into trap-1 mark t)
      (print-test "mark was damaged (~d to ~d health)" (< (health mark) initial-health)
		  initial-health (health mark))
      (setf (health mark) initial-health)
      (move-into trap-2 mark nil)
      (print-test "mark didn't fall in" (= initial-health (health mark)))
      (setf (health mark) 1)
      (move mark +right+)
      (print-test "mark died in trap" (deadp mark))
      (print-test "mark is gone" (not (solid (pos mark)))))))

(deftest checks
  (let ((tester (make-goblin +zero+)))
    (setf (dex+ tester) 100)
    (setf (str+ tester) -100)
    (print-test "check succeeds" (checkp #'dex+ tester 10))
    (print-test "check fails" (not (checkp #'str+ tester 10)))))

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
    (animate-dead *player* :die 8 :succeedp t)
    (print-test "animated corpse" (solid '(2 . 0)))
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
      (let ((damage (life-drain caster target :succeedp t)))
	(print-test "target was damaged" (= (health target)
					    (max 0 (- target-initial-health damage))))
	(print-test "caster was healed"
		    (= (health caster) (max-health caster))))))
  (subheader "enervate")
  (with-clean-board
    (let* ((caster (make-goblin +zero+))
	   (target (make-goblin +right+))
	   (target-initial-health (health target)))
      (setf (knl+ caster) 1000) ;; so that there's no chance of avoiding the status
      (let ((damage (enervate caster target)))
	(print-test "target was damaged" (= (health target)
					    (max 0 (- target-initial-health damage))))
	(print-test "target was inflicted with a status"
		    (= (length (statuses target)) 1)))
      (setf (knl+ caster) -100)
      (enervate caster target :succeedp t)
      (print-test "target avoided the status (~d statuses)"
		  (= (length (statuses target)) 1)
		  (length (statuses target))))))

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
	(make-pit-trap '(2 . 2))
      (labels ((print-explored-cells (cells &optional path)
		 (loop for y from -1 to 6
		       do (format t "~{~a~}~%"
				  (loop for x from -1 to 11
					collect (cond ((contents (cons x y))
						       (display-char (contents (cons x y))))
						      ((or (equal (cons x y) +zero+)
							   (equal (cons x y) '(10 . 5)))
						       #\*)
						      ((member (cons x y) path :test #'equal)
						       #\@)
						      ((gethash (cons x y) cells)
						       #\#)
						      (t
						       #\.)))))))
	(multiple-value-bind (path cells) (a-star +zero+ '(10 . 5) #'movement-cost)
	  (print-explored-cells cells path)
	  (print-test "shortest path found" (= (length path) 11))))))

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
  (let ((status (make-instance 'status :spd 1 :duration 3)))
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

(deftest max-health
  (let ((test-creature (make-goblin +zero+)))
    (print-test "max health = starting health"
		(eq (health test-creature) (max-health test-creature)))
    (incf (health test-creature))
    (print-test "health caps at max health"
		(eq (health test-creature) (max-health test-creature)))))

(deftest bows
  (let ((*inventory* nil)
	(quiver (make-quiver 1))
	(bow (make-bow)))
    (setf (gethash 'hand (equipment *player*)) (list bow))
    (print-test "cannot attack without arrows" (not (can-attack-p bow *player*)))
    (add-to-inventory quiver)
    (print-test "quiver has one arrow" (= 1 (arrows quiver)))
    (print-test "can attack with arrows" (can-attack-p bow *player*))
    (attack (make-goblin +zero+) *player*)
    (print-test "used an arrow" (= 0 (arrows quiver)))
    (print-test "cannot attack without arrows" (not (can-attack-p bow *player*)))))

(deftest disease
  (with-clean-board
    (let ((goblin (make-goblin +zero+))
	  (graverot (make-graverot-status))
	  (resting (make-resting-status)))
      (apply-to goblin graverot)
      (setf (new-day graverot) t)
      (update graverot)
      (print-test "day updates" (not (new-day graverot)))
      (apply-to goblin resting)
      (remove-status resting)
      (print-test "new day again" (new-day graverot)))))

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
    (let ((equipment (make-test-equipment))
	  (wand (make-wand))
	  (wand-2 (make-wand)))
      (setf (spell wand) *animate-dead*)
      (setf (spell wand-2) *life-drain*)
      (print-test "secret name" (eq (name equipment) 'cover-name))
      (print-test "color updated" (= (color equipment) 34))
      (print-test "character correct" (eq (slot-value equipment 'display-char) #\t))
      (identify equipment)
      (print-test "identified" (eq (name equipment) 'test-equipment))
      (print-test "wand unidentified" (eq (name wand) 'unidentified-wand))
      (identify wand)
      (setf (spell-die-index wand) 2)
      (print-test "wand identified" (string= (name wand) "wand of animate dead"))
      (print-test "other wand unidentified" (eq (name wand-2) 'unidentified-wand))))

(deftest armor
  (let* ((armor (make-leather-armor))
	 (goblin (make-goblin +zero+))
	 (initial-armor (slot-value goblin 'armor)))
    (equip armor goblin)
    (print-test "armor updated" (= (slot-value goblin 'armor) (armor armor)))
    (unequip armor goblin :to 'nowhere)
    (print-test "armor updated" (= (slot-value goblin 'armor) initial-armor))))

(defun test ()
  (mapc #'funcall *tests*)
  (format t "~2%"))
