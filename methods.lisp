(macrolet ((define-mask-accessors (&rest names)
	     `(progn
		,@(loop for name in names
			collect `(defmethod (setf ,name) ((value number) (obj creature))
				   (setf (slot-value obj ',name) value))
			collect `(defmethod (setf ,name) ((value list) (obj creature))
				   (setf (slot-value obj ',name) (make-mask value)))
			collect `(defmethod ,name ((obj creature))
				   (let ((value (slot-value obj ',name)))
				     (if (numberp value)
					 value
					 (let ((mask (make-mask value)))
					   (setf (,name obj) mask)
					   mask))))))))
  (define-mask-accessors resistances immunities absorbances vulnerabilities
    allies enemies types))

(defmethod quaffablep ((obj potion))
  (declare (ignore obj))
  t)

(defmethod zappablep ((obj wand))
  (declare (ignore obj))
  t)

(defmethod playerp ((obj player))
  (declare (ignore obj))
  t)

(defmethod drop-corpse ((obj enemy))
  (let ((corpse (make-corpse (pos obj))))
    (setf (name corpse) (log-to-string "~a corpse" (name obj)))
    (setf (loot corpse) (get-loot obj))
    corpse))

(defmethod drop-corpse ((obj sprout)))

(defmethod drop-corpse ((obj grenadier-sprout))
  (make-sprout-bomb-pickup (pos obj)))

(defmethod get-loot ((obj enemy))
  (let ((loot '()))
    (when (meat obj)
      (push (meat obj) loot))
    (loop for item in (loot obj)
	  if (and item (atom item))
	    do (push item loot)
	  else
	    do (let ((bit (eval-weighted-list item)))
		 (when (car bit)
		   (mapc (lambda (b) (push b loot)) bit))))
    (loop for item-list being the hash-values of (equipment obj)
	  do (loop for item in item-list
		   unless (breaksp item 50)
		     do (push item loot)))
    loot))

(defmethod hiddenp ((obj ladder))
  (or (and (= *current-depth* 0)
	   (= -1 (direction obj)))
      (and (= *current-depth* (1- (length *layers*)))
	   (= 1 (direction obj)))))

(defmethod hiddenp ((obj symbol)) t)

(defmethod pickup :around ((item gold))
  (remove-non-solid (pos item))
  (incf *gold* (amount item))
  (print-to-log "you picked up ~d gold" (amount item)))

(defmethod interact ((object corpse) (actor (eql *player*)))
  (mapc (lambda (i) (interact i actor)) (loot object))
  (remove-non-solid (pos object)))

(defmethod quaff ((item healing-potion) (actor player))
  (print-to-log "you quaffed ~a and ~a" (name item) (heal actor (healing item))))

(defmethod eat ((item food) (actor player))
  (incf (hunger actor) (sustenance item))
  (print-to-log "you ate ~a and recovered ~d hunger"
		(name item)
		(sustenance item)))

(defmethod remove-status ((status elevated))
  (decf (dex (target status))))

(defmethod trigger :around ((trap trap) activator)
  (setf (hiddenp trap) nil)
  (if (>= (roll 1 20 (dex activator)) (avoid-dc trap))
      (progn (print-to-log "you triggered a ~a but dodged out of the way"
			   (name trap))
	     (flag-warning))
      (call-next-method)))

(defmethod trigger ((trap pit-trap) (activator creature))
  (let ((damage (damage activator (make-damage :source trap
					       :amount (roll 1 6)
					       :types '(bludgeoning)))))
    (when (playerp activator)
      (print-to-log "you triggered a ~a and took ~d damage"
		    (name trap)
		    damage)
      (flag-alert))))

(defmethod evasion ((obj creature))
  (max 1 (+ 5 (dex obj) (slot-value obj 'evasion))))

(defmethod evadesp ((obj creature) dc)
  (>= (roll 1 20 (dex obj)) dc))

(defmethod name ((obj ladder))
  (cond ((= -1 (direction obj))
	 'ladder-leading-up)
	((= 1 (direction obj))
	 'ladder-leading-down)))

(defmethod name ((obj wand))
  (log-to-string "wand of ~a" (spell-name (spell obj))))

(defmethod name ((obj character)) 'wall)

(defmethod name ((obj symbol)) obj)

(defmethod name :around ((obj secret-equipment))
  (if (identifiedp obj)
      (call-next-method)
      (cover-name obj)))

(defmethod (setf health) (value (obj creature))
  (cond ((<= value 0)
	 (setf (slot-value obj 'health) 0))
	((slot-boundp obj 'max-health)
	 (setf (slot-value obj 'health) (min value (max-health obj))))
	(t
	  (setf (slot-value obj 'max-health) (health obj))
	  (setf (slot-value obj 'health) value))))

(defmethod (setf health) (value (obj sprout-hulk))
  (if (> value 0)
      (setf (slot-value obj 'health) value)
      (setf (slot-value obj 'health) 0)))

(defmethod (setf con) (value (obj creature))
  (let ((dhealth (- value (con obj))))
    (incf (slot-value obj 'max-health)
	  dhealth)
    (incf (health obj) dhealth)))

(defmethod (setf hunger) (value (obj player))
  (cond ((> value (max-hunger obj))
	 (setf (slot-value obj 'hunger) (max-hunger obj)))
	((< value 0)
	 (setf (slot-value obj 'hunger) 20)
	 (decf (health obj)))
	(t
	 (setf (slot-value obj 'hunger) value))))

(defmethod (setf illumination) (value (obj actor))
  (let ((current-value (illumination obj)))
    (when (and (<= current-value 0)
	       (> value 0))
      (add-glowing obj))
    (when (and (> current-value 0)
	       (<= value 0))
      (remove-glowing obj)))
  (setf (slot-value obj 'illumination) value))

(defmethod illumination ((pos list))
  (gethash pos *lightmap* 0))

(defmethod stationaryp ((obj creature))
  nil)

(defmethod equip :around ((item equipment) (actor player))
  (if (shopkeeper item)
      (progn (print-to-log "you must buy that before equipping it")
	     nil)
      (let ((result (call-next-method)))
	(when result
	  (remove-from-inventory item))
	result)))

(defmethod equip :around ((item equipment) (actor creature))
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
		  items-to-unequip))))))))

(defmethod unequip :after ((item equipment) (actor player) &key (to 'inventory))
  (declare (ignore actor))
  (cond ((eq to 'inventory)
	 (add-to-inventory item))
	((eq to 'ground)
	 (place item (pos *player*) :solid nil))))

(defmethod unequip :after ((item equipment) (actor creature) &key to)
  (declare (ignore to))
  (setf (gethash (equip-slot item) (equipment actor))
	(remove item (gethash (equip-slot item) (equipment actor)) :test #'equal)))

(defmethod add-to-inventory ((item equipment))
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
    (setf *shopkeeper* (shopkeeper item))))

(defmethod remove-from-inventory ((item debt) &key (selling nil) &allow-other-keys)
  (if selling
      (call-next-method)
      (progn (print-to-log "you can't get rid of debt that easily")
	     nil)))

(defmethod remove-from-inventory ((item equipment) &key (exact-match-p t) &allow-other-keys)
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
		  removed-item)))))

(defmethod surrounding-name ((obj equipment))
  (log-to-string "~a~:[~; (~d gold)~]"
		 (name obj)
		 (shopkeeper obj)
		 (price obj)))

(defmethod surrounding-name ((obj actor))
  (name obj))

(defmethod throw-at :before (target (item equipment) (thrower player))
  (declare (ignore target))
  (when (equippedp item thrower)
    (unequip item thrower))
  (remove-from-inventory item))

(defmethod throw-at ((target creature) (item equipment) (thrower creature))
  (attack target (get-attack item thrower)))

(defmethod throw-at (target (item sprout-bomb) (thrower creature))
  (declare (ignore item))
  (throw-sprout-bomb (pos target) thrower))

(defmethod throw-at :after ((target list) (item equipment) thrower)
  (unless (breaksp item)
    (place item target :solid nil)))

(defmethod update :before ((obj creature))
  (mapc #'update (statuses obj))
  (incf (idle-time obj)))

(defmethod update :around ((obj status))
  (incf (energy obj) (/ (spd obj) (spd *player*)))
  (loop while (and (>= (energy obj) 1)
		   (not (= (duration obj) 0)))
	do (decf (energy obj))
	do (call-next-method)
	do (decf (duration obj))
	finally (if (= (duration obj) 0)
		    (remove-status obj))))

(defmethod update ((obj corpse))
  (decf (decay-time obj))
  (when (= (decay-time obj) 0)
    (remove-non-solid (pos obj))
    (drop-bones obj)))

(defmethod update ((obj elevated))
  (let ((f (non-solid (pos (target obj)))))
    (unless (and f (eq (name f) 'table))
      (remove-status obj))))

(defmethod update ((obj resting))
  (when (playerp (target obj))
    (incf (hunger (target obj))))
  (incf (health (target obj)))
  (when (or (= (health (target obj)) (max-health (target obj)))
	    (and (slot-exists-p obj 'target-pos) (target-pos obj)))
    (remove-status obj)))

(defmethod update ((obj player))
  (decf (hunger obj) (/ 1 (spd *player*))))

(defmethod update :around ((obj creature))
  (unless (deadp obj)
    (call-next-method)))

(defmethod update ((obj enemy))
  (incf (energy obj) (/ (spd obj) (spd *player*)))
  (loop while (>= (energy obj) 1)
	do (decf (energy obj)
		 (let ((result (act obj)))
		   (if (numberp result)
		       result
		       1)))))

(defmethod level ((obj creature))
  (* (1+ (str obj))
     (/ (health obj) (max-health obj))))

(defmethod act :around ((obj shopkeeper) &key &allow-other-keys)
  (setf (targets obj)
	(loop for target in (targets obj)
	      unless (deadp target)
		collect target))
  (cond ((targets obj)
	 (move-towards (pos (car (targets obj))) obj #'movement-cost))
	((not (equal (pos obj) (home obj)))
	 (move-towards (home obj) obj #'movement-cost))))

(defmethod act :around ((obj enemy) &key &allow-other-keys)
  (multiple-value-bind (foes allies) (get-actors-in-los-of obj t nil nil
							   (hostilep obj actor)
							   (alliedp obj actor))
    (if (eq (morale obj) 'fearless)
	(call-next-method obj :foes foes :allies allies)
	(let* ((allied-strength (+ (level obj)
				   (morale obj)
				   (loop for ally in allies
					 sum (fear-of ally (pos obj)))))
	       (fearsome-foes (loop for foe in foes
				    when (> (level foe) allied-strength)
				      collect foe))
	       (fear (loop for foe in fearsome-foes sum (fear-of foe (pos obj)))))
;	  (when (visiblep obj *player*)
;	    (print-to-log "fear: ~d   strength: ~d" fear allied-strength))
	  (flet ((heuristic (pos)
		   (+ (movement-cost pos)
		      (loop for foe in fearsome-foes
			    sum (fear-of foe pos)))))
	    (if (< fear allied-strength)
		(call-next-method obj :foes foes :allied-strength allied-strength
				      :heuristic #'heuristic :allies allies)
		(flee obj #'heuristic)))))))

(defmethod act ((obj enemy)
		&key foes (allied-strength 0) (heuristic #'movement-cost)
		&allow-other-keys)
  (let ((target (get-closest-of-list obj (or (loop for foe in foes
						   when (<= (level foe) allied-strength)
						     collect foe)
					     foes)))
	(primary (car (weapons obj))))
    (when target
      (setf (target-pos obj) (pos target)))
    (cond ((has-status-p obj 'resting)
	   ;; if resting, do nothing
	   nil)
	  ((and target (<= (distance (pos obj) (pos target)) (range primary)))
	   ;; if has target and target is in range, attack
	   (attack target obj))
	  ((and (not target) (< (health obj) (/ (max-health obj) 2)))
	   ;; if injured and no target, rest
	   (apply-to obj (make-resting-status)))
	  ((target-pos obj)
	   ;; if knows about a target, move towards it
	   (move-towards (target-pos obj) obj heuristic))
	  ((< (health obj) (max-health obj))
	   ;; damaged at all
	   (apply-to obj (make-resting-status)))
	  (t
	   ;; idle
	   (funcall (idle-behavior obj) obj)))))

(defmethod act ((obj sprout) &key allies foes &allow-other-keys)
  (let ((target (or (get-closest-of-list obj allies)
		    (get-closest-of-list obj foes))))
    (when target
      (move-towards (pos target) obj (lambda (pos) 1)))))

(defmethod act ((obj sprout-hulk) &key allies foes &allow-other-keys)
  (let ((target (or (get-closest-of-list obj foes)
		    (get-closest-of-list obj allies))))
    (when target
      (move-towards (pos target) obj (lambda (pos) 1)))))

(defmethod act ((obj grenadier-sprout) &key allies foes &allow-other-keys)
  (let ((ally (get-closest-of-list obj allies))
	(foe  (get-closest-of-list obj foes)))
    (cond (ally
	   (move-towards (pos ally) obj (lambda (pos) 1)))
	  ((and foe (< 1 (distance (pos foe) (pos obj)) 3))
	   (throw-at foe (make-sprout-bomb) obj))
	  (foe
	   (move-towards (pos foe) obj (lambda (pos) 1))))))

(defmethod act :after ((obj enemy) &key &allow-other-keys)
  (when (equal (pos obj) (target-pos obj))
    (setf (target-pos obj) nil)))

(defmethod move :around ((obj player) direction)
  (if *shopkeeper*
      (let ((new-pos (vec+ (pos obj) direction)))
	(if (and (not (solid new-pos))
		 (or (> (distance new-pos (pos *shopkeeper*))
			(domain *shopkeeper*))
		     (not (visiblep (home *shopkeeper*) new-pos))))
	    (if (confirmp  "if you move there, you will be stealing from the shopkeeper~
                                ~%do you want to move anyway?")
		(progn (steal-items)
		       (call-next-method))
		0)
	    (call-next-method)))
      (call-next-method)))

(defmethod move :after ((obj creature) direction)
  (declare (ignore direction))
  (setf (idle-time obj) 0))

(defmethod move ((obj actor) direction)
  (reposition obj (vec+ (pos obj) direction)))

(defmethod move ((pos list) direction)
  (let ((newpos (vec+ pos direction)))
    (if (wallp (solid newpos))
	pos
	newpos)))

(defmethod reposition :around ((obj creature) new-pos)
  (if (has-status-p obj 'immobilized)
      1
      (call-next-method)))

(defmethod reposition ((obj actor) new-pos)
  (let ((collider (solid new-pos))
	(cost (move-into new-pos obj)))
    (unless collider
      (force-movement obj new-pos))
    (if collider 1 cost)))

(defmethod move-into ((position list) active)
  (let ((cost (movement-cost position)))
    (mapc (lambda (p)
	    (move-into p active))
	  (contents position :all t))
    cost))

(defmethod move-into ((passive breakable) (active player))
  (when (and (equal (solid (pos passive)) passive)
	     (confirmp "do you want to attack the ~a?" (name passive)))
    (attack passive active)))

(defmethod move-into ((passive creature) (active player))
  (when (if (hostilep active passive)
	    t
	    (confirmp "that creature does not appear hostile. Do you want to attack it?"))
    (attack passive active)))

(defmethod move-into ((passive creature) (active creature))
  (when (hostilep active passive)
    (attack passive active)))

(defmethod move-into ((passive table) (active creature))
  (apply-to active (make-elevated-status)))

(defmethod move-into ((passive sprout) (active sprout))
  (remove-solid (pos passive))
  (remove-solid (pos active))
  (setf (health (make-sprout-hulk (pos passive)))
	(+ (health passive) (health active)))
  (setf (deadp passive) t)
  (setf (deadp active) t)
  (when (visiblep (pos passive) *player*)
    (print-to-log "two sprouts combine to form a sprout hulk")))

(defmethod move-into ((passive sprout) (active sprout-hulk))
  (force-movement active (pos passive))
  (setf (deadp passive) t)
  (incf (health active) (health passive))
  (when (visiblep (pos passive) *player*)
    (print-to-log "the sprout hulk absorbs a smaller sprout"))
  (movement-cost (pos passive)))

(defmethod move-into ((passive sprout-hulk) (active sprout))
  (remove-solid (pos active))
  (setf (deadp active) t)
  (incf (health passive) (health active))
  (when (visiblep (pos passive) *player*)
    (print-to-log "the sprout hulk absorbs a smaller sprout")))

(defmethod move-into ((passive trap) (active player))
    (when (< (random 100) (trigger-chance passive))
      (trigger passive active)))

(defmethod move-into ((passive shopkeeper) (active player))
  (if (hostilep passive active)
      (attack passive active)
      (let ((action (get-item-from-list '(attack sell buy) :what 'option)))
	(cond ((eq action 'attack)
	       (attack passive active))
	      ((eq action 'sell)
	       (sell-item passive))
	      ((eq action 'buy)
	       (checkout))))))

(defmethod heal ((actor player) amount &key to-print &allow-other-keys)
  (let ((previous-health (health actor)))
    (call-next-method)
    (if to-print
	(print-to-log "you healed ~d" (- (health actor) previous-health))
	(log-to-string "healed ~d" (- (health actor) previous-health)))))

(defmethod heal ((actor creature) amount &key &allow-other-keys)
  (incf (health actor) amount))

(defmethod interact ((item equipment) (actor player))
  (pickup item))

(defmethod interact ((item ladder) (actor player))
  (unless (hiddenp item)
    (remove-solid (pos *player*))
    (remove-glowing (pos *player*))
    (incf *current-depth* (direction item))
    (setf *current-layer* (nth *current-depth* *layers*))
    (place *player* (if (= (direction item) -1)
			(down-ladder-pos)
			(up-ladder-pos)))))

(DEFMETHOD LOOK-AT (OBJECT)
  (DECLARE (IGNORE OBJECT))
  (PRINT-TO-LOG "you see nothing there"))

(DEFMETHOD LOOK-AT ((POS LIST))
  (IF POS
      (LOOK-AT (CONTENTS POS))
      (CALL-NEXT-METHOD)))

(DEFMETHOD LOOK-AT ((OBJECT ACTOR))
  (IF (VISIBLEP (POS OBJECT) (POS *PLAYER*))
      (PRINT-TO-LOG "you see a ~a" (NAME OBJECT))
      (CALL-NEXT-METHOD)))

(DEFMETHOD LOOK-AT ((OBJECT TRAP))
  (COND ((SEARCHEDP OBJECT) (CALL-NEXT-METHOD))
        ((>= (ROLL 1 20 (PER *PLAYER*)) (FIND-DC OBJECT))
         (PRINT-TO-LOG "you found a ~a" (NAME OBJECT))
         (SETF (HIDDENP OBJECT) NIL))
        (T (CALL-NEXT-METHOD)))
  (SETF (SEARCHEDP OBJECT) T))

(DEFMETHOD LOOK-AT ((OBJECT CHARACTER))
  (IF (WALLP OBJECT)
      (PRINT-TO-LOG "you see a wall")
      (CALL-NEXT-METHOD)))

(DEFMETHOD MOVEMENT-COST ((POSITION LIST))
  (APPLY #'+ (MAPCAR #'MOVEMENT-COST (CONTENTS POSITION :ALL T))))

(DEFMETHOD MOVEMENT-COST ((TERRAIN SYMBOL))
  (GETHASH TERRAIN *TERRAIN-COSTS* 1))

(DEFMETHOD MOVEMENT-COST ((CREATURE CREATURE))
  (IDLE-TIME CREATURE))

(DEFMETHOD APPLY-TO :BEFORE ((SUBJ CREATURE) (OBJ STATUS))
  (SETF (TARGET OBJ) SUBJ)
  (PUSH OBJ (STATUSES SUBJ)))

(defmethod apply-to :before ((subj enemy) (obj resting))
  (setf (target-pos subj) nil))

(defmethod apply-to ((subj creature) (obj elevated))
  (incf (dex subj)))

(DEFMETHOD DAMAGE ((DEFENDER CREATURE) (damage damage))
  (LET* ((BASE-DAMAGE (MAX 1 (- (damage-amount damage) (ARMOR DEFENDER))))
         (MOD-DAMAGE (ROUND (* BASE-DAMAGE (DAMAGE-MODIFIER DEFENDER (damage-types damage)))))
         (REAL-DAMAGE
          (COND ((= MOD-DAMAGE 0) 0) ((< MOD-DAMAGE 0) MOD-DAMAGE)
                (T (MAX 1 MOD-DAMAGE)))))
    (DECF (HEALTH DEFENDER) REAL-DAMAGE)
    (mapc (lambda (status) (apply-to defender status)) (damage-statuses damage))
    (MAX 0 REAL-DAMAGE)))

(DEFMETHOD DAMAGE ((DEFENDER BREAKABLE) (damage damage))
  (INCF (BREAK-CHANCE DEFENDER) (* 5 (damage-amount damage)))
  (damage-amount damage))

(defmethod damage :after ((defender creature) (damage damage))
  (when (slot-exists-p (damage-source damage) 'types)
    (make-hostile defender (damage-source damage))))

(defmethod damage :after (defender (damage damage))
  (when (deadp defender)
    (kill defender (damage-source damage))))

(defmethod damage :around ((defender sprout-hulk) (damage damage))
  (let ((damage (call-next-method)))
    (when (> damage 0)
      (setf (health (make-sprout (do ((index (random 16) (mod (1+ index) 16))
				      (new-pos nil (vec+ (pos defender)
							 (rotate '(2 . 0) (* 1/8 index pi)))))
				     ((and new-pos (visiblep new-pos (pos defender)))
				      new-pos))))
	    damage))))

(DEFMETHOD VISIBLEP ((POS LIST) (FROM LIST))
  (AND (HAS-LOS POS FROM) (ILLUMINATEDP POS)))

(DEFMETHOD VISIBLEP ((N SYMBOL) FROM) NIL)

(DEFMETHOD VISIBLEP ((C CHARACTER) FROM) T)

(DEFMETHOD VISIBLEP ((POS LIST) (FROM CREATURE))
  (AND (HAS-LOS POS (POS FROM)) (OR (DARKVISIONP FROM) (ILLUMINATEDP POS))))

(DEFMETHOD VISIBLEP ((OBJ ACTOR) FROM)
  (IF (HIDDENP OBJ)
      NIL
      (VISIBLEP (POS OBJ) FROM)))

(defmethod make-hostile ((obj shopkeeper) (to creature))
  (unless (member to (targets obj) :test #'equal)
    (push to (targets obj))))

(defmethod make-hostile ((obj creature) (to creature))
  (setf (enemies obj) (logior (types to) (enemies obj))))

(DEFMETHOD ATTACK :AROUND ((DEFENDER CREATURE) ATTACKER)
  (UNLESS (DEADP DEFENDER) (CALL-NEXT-METHOD)))

(DEFMETHOD ATTACK :AFTER (DEFENDER (ATTACKER creature))
  (MAPC
   (LAMBDA (WEAPON)
     (WHEN (BREAKSP WEAPON)
       (when (playerp attacker)
	 (PRINT-TO-LOG "your ~a breaks!" (NAME WEAPON))
	 (flag-alert))
       (UNEQUIP WEAPON attacker :TO 'THE-ABYSS)))
   (WEAPONS ATTACKER :equipped-only t)))

(defmethod attack :after ((defender creature) (attack attack))
  (when (slot-exists-p (damage-source attack) 'types)
    (make-hostile defender (damage-source attack))))

(DEFMETHOD ATTACK ((DEFENDER BREAKABLE) (ATTACK ATTACK))
  (PRINT-TO-LOG "~a hit ~a for ~d damage~:[~;, ~a~]" (name (damage-source ATTACK))
                (NAME DEFENDER) (DAMAGE DEFENDER attack)
                (DEADP DEFENDER)
		(DEATH DEFENDER)))

(DEFMETHOD ATTACK (DEFENDER (ATTACKER CREATURE))
  (MAPC
   (LAMBDA (WEAPON)
     (WHEN (<= (DISTANCE (POS DEFENDER) (POS ATTACKER)) (RANGE WEAPON))
       (ATTACK DEFENDER (GET-ATTACK WEAPON ATTACKER))))
   (WEAPONS ATTACKER)))

(DEFMETHOD ATTACK ((DEFENDER CREATURE) (ATTACK ATTACK))
  (IF (>= (ATTACK-TO-HIT ATTACK) (EVASION DEFENDER))
      (PRINT-TO-LOG "~:[~;~a hit ~a for ~d damage~:[~;, ~a~]~]"
                    (VISIBLEP DEFENDER *PLAYER*) (name (damage-source ATTACK))
                    (NAME DEFENDER)
                    (DAMAGE DEFENDER attack)
                    (DEADP DEFENDER) (DEATH DEFENDER))
      (PRINT-TO-LOG "~a missed ~a" (name (damage-source ATTACK)) (NAME DEFENDER))))

(DEFMETHOD PICKUP :AFTER ((ITEM EQUIPMENT))
  (REMOVE-NON-SOLID (POS ITEM))
  (WHEN (> (ILLUMINATION ITEM) 0) (REMOVE-GLOWING ITEM))
  (ADD-TO-INVENTORY ITEM)
  (PRINT-TO-LOG "you picked up a ~a" (NAME ITEM)))

(DEFMETHOD KILL ((OBJ ENEMY) killer) (DROP-CORPSE OBJ))

(DEFMETHOD KILL :BEFORE ((OBJ ACTOR) killer)
  (IF (EQUAL (SOLID (POS OBJ)) OBJ)
      (REMOVE-SOLID (POS OBJ))
      (REMOVE-NON-SOLID (POS OBJ)))
  (REMOVE-GLOWING OBJ))

(defmethod kill :after ((obj enemy) (killer player))
  (gain-experience (xp obj)))

(defmethod kill :after ((obj equipment) (killer player))
  (when (shopkeeper obj)
    (let ((debt (make-debt)))
      (print-to-log "you owe the shopkeeper ~d gold for destroying the ~a"
		    (price obj)
		    (name obj))
      (setf (price debt) (price obj))
      (setf (shopkeeper debt) (shopkeeper obj))
      (add-to-inventory debt))))

(DEFMETHOD WALLP ((OBJ LIST)) (WALLP (SOLID OBJ)))

(DEFMETHOD WALLP ((OBJ SYMBOL)) (EQ OBJ 'WALL))

(DEFMETHOD WALLP ((OBJ CHARACTER)) (member obj '(#\- #\| #\O)))

(DEFMETHOD SELL-PRICE ((ITEM EQUIPMENT)) (ASH (PRICE ITEM) -1))

(DEFMETHOD COLOR ((OBJ SYMBOL))
  (LET ((COLOR-SET (ASSOC OBJ *COLORS*)))
    (IF COLOR-SET
        (CADR COLOR-SET)
        0)))

(DEFMETHOD COLOR ((OBJ LIST)) (APPLY #'CALCULATE-COLOR OBJ))

(DEFMETHOD COLOR ((OBJ ACTOR))
  (LET* ((STORED-VALUE (SLOT-VALUE OBJ 'COLOR)) (COLOR (COLOR STORED-VALUE)))
    (UNLESS (NUMBERP STORED-VALUE) (SETF (SLOT-VALUE OBJ 'COLOR) COLOR))
    COLOR))

(DEFMETHOD BG-COLOR ((OBJ FURNITURE))
  (WHEN (SLOT-VALUE OBJ 'BG-COLOR) (COLOR (SLOT-VALUE OBJ 'BG-COLOR))))

(DEFMETHOD BG-COLOR ((POS LIST))
  (IF (NON-SOLID POS)
      (BG-COLOR (NON-SOLID POS))
      (BG-COLOR (TERRAIN POS))))

(defmethod display-char ((obj character) &key &allow-other-keys) obj)

(DEFMETHOD DISPLAY-CHAR ((OBJ ACTOR) &key darken &allow-other-keys)
  (LET ((TEXT
         (IF (EQ (COLOR OBJ) 30)
             (SLOT-VALUE OBJ 'DISPLAY-CHAR)
             (APPLY-COLOR (SLOT-VALUE OBJ 'DISPLAY-CHAR)
			  (if darken
			      (darken (COLOR OBJ) darken)
			      (color obj))))))
    (IF (BG-COLOR OBJ)
        (APPLY-COLOR TEXT (BG-COLOR OBJ) :BG T)
        (IF (BG-COLOR (POS OBJ))
            (APPLY-COLOR TEXT (BG-COLOR (POS OBJ)) :BG T)
            TEXT))))

(DEFMETHOD DISPLAY-CHAR ((POS LIST) &key has-los-p &allow-other-keys)
  (IF (WALLP POS)
      (apply-color (let ((wall-up (wallp (vec+ pos +up+)))
			 (wall-down (wallp (vec+ pos +down+))))
		     (cond ((AND wall-up wall-down)
			    (SETF (SOLID POS) #\|))
			   ((or (wallp (vec+ pos +left+)) (wallp (vec+ pos +right+))
				wall-up wall-down)
			    (SETF (SOLID POS) #\-))
			   (t
			    (setf (solid pos) #\O))))
		   (if has-los-p
		       (darken 255 (darkness pos) 240)
		       240))
      (DISPLAY-CHAR (TERRAIN POS) :darken (darkness pos))))

(DEFMETHOD DISPLAY-CHAR ((OBJ SYMBOL) &key darken &allow-other-keys)
  (APPLY-COLOR (GETHASH OBJ *TERRAIN-CHARACTERS*)
               (if darken
		   (darken (GETHASH OBJ *TERRAIN-COLORS*) darken)
		   (gethash obj *terrain-colors*))))

(DEFMETHOD DEADP ((OBJ CREATURE))
  (= (HEALTH OBJ) 0))

(DEFMETHOD DEADP ((OBJ BREAKABLE))
  (>= (BREAK-CHANCE OBJ) 100))

(defmethod (setf deadp) (value (obj creature))
  (when value
    (setf (health obj) 0)
    (kill obj 'death)))

(DEFMETHOD DEATH ((OBJ CREATURE)) "killing it")

(DEFMETHOD DEATH ((OBJ BREAKABLE)) "destroying it")

(DEFMETHOD WEAPONS ((OBJ CREATURE) &key (equipped-only nil) &allow-other-keys)
  (LET ((HELD-ITEMS (GETHASH 'HAND (EQUIPMENT OBJ)))
	(natural-weapons (natural-weapons obj)))
    (COND ((= (LENGTH HELD-ITEMS) 1) HELD-ITEMS)
	  ((> (LENGTH HELD-ITEMS) 1)
	   (LOOP FOR EQUIPMENT IN HELD-ITEMS
		 WHEN (WEAPONP EQUIPMENT)
		   COLLECT EQUIPMENT))
	  (equipped-only nil)
	  ((not natural-weapons)
	   (list (make-fist)))
	  ((listp (car natural-weapons))
	   natural-weapons)
	  (t
	   (list natural-weapons)))))

(defmethod range ((obj list))
  (or (loop for key in obj
	    with collecting = nil
	    when collecting
	      return key
	    when (eq key :range)
	      do (setf collecting t))
      1))

(DEFMETHOD DESCRIPTION ((OBJ EQUIPMENT))
  (LOG-TO-STRING "takes ~d ~a slots~%deals ~a as a weapon~:[~;~%~:*~a~]"
                 (SIZE OBJ) (EQUIP-SLOT OBJ) (DAMAGE-STRING (ATK OBJ))
                 (SLOT-VALUE OBJ 'DESCRIPTION)))

(DEFMETHOD IDENTIFY ((OBJ SECRET-EQUIPMENT))
  (SETF (IDENTIFIEDP OBJ) T))

(DEFMETHOD MEAT ((OBJ ENEMY))
  (LET ((MEAT (SLOT-VALUE OBJ 'MEAT)))
    (IF MEAT
        (IF (NUMBERP MEAT)
            (MAKE-INSTANCE 'FOOD :SUSTENANCE MEAT :NAME
                           (LOG-TO-STRING "~a meat" (NAME OBJ)))
            MEAT))))

(DEFMETHOD BREAKSP ((OBJ EQUIPMENT) &OPTIONAL (OFFSET 0))
  (< (RANDOM 100)
     (MAX (MIN 1 (BREAK-CHANCE OBJ)) (+ (BREAK-CHANCE OBJ) OFFSET))))

(DEFMETHOD REMOVE-STATUS :AFTER ((STATUS STATUS))
  (SETF (STATUSES (TARGET STATUS)) (REMOVE STATUS (STATUSES (TARGET STATUS)))))

(defmethod get-attack ((weapon equipment) (attacker creature))
  (apply #'generate-attack attacker (atk weapon)))

(defmethod get-attack ((weapon list) (attacker creature))
  (apply #'generate-attack attacker (or (loop for i in weapon
					      when (keywordp i)
						return trimmed-weapon
					      collect i into trimmed-weapon)
					weapon)))

(defmethod alignment ((obj creature))
  (let ((alignment (slot-value obj 'alignment)))
    (cond ((has-status-p obj 'neutral)
	   'n)
	  ((eq alignment 'n)
	   (apply-alignment-status obj))
	  ((has-status-p obj 'evil)
	   'e)
	  ((has-status-p obj 'good)
	   'g)
	  ((eq alignment 'e)
	   'e)
	  ((eq alignment 'g)
	   'g)
	  (t
	   'n))))

(defmethod make-sprout :around (pos)
  (let ((index (random 4)))
    (cond ((= index 3)
	   (make-grenadier-sprout pos))
	  (t
	   (call-next-method)))))

(defmethod hostilep ((obj creature) (to creature))
  (maskp (enemies obj) (types to)))

(defmethod hostilep ((obj shopkeeper) (to creature))
  (member to (targets obj) :test #'equal))

(defmethod alliedp ((obj creature) (to creature))
  (maskp (allies obj) (types to)))

(defmethod make-shopkeeper :around (pos)
  (let ((shopkeeper (call-next-method)))
    (loop for x from (- (car pos) (domain shopkeeper)) to (+ (car pos) (domain shopkeeper))
	  do (loop for y from (- (cdr pos) (domain shopkeeper))
		     to (+ (cdr pos) (domain shopkeeper))
		   when (and (= (random 6) 0)
			     (not (equal (cons x y) pos))
			     (visiblep (cons x y) shopkeeper))
		     do (place-shop-item (cons x y) shopkeeper)))
    (setf (home shopkeeper) (pos shopkeeper))
    shopkeeper))

(defmethod pos ((obj list)) obj)

(defmethod (setf enemies) (value (obj shopkeeper)))

(defmethod reanimate :around ((obj actor) &optional master)
  (unless (solid (pos obj))
    (let ((new-creature (call-next-method)))
      (when new-creature
	(remove-non-solid (pos obj))
	(when master
	  (setf (enemies new-creature) (enemies master)))
	new-creature))))

(defmethod reanimate ((obj bones) &optional master)
  (make-skeleton (pos obj)))

(defmethod reanimate ((obj corpse) &optional master)
  (let ((zombie (make-zombie (pos obj)))
	(weapon (loop for loot in (loot obj)
		      when (weaponp loot)
			return loot)))
    (when weapon
      (equip weapon zombie))
    zombie))

(defmethod cast-spell ((spell spell) (obj creature))
  (if (spell-requires-target-p spell)
      nil
      (funcall (spell-function spell) obj)))

(defmethod zap :around ((obj wand) zapper)
  (if (> (charges obj) 0)
      (call-next-method)
      (when (playerp zapper)
	(print-to-log "there is no effect"))))

(defmethod zap ((obj wand) (zapper creature))
  (when (visiblep zapper *player*)
    (print-to-log "~a zaps a ~a" (name zapper) (name obj)))
  (when (cast-spell (spell obj) zapper)
    (identify obj))
  (decf (charges obj)))
