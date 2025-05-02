(defmethod quaffablep ((obj potion))
  (declare (ignore obj))
  t)

(defmethod drop-corpse ((obj enemy))
  (let ((corpse (make-corpse (pos obj))))
    (setf (name corpse) (log-to-string "~a corpse" (name obj)))
    (setf (loot corpse) (get-loot obj))))

(defmethod kill :after ((obj enemy))
  (gain-experience (xp obj)))

(defmethod kill :after ((obj equipment))
  (when (shopkeeper obj)
    (let ((debt (make-debt)))
      (print-to-log "you owe the shopkeeper ~d gold for destroying the ~a"
		    (price obj)
		    (name obj))
      (setf (price debt) (price obj))
      (setf (shopkeeper debt) (shopkeeper obj))
      (add-to-inventory debt))))

(defmethod remove-from-inventory ((item debt) &key (selling nil) &allow-other-keys)
  (if selling
      (call-next-method)
      (print-to-log "you can't get rid of debt that easily")))

(defmethod hiddenp ((obj ladder))
  (or (and (= *current-depth* 0)
	   (= -1 (direction obj)))
      (and (= *current-depth* (1- (length *layers*)))
	   (= 1 (direction obj)))))

(defmethod name ((obj ladder))
  (cond ((= -1 (direction obj))
	 'ladder-leading-up)
	((= 1 (direction obj))
	 'ladder-leading-down)))

(defmethod apply-to ((subj creature) (obj elevated))
  (incf (dex subj)))

(defmethod attack :after ((defender shopkeeper) (attacker player))
  (setf (enragedp defender) t))

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
  (let ((damage (damage activator (roll 1 6) '(bludgeoning))))
    (when (playerp activator)
      (print-to-log "you triggered a ~a and took ~d damage"
		    (name trap)
		    damage)
      (flag-alert))))

(defmethod move-into ((passive table) (active creature))
  (apply-to active (make-elevated-status)))

(defmethod move-into ((passive trap) (active player))
    (when (< (random 100) (trigger-chance passive))
      (trigger passive active)))

(defmethod move-into ((passive shopkeeper) (active player))
  (if (enragedp passive)
      (attack passive active)
      (let ((action (get-item-from-list '(attack sell buy) :what 'option)))
	(cond ((eq action 'attack)
	       (attack passive active))
	      ((eq action 'sell)
	       (sell-item passive))
	      ((eq action 'buy)
	       (checkout))))))

(defmethod evasion ((obj creature))
  (max 1 (+ 5 (dex obj) (slot-value obj 'evasion))))

(defmethod name ((obj character)) "wall")
(defmethod name ((obj symbol)) obj)
(defmethod name ((obj secret-equipment))
  (if (identifiedp obj)
      (slot-value obj 'name)
      (cover-name obj)))

(defmethod (setf health) (value (obj creature))
  (if (> value 0)
      (if (slot-boundp obj 'max-health)
	  (setf (slot-value obj 'health) (min value (max-health obj)))
	  (progn (setf (slot-value obj 'max-health) (health obj))
		 (setf (slot-value obj 'health) value)))
      (progn (setf (slot-value obj 'health) 0)
	     (kill obj))))

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

(defmethod act ((obj elevated))
  (let ((f (non-solid (pos (target obj)))))
    (unless (and f (eq (name f) 'table))
      (remove-status obj))))

(defmethod act ((obj resting))
  (incf (hunger (target obj)))
  (incf (health (target obj))))

(defmethod act ((obj shopkeeper))
  (when (enragedp obj)
    (call-next-method)))

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

(defmethod throw-at :after ((target actor) (item equipment) thrower)
  (place item (pos target) :solid nil))

(defmethod throw-at :after ((target list) (item equipment) thrower)
  (place item target :solid nil))

(defmethod update ((obj status))
    (incf (energy obj) (/ (spd obj) (spd *player*)))
    (act obj))

(defmethod update :before ((obj creature))
  (mapc #'update (statuses obj))
  (incf (idle-time obj)))

(defmethod update ((obj player))
  (decf (hunger obj) (/ 1 (spd *player*))))

(defmethod update ((obj enemy))
  (incf (energy obj) (/ (spd obj) (spd *player*)))
  (act obj))

(defmethod act :around ((obj status))
    (if (= (duration obj) 0) ; = 0 so that a status with negative duration is permanent
	(remove-status obj)
	(when (>= (energy obj) 1)
	  (decf (energy obj))
	  (when (next-method-p)
	    (call-next-method))
	  (decf (duration obj))
	  (act obj))))

(defmethod act :around ((obj enemy))
  (let ((closest-hostile (get-hostile-in-los-of obj)))
    (when closest-hostile
      (setf (target-pos obj) (pos closest-hostile))))
  (loop while (>= (energy obj) 1)
	do (decf (energy obj)
		 (let ((result (cond ((target-pos obj)
				      (call-next-method))
				     ((idle-behavior obj)
				      (funcall (idle-behavior obj) obj)))))
		   (if (numberp result)
		       result
		       1)))))

(defmethod act ((obj enemy))
  (let* ((primary (car (weapons obj)))
	 (bravep (has-status-p obj 'brave))
	 (afraidp (has-status-p obj 'frightened))
	 (path (find-path (pos obj) (target-pos obj))))
    (cond ((and (<= (health obj) (/ (max-health obj) 2))
		(not afraidp)
		(not bravep))
	   (let ((morale-roll (roll 3 6)))
	     (if (>= (morale obj) morale-roll)
		 (apply-to obj (make-brave-status :duration morale-roll))
		 (apply-to obj (make-frightened-status :duration morale-roll)))))
	  ((and (can-flee obj path)
		(or (target-too-close-p obj (range primary))
		    afraidp))
	   (flee obj path))
	  ((<= (distance (pos obj) (target-pos obj))
	       (range primary))
	   (let ((target (solid (target-pos obj))))
	     (when (and target
			(hostilep target obj))
	       (attack target obj))))
	  ((not afraidp)
	   (step-on-path path obj)))))

(defmethod act :after ((obj enemy))
  (if (equal (pos obj) (target-pos obj))
      (setf (target-pos obj) nil)))

(defmethod move :around ((obj player) direction)
  (if *shopkeeper*
      (let ((new-pos (vec+ (pos obj) direction)))
	(if (and (not (solid new-pos))
		 (or (> (distance new-pos (pos *shopkeeper*))
			(domain *shopkeeper*))
		     (not (visiblep (pos *shopkeeper*) new-pos))))
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
      (remove-solid (pos obj))
      (setf (solid new-pos) obj)
      (setf (pos obj) new-pos))
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
  (when (if (hostilep passive active)
	    t
	    (confirmp "that creature does not appear hostile. Do you want to attack it?"))
    (attack passive active)))

(defmethod move-into ((passive creature) (active creature))
  (when (hostilep passive active)
    (attack passive active)))

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
  (OR (GETHASH TERRAIN *TERRAIN-COSTS*) 1))

(DEFMETHOD MOVEMENT-COST ((CREATURE CREATURE))
  (IDLE-TIME CREATURE))

(DEFMETHOD APPLY-TO :BEFORE ((SUBJ CREATURE) (OBJ STATUS))
  (SETF (TARGET OBJ) SUBJ)
  (PUSH OBJ (STATUSES SUBJ)))

(DEFMETHOD DAMAGE ((DEFENDER CREATURE) AMOUNT TYPES &OPTIONAL STATUSES)
  (LET* ((BASE-DAMAGE (MAX 1 (- AMOUNT (ARMOR DEFENDER))))
         (MOD-DAMAGE (ROUND (* BASE-DAMAGE (DAMAGE-MODIFIER DEFENDER TYPES))))
         (REAL-DAMAGE
          (COND ((= MOD-DAMAGE 0) 0) ((< MOD-DAMAGE 0) MOD-DAMAGE)
                (T (MAX 1 MOD-DAMAGE)))))
    (DECF (HEALTH DEFENDER) REAL-DAMAGE)
    (LOOP FOR STATUS IN STATUSES
          DO (APPLY-TO DEFENDER STATUS))
    (MAX 0 REAL-DAMAGE)))

(DEFMETHOD DAMAGE ((DEFENDER BREAKABLE) AMOUNT TYPES &OPTIONAL STATUSES)
  (DECLARE (IGNORE TYPES STATUSES))
  (INCF (BREAK-CHANCE DEFENDER) (* 5 AMOUNT))
  AMOUNT)

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

(DEFMETHOD ATTACK (DEFENDER ATTACKER))

(DEFMETHOD ATTACK :AFTER ((DEFENDER CREATURE) (ATTACKER PLAYER))
  (UNLESS (HOSTILEP DEFENDER ATTACKER) (APPLY-TO DEFENDER (MAKE-EVIL-STATUS))))

(DEFMETHOD ATTACK :AROUND ((DEFENDER CREATURE) ATTACKER)
  (UNLESS (DEADP DEFENDER) (CALL-NEXT-METHOD)))

(DEFMETHOD ATTACK :AFTER (DEFENDER (ATTACKER PLAYER))
  (MAPC
   (LAMBDA (WEAPON)
     (WHEN (BREAKSP WEAPON)
       (PRINT-TO-LOG "your ~a breaks!" (NAME WEAPON))
       (flag-alert)
       (UNEQUIP WEAPON *PLAYER* :TO 'THE-ABYSS)))
   (WEAPONS ATTACKER)))

(DEFMETHOD ATTACK ((DEFENDER BREAKABLE) (ATTACK ATTACK))
  (PRINT-TO-LOG "~a hit ~a for ~d damage~:[~;, ~a~]" (ATTACK-SOURCE ATTACK)
                (NAME DEFENDER) (DAMAGE DEFENDER (ATTACK-DMG ATTACK) NIL)
                (WHEN (DEADP DEFENDER) (KILL DEFENDER) T) (DEATH DEFENDER)))

(DEFMETHOD ATTACK (DEFENDER (ATTACKER CREATURE))
  (MAPC
   (LAMBDA (WEAPON)
     (WHEN (<= (DISTANCE (POS DEFENDER) (POS ATTACKER)) (RANGE WEAPON))
       (ATTACK DEFENDER (GET-ATTACK WEAPON ATTACKER))))
   (WEAPONS ATTACKER)))

(DEFMETHOD ATTACK ((DEFENDER CREATURE) (ATTACK ATTACK))
  (IF (>= (ATTACK-TO-HIT ATTACK) (EVASION DEFENDER))
      (PRINT-TO-LOG "~:[~;~a hit ~a for ~d damage~:[~;, ~a~]~]"
                    (VISIBLEP DEFENDER *PLAYER*) (ATTACK-SOURCE ATTACK)
                    (NAME DEFENDER)
                    (DAMAGE DEFENDER (ATTACK-DMG ATTACK) (ATTACK-TYPES ATTACK)
                            (ATTACK-STATUSES ATTACK))
                    (DEADP DEFENDER) (DEATH DEFENDER))
      (PRINT-TO-LOG "~a missed ~a" (ATTACK-SOURCE ATTACK) (NAME DEFENDER))))

(DEFMETHOD PICKUP :AFTER ((ITEM EQUIPMENT))
  (REMOVE-NON-SOLID (POS ITEM))
  (WHEN (> (ILLUMINATION ITEM) 0) (REMOVE-GLOWING ITEM))
  (ADD-TO-INVENTORY ITEM)
  (PRINT-TO-LOG "you picked up a ~a" (NAME ITEM)))

(DEFMETHOD KILL (OBJ))

(DEFMETHOD KILL ((OBJ ENEMY)) (DROP-CORPSE OBJ))

(DEFMETHOD KILL :BEFORE ((OBJ ACTOR))
  (IF (EQUAL (SOLID (POS OBJ)) OBJ)
      (REMOVE-SOLID (POS OBJ))
      (REMOVE-NON-SOLID (POS OBJ)))
  (REMOVE-GLOWING OBJ))

(DEFMETHOD WALLP ((OBJ LIST)) (WALLP (SOLID OBJ)))

(DEFMETHOD WALLP ((OBJ SYMBOL)) (EQ OBJ 'WALL))

(DEFMETHOD WALLP ((OBJ CHARACTER)) (OR (EQ OBJ #\|) (EQ OBJ #\-)))

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

(DEFMETHOD DISPLAY-CHAR ((POS LIST) &key &allow-other-keys)
  (IF (WALLP POS)
      (IF (AND (WALLP (VEC+ POS +UP+)) (WALLP (VEC+ POS +DOWN+)))
          (SETF (SOLID POS) #\|)
          (SETF (SOLID POS) #\-))
      (DISPLAY-CHAR (TERRAIN POS) :darken (darkness pos))))

(DEFMETHOD DISPLAY-CHAR ((OBJ SYMBOL) &key darken &allow-other-keys)
  (APPLY-COLOR (GETHASH OBJ *TERRAIN-CHARACTERS*)
               (if darken
		   (darken (GETHASH OBJ *TERRAIN-COLORS*) darken)
		   (gethash obj *terrain-colors*))))

(DEFMETHOD DEADP ((OBJ CREATURE)) (= (HEALTH OBJ) 0))

(DEFMETHOD DEADP ((OBJ BREAKABLE)) (>= (BREAK-CHANCE OBJ) 100))

(DEFMETHOD DEATH ((OBJ CREATURE)) "killing it")

(DEFMETHOD DEATH ((OBJ BREAKABLE)) "destroying it")

(DEFMETHOD WEAPONS ((OBJ CREATURE))
  (LET ((HELD-ITEMS (GETHASH 'HAND (EQUIPMENT OBJ))))
    (COND ((= (LENGTH HELD-ITEMS) 1) HELD-ITEMS)
          ((= (LENGTH HELD-ITEMS) 0) (LIST (MAKE-FIST)))
          (T
           (LOOP FOR EQUIPMENT IN HELD-ITEMS
                 WHEN (WEAPONP EQUIPMENT)
                 COLLECT EQUIPMENT)))))

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
     (MAX (MIN 1 (BREAK-CHANCE OBJ)) (- (BREAK-CHANCE OBJ) OFFSET))))

(DEFMETHOD REMOVE-STATUS :AFTER ((STATUS STATUS))
  (SETF (STATUSES (TARGET STATUS)) (REMOVE STATUS (STATUSES (TARGET STATUS)))))

(defmethod get-attack ((weapon equipment) (attacker creature))
  (apply #'generate-attack attacker (atk weapon)))

(defmethod get-attack ((weapon list) (attacker creature))
  (apply #'generate-attack attacker weapon))


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

(defmethod hostilep ((obj creature) (to creature))
  (or (and (evilp obj) (goodp to))
      (and (evilp to) (goodp obj))))

(defmethod hostilep ((obj shopkeeper) (to player))
  (enragedp obj))

(defmethod hostilep ((obj shopkeeper) (to creature))
  nil)

(defmethod hostilep ((obj creature) (to shopkeeper))
  nil)

(defmethod make-shopkeeper :around (pos)
  (let ((shopkeeper (call-next-method)))
    (loop for x from (- (car pos) (domain shopkeeper)) to (+ (car pos) (domain shopkeeper))
	  do (loop for y from (- (cdr pos) (domain shopkeeper))
		     to (+ (cdr pos) (domain shopkeeper))
		   when (and (= (random 6) 0)
			     (not (equal (cons x y) pos))
			     (visiblep (cons x y) shopkeeper))
		     do (place-shop-item (cons x y) shopkeeper)))
    shopkeeper))

(macrolet ((define-damage-mod-accessors (&rest names)
	     `(progn
		,@(loop for name in names
			collect `(defmethod (setf ,name) ((value number) (obj creature))
				   (setf (slot-value obj ',name) value))
			collect `(defmethod (setf ,name) ((value list) (obj creature))
				   (setf (slot-value obj ',name) (make-mask value)))
			collect `(defmethod ,name ((obj creature))
				   (let ((damage-types (slot-value obj ',name)))
				     (if (numberp damage-types)
					 damage-types
					 (let ((mask (make-mask damage-types)))
					   (setf (,name obj) mask)
					   mask))))))))
  (define-damage-mod-accessors resistances immunities absorbances vulnerabilities))

(defmethod playerp ((obj player)) t)
