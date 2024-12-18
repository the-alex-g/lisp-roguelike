(load "./colors.lisp")
(load "./bsp-dungeon.lisp")
(load "./class-definitions.lisp")

(load "~/quicklisp/setup.lisp")
(ql:quickload :trivial-raw-io)

(defparameter *layers* '())
(defparameter *current-layer* nil)
(defparameter *layer-index* 0)
(defparameter *player-actions* 0)
(defparameter *sight-distance* 6)
(defconstant +left+ '(-1 . 0))
(defconstant +right+ '(1 . 0))
(defconstant +up+ '(0 . -1))
(defconstant +down+ '(0 . 1))
(defconstant +zero+ '(0 . 0))
(defconstant +directions+ '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
(defconstant +directions-with-zero+
  '((-1 . 0) (1 . 0) (0 . -1) (0 . 1) (0 . 0)))
(defparameter *actions* (make-hash-table :test 'equal))
(defparameter *action-descriptions* (make-hash-table))
(defparameter *inventory* '())
(defparameter *inventory-size* 9)
(defparameter *light-zone* '())
(defparameter *show-found-spaces* nil)
(defparameter *level-up-pending* nil)
(defparameter *statuses* '())
(defparameter *monster-spawn-table* (make-hash-table))
(defparameter *treasure-spawn-table* (make-hash-table))
(defparameter *trap-spawn-table* (make-hash-table))
(defparameter *in-terminal* (handler-case (sb-posix:tcgetattr 0)
			      (error () nil)))

(defun layers (&rest controls)
  (labels ((get-segments (list operator arg sequences)
	     (if list
		 (cond ((eq operator 'above)
			(get-segments (cdr list) nil nil
				      (cons (format nil "1-~d"
						    (1- (car list)))
					    sequences)))
		       ((eq operator 'below)
			(get-segments (cdr list) nil nil
				      (cons (format nil "~d+"
						    (1+ (car list)))
					    sequences)))
			 ((eq operator 'from)
			  (get-segments (cdr list) nil (car list) sequences))
			 ((eq operator 'to)
			  (get-segments (cdr list) nil nil
					(cons (format nil "~d-~d"
						      arg
						      (car list))
					      sequences)))
			 ((and (eq operator 'on) (numberp (car list)))
			  (get-segments (cdr list) 'on nil
					(cons (car list) sequences)))
			 ((and (eq operator 'excluding) (numberp (car list)))
			  (get-segments (cdr list) 'excluding nil
					(cons (format nil "~d!" (car list))
					      sequences)))
			 (t
			  (get-segments (cdr list) (car list) arg sequences)))
		 sequences)))
    (let ((sequences (get-segments controls nil nil nil)))
      (if (> (length sequences) 1)
	  (format nil "~a~{,~a~}" (car sequences) (cdr sequences))
	  (format nil "~a" (car sequences))))))

(defun custom-read-char ()
  (force-output)
  (if *in-terminal*
      (trivial-raw-io:read-char)
      (read-char)))

(defun distribute-list (list)
  (let ((step (max 1 (floor (/ 100 (length list)))))
	(d100 100))
    (mapcar (lambda (i)
	      (decf d100 step)
	      (if (>= d100 step)
		  (list step i)
		  (list (+ step d100) i)))
	    list)))

(defun spawn-list (depth)
  (labels ((get-sections (a b c)
	     (if a
		 (if (eq (car a) #\,)
		     (if (and (= (length b) 1) (digit-char-p (car b)))
			 (get-sections (cdr a) nil (cons (digit-char-p (car b))
							 c))
			 (get-sections (cdr a) nil (cons (reverse b) c)))
		     (get-sections (cdr a) (cons (car a) b) c))
		 (cons (reverse b) c)))
	   (split (list)
	     (get-sections list nil nil))
	   (in-depth-p (value)
	     (let ((layer-list
		     (mapcar (lambda (x)
			       (if (= 1 (length x))
				   (= depth (digit-char-p (car x)))
				   (loop for c in x
					 with last = 0
					 with modifier = #\space
					 when (digit-char-p c)
					   if (eq modifier #\-)
					     return (<= last depth
							(digit-char-p c))
					 else
					   do (setf last (digit-char-p c))
					 when (eq c #\+)
					   return (>= depth last)
					 when (and (eq c #\!) (= depth last))
					   return 'abort
					 when (eq c #\-)
					   do (setf modifier #\-))))
			     (split (coerce value 'list)))))
	       (and (loop for x in layer-list thereis x)
		    (loop for x in layer-list never (eq x 'abort)))))
	   (prune-for-depth (list)
	     (loop for i in list
		   when (in-depth-p (car i))
		     collect (cdr i)))
	   (get-list-from-table (table)
	     (let ((rare (prune-for-depth (gethash 'rare table)))
		   (common (prune-for-depth (gethash 'common table)))
		   (uncommon (prune-for-depth (gethash 'uncommon table)))
		   (legendary (prune-for-depth (gethash 'legendary table)))
		   (amount 10)
		   (total 0))
	       (loop for sub-list in (list legendary rare uncommon common)
		     when sub-list
		       if (<= amount 30)
			 collect (cons amount (distribute-list	sub-list))
			 and do (incf total amount)
		     else collect (cons (- 100 total)
					(distribute-list sub-list))
		     do (incf amount 10)))))
    (list (get-list-from-table *treasure-spawn-table*)
	  (get-list-from-table *monster-spawn-table*)
	  (get-list-from-table *trap-spawn-table*))))

(defun add-to-spawn (list rarity depths function &rest more-functions)
  (let ((flist (cons depths (if more-functions
				(distribute-list (cons function more-functions))
				function))))
    (cond ((eq list 'monster)
	   (push flist (gethash rarity *monster-spawn-table*)))
	  ((eq list 'treasure)
	   (push flist (gethash rarity *treasure-spawn-table*)))
	  ((eq list 'trap)
	   (push flist (gethash rarity *trap-spawn-table*))))))

(defmacro defaction (key description &body body)
  `(progn (setf (gethash ,key *action-descriptions*) ,description)
	  (setf (gethash ,key *actions*) (lambda ()
					   ,@body
					   (incf *player-actions*) t))))

(defmethod name ((obj equipment))
  (if (and (or (secretp obj)
	       (not (identifiedp obj)))
	   (slot-boundp obj 'fake-name))
      (fake-name obj)
      (slot-value obj 'name)))

(defmethod description ((obj equipment))
  (if (slot-boundp obj 'description)
      (slot-value obj 'description)
      (log-to-string "a ~a" (name obj))))

(defgeneric get-ascii (obj)
  (:method ((obj actor))
    (if *in-terminal*
	(apply-color (display-char obj) (color obj))
	(display-char obj)))
  (:method ((obj equipment))
    (if *in-terminal*
	(apply-color (display-char obj) (color obj))
	(display-char obj)))
  (:method ((obj pickup))
    (get-ascii (equipment obj))))

(defgeneric death-verb (obj)
  (:method ((obj actor))
    "destroying")
  (:method ((obj combat-entity))
    "killing"))

(defmethod name ((obj pickup))
  (name (equipment obj)))

(defmethod description ((obj pickup))
  (description (equipment obj)))

;; generate setters and getters for combat-entity stats
(mapc (lambda (name)
	(eval `(progn
		 (defmethod ,name ((obj combat-entity))
		   (+ (slot-value obj (quote ,name)) 
		      (loop for eq-slot being the hash-keys of (equips obj)
			    sum (let ((qpmnt (gethash eq-slot (equips obj))))
				  (if (weaponp qpmnt)
				      (if (eq eq-slot 'hand)
					  (slot-value qpmnt (quote ,name))
					  0)
				      (if (eq eq-slot 'hand)
					  0
					  (slot-value qpmnt (quote ,name))))))))
		 (defmethod (setf ,name) (new-val (obj combat-entity))
		   (setf (slot-value obj (quote ,name)) new-val)))))
      '(def str health dex)) ; this is the list of stats

(defmethod (setf xp) (value (obj player))
  (setf (slot-value obj 'xp) value)
  (setf *level-up-pending* (>= value (xp-bound obj))))

(defmethod (setf heal-clock) (value (obj player))
  (if (= value 0)
      (progn (setf (slot-value obj 'heal-clock) 10)
	     (unless (starvingp obj)
	       (incf (health obj))))
      (setf (slot-value obj 'heal-clock) value)))

(defmethod (setf health) (value (obj player))
  (setf (slot-value obj 'health) (max 0 (min value (max-health obj)))))

(defparameter *player* (make-instance 'player :name 'player
					      :health 10))

(defun dynamic-actors ()
  (slot-value *current-layer* 'dynamic-actors))

(defun static-actors ()
  (slot-value *current-layer* 'actors))

(defun (setf static-actors) (value)
  (setf (slot-value *current-layer* 'actors) value))

(defun (setf dynamic-actors) (value)
  (setf (slot-value *current-layer* 'dynamic-actors) value))

(defun actors ()
  (append (slot-value *current-layer* 'actors)
	  (dynamic-actors)
	  (list *player*)))

(defun board ()
  (slot-value *current-layer* 'board))

(defun board-size ()
  (slot-value *current-layer* 'board-size))

(let ((last-modified -1)
      (current-walls '()))
  (defun walls ()
    (unless (= last-modified *player-actions*)
      (setf last-modified *player-actions*)
      (setf current-walls (loop for actor in (actors)
				when (wallp actor)
				  collect (pos actor))))
    current-walls))

(defmacro define-damage-modifier-getter (name slot-name)
  (let ((listing-body `(if (listp (slot-value obj (quote ,slot-name)))
			   (slot-value obj (quote ,slot-name))
			   (list (slot-value obj (quote ,slot-name))))))
  `(defgeneric ,name (obj)
     (:method ((obj equipment))
       ,listing-body)
     (:method ((obj actor))
       ,listing-body)
     (:method ((obj combat-entity))
       (append ,listing-body
	       (append (loop for item being the hash-values of (equips obj)
			     unless (and (eq (equip-slot item) 'hand)
					 (not (weaponp item)))
			       collect (,name item))))))))

(define-damage-modifier-getter resistances resist)
(define-damage-modifier-getter vulnerabilities vulnerable)
(define-damage-modifier-getter immunities immune)

(defgeneric make-pickup (equipment pos)
  (:method ((equipment equipment) pos)
    (let ((pickup (make-instance 'pickup :equipment equipment :pos pos
					 :interact-action-only t)))
      (push pickup (static-actors))
      pickup)))

;; initialize helper functions for macros
(labels ((constructor (name)
	   (read-from-string (concatenate 'string "make-"
					  (symbol-name name))))
	 (build-slot (slt) ; creates slot information for new slots
	     (list (if (listp slt)
		       `(,(car slt) :accessor ,(car slt)
				    :initform ,(cadr slt)
				    :initarg ,(intern (symbol-name (car slt))
						      "KEYWORD"))
		       `(,slt :accessor ,slt
			      :initform nil
			      :initarg ,(intern (symbol-name slt) "KEYWORD")))))
	   (reinit-slots (args slotlist &key (slotname nil slotnamep))
	     (if (> (length args) 0) ; cannot check for (car arg) because it might be nil
		 (if slotnamep
		     (reinit-slots (cdr args)
				   (cons (list (read-from-string slotname)
					       :initform (car args))
					 slotlist))
		     (reinit-slots (cdr args)
				   slotlist
				   :slotname (symbol-name (car args))))
		 slotlist)))
  ;; define new monster class and matching constructor function
  (defmacro defenemy (name display-char new-slots
		      &rest keys
		      &key
			(inherit 'enemy)
			equips
		      &allow-other-keys)
    ;; remove :inherit from key list to prevent odd behavior
    (setf keys (loop for k in keys
		     with skip = nil
		     if skip
		       do (setf skip nil)
		     else if (or (eq k :inherit)
				 (eq k :equips))
			    do (setf skip t)
		     else
		       collect k))
    `(progn
       ;; declare new monster class, including new keys and setting initform of
       ;; old values
       (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil)))
       ;; define constructor for new class
       (defun ,(constructor name) (pos)
	 (let ((new-enemy (make-instance (quote ,name)
					 :pos pos
					 :display-char ,display-char
					 :name (quote ,name))))
	   (push new-enemy (dynamic-actors))
	   (when ,equips
	     (mapc (lambda (i)
		     (equip (funcall i) new-enemy))
		   (if (atom ,equips)
		       (list ,equips)
		       ,equips)))
	   new-enemy))))

  ;; define class and constructor function for actor
  (defmacro defactor (name display-char new-slots
		      &rest keys
		      &key (inherit 'actor inheritp)
		      &allow-other-keys)
    (when inheritp
      (setf keys (remove inherit (remove :inherit keys))))
    `(progn (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					     ,@(reinit-slots keys nil)
					     (name :initform (quote ,name))
					     (display-char :initform ,display-char)))
	    (defun ,(constructor name) (pos)
	      (let ((new-actor (make-instance (quote ,name) :pos pos)))
		,(unless (member :interact-action-only keys)
		   '(setf (interact-action-only new-actor) (not (solid new-actor))))
		(if (dynamicp new-actor)
		    (push new-actor (dynamic-actors))
		    (push new-actor (static-actors)))
		new-actor))))
  
  ;; define class, constructor function, and pickup generator function for equipment
  (defmacro defequipment (name new-slots
			  &rest keys
			  &key (inherit 'equipment inheritp)
			    (identifiedp t identifiedp-p)
			  &allow-other-keys)
    (when inheritp
      (setf keys (remove inherit (remove :inherit keys))))
    (when identifiedp-p
      (setf keys (loop for k in keys
		       with skip = nil
		       unless (or skip (eq k :identifiedp))
			 collect k
		       when skip
			 do (setf skip nil)
		       when (eq k :identifiedp)
			 do (setf skip t))))
    `(progn
       ;; define equipment class
       (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil)
					(identifiedp :initform ,identifiedp
						      :allocation :class)
					(name :initform (quote ,name))))
       ;; define constructor function
       (defun ,(constructor name) (&rest keys &key &allow-other-keys)
	 (apply #'make-instance (quote ,name) keys))
       ;; define pickup constructor function
       (defun ,(read-from-string (concatenate 'string "make-"
					      (symbol-name name) "-pickup"))
	   (pos)
	 (make-pickup (,(constructor name)) pos)))))

(defmacro make-status (duration &key
				  (on-update nil on-update-p)
				  (on-applied nil on-applied-p))
  `(let* ((status (make-instance 'status :duration ,duration)))
     (when ,on-update-p
       (setf (slot-value status 'on-update) (lambda (target)
					      (when target ,on-update))))
     (when ,on-applied-p
       (setf (slot-value status 'on-applied) (lambda (target)
					       (when target ,on-applied))))
     status))

(defactor ladder #\# (direction) :destructible nil :solid nil)
;;; corpse, decay time from 60 to 100, with average of 80
(defactor corpse #\c (corpse-type (loot nil) (decay-time (+ 60 (random 21) (random 21))))
  :solid nil :dynamicp t :consumable t)
(defactor bones #\x (bone-type) :solid nil)
(defactor secret-door #\S () :solid nil :health 10 :wallp t :persistent-visiblity-p t
  :interact-action-only nil)
(defequipment weapon (statuses onetime-effects) :weaponp t)

(defun get-loot (obj)
  (append (loop for fxn in (eval-weighted-list (loot obj))
		when fxn
		  collect (funcall fxn))
	  (loop for q being the hash-values of (equips obj)
		collect q)))

(let ((old-make-corpse #'make-corpse))
  (defun make-default-corpse (obj)
    (let ((corpse (funcall old-make-corpse (pos obj))))
      (setf (corpse-type corpse) (name obj))
      (setf (loot corpse) (get-loot obj))
      corpse)))
(fmakunbound 'make-corpse)

(defgeneric make-corpse (obj)
  (:method ((obj enemy))
    (make-default-corpse obj)))

(defun has-legal-destination (obj)
  (and (>= (+ *layer-index* (direction obj)) 0)
       (< (+ *layer-index* (direction obj)) (length *layers*))))

(defmethod description ((obj ladder))
  (when (has-legal-destination obj)
    (if (= (direction obj) -1)
	"a ladder leading back up"
	"a ladder leading down into the darkness")))

(defmethod description ((obj corpse))
  (log-to-string "a ~a corpse" (corpse-type obj)))

(defmethod name ((obj corpse))
  (log-to-string "~a corpse" (corpse-type obj)))

(defmethod description ((obj bones))
  (log-to-string "~a bones" (bone-type obj)))

(defmethod name ((obj bones))
  (log-to-string "~a bones" (bone-type obj)))

(defmethod hiddenp ((obj ladder))
  (not (has-legal-destination obj)))

(defun make-layer (dungeon)
  (let* ((dungeon-board (slot-value dungeon 'board))
	 (down-ladder-pos (car dungeon-board))
	 (up-ladder-pos (randnth (cdr dungeon-board)))
	 (layer (make-instance
		 'layer
		 :up-ladder-pos up-ladder-pos
		 :down-ladder-pos down-ladder-pos
		 :board (loop for pos in dungeon-board
			      with table = (make-hash-table :test #'equal)
			      do (setf (gethash pos table) 'hidden)
			      finally (return table))
		 :board-size (loop for pos in dungeon-board
				   maximize (car pos) into x
				   maximize (cdr pos) into y
				   finally (return (cons x y))))))
    (setf *current-layer* layer)
    (setf (pos *player*) up-ladder-pos)
    (setf (direction (make-ladder down-ladder-pos)) 1)
    (setf (direction (make-ladder up-ladder-pos)) -1)
    (loop for pos being the hash-keys of (slot-value dungeon 'actors)
	  unless (or (equal pos up-ladder-pos) (equal pos down-ladder-pos))
	    do (let ((fxn (gethash pos (slot-value dungeon 'actors))))
		 (when fxn
		   (funcall fxn pos))))
    ;; Create secret doors
    (flet ((board-member (&rest points)
	     (member (apply #'add-pos points) dungeon-board :test #'equal)))
      (loop for pos in dungeon-board
	    when (and (= 0 (random 8))
		      ;; check for chokepoints
		      (loop for set in (list (list +down+ +left+ +right+)
					     (list +up+ +left+ +right+)
					     (list +right+ +up+ +down+)
					     (list +left+ +up+ +down+))
			      thereis (and (board-member pos (car set))
					   (board-member pos (car set) (cadr set))
					   (board-member pos (car set) (caddr set))
					   (not (board-member pos (cadr set)))
					   (not (board-member pos (caddr set))))))
	      ;; make a secret door with the correct orientation
	      do (setf (display-char (make-secret-door pos))
		       (if (or (board-member pos +left+)
			       (board-member pos +right+))
			   #\|
			   #\-))))
    (push layer *layers*)
    layer))

(defun names-equal-p (a b)
  (equal (log-to-string "~a" (name a))
	 (log-to-string "~a" (name b))))

(defun short-inventory ()
  (loop for item in *inventory*
	with used-names = nil
	with string-name = ""
	do (setf string-name (log-to-string "~a" (name item)))
	unless (member string-name used-names :test #'equal)
	  collect (progn (push string-name used-names)
			 item)))

(defun inventory-length ()
  (length (short-inventory)))

(defun remove-from-inventory (item)
  (setf *inventory*
	(remove item *inventory* :test (lambda (a b)
					 (names-equal-p a b))
				 :count 1)))

(defun in-inventory-p (item)
  (loop for i in *inventory*
	  thereis (names-equal-p item i)))

(defun num-in-inventory (item)
  (loop for i in *inventory*
	count (names-equal-p i item)))

(defun add-to-inventory (item)
  (if (or (< (inventory-length) *inventory-size*) (in-inventory-p item))
      (setf *inventory* (append *inventory* (list item)))
      (progn (print-to-log "your inventory is full")
	     (make-pickup item (pos *player*))
	     nil)))

(defgeneric identify (obj)
  (:method ((obj equipment))
    (setf (identifiedp obj) t)))

(defgeneric destroy (obj)
  (:method (obj)
    (print-to-log "~a destroyed~&" obj))
  (:method ((obj actor))
    (if (dynamicp obj)
	(setf (dynamic-actors) (remove obj (dynamic-actors) :test 'equal))
	(setf (static-actors) (remove obj (static-actors) :test 'equal))))
  (:method ((obj enemy))
    (setf (dynamic-actors) (remove obj (dynamic-actors) :test 'equal))
    (incf (xp *player*) (xp obj))
    (make-corpse obj))
  (:method ((obj equipment))
    (remove-from-inventory obj))
  (:method ((obj status))
    (setf *statuses* (remove obj *statuses* :test #'equal))))

(defgeneric on-applied (obj)
  (:method ((obj status))
    (when (slot-boundp obj 'on-applied)
      (funcall (slot-value obj 'on-applied) (target obj)))))

(defgeneric on-update (obj)
  (:method ((obj status))
    (when (slot-boundp obj 'on-update)
      (funcall (slot-value obj 'on-update) (target obj)))))

(defgeneric equip (item obj)
  (:method ((item equipment) (obj combat-entity))
    (let ((old-item (gethash (equip-slot item) (equips obj))))
      (setf (gethash (equip-slot item) (equips obj)) item)
      old-item))
  (:method :around ((item equipment) (obj combat-entity))
    (if (eq (equip-slot item) 'none)
	(progn (print-to-log "That cannot be equipped")
	       'failed)
	(when (next-method-p)
	  (call-next-method)))))

(defgeneric eat (item target)
  (:method :before ((item equipment) target)
    (when (secretp item)
      (setf (secretp item) nil)))
  (:method :around ((item equipment) target)
    (if (consumable item)
	(progn (destroy item) ; destroy first to ensure it is removed correctly
	       (call-next-method))
	(print-to-log "That cannot be eaten")))
  (:method (item target)
    (print-to-log "That cannot be eaten")))

(defgeneric apply-to (item target)
  (:method :around (item (target pickup))
    (apply-to item (equipment target)))
  (:method (item target)
    (print-to-log "You can't apply ~a to ~a" item target))
  (:method ((item status) (target actor))
    (setf (target item) target)
    (on-applied item)
    (when (slot-boundp item 'on-update)
      (push item *statuses*))))

(defgeneric deadp (obj)
  (:method ((obj actor))
    (and (destructible obj)
	 (<= (health obj) 0))))

(defgeneric resolve-damage-modifiers (amount types actor)
  (:method (amount (types list) (actor actor))
    (flet ((resolve-damage-modifier (lst amt)
	     (loop for x in lst
		   when (member x types)
		     return amt)))
      (or (resolve-damage-modifier (vulnerabilities actor) (* 2 amount))
	  (resolve-damage-modifier (resistances actor) (max 1 (ash amount -1)))
	  (resolve-damage-modifier (immunities actor) 0)
	  amount)))
  (:method (amount types (actor actor))
    (resolve-damage-modifiers amount (list types) actor)))

(defgeneric damage (target amount &key unblockable damage-types)
  (:method :around ((target actor) amount &key unblockable damage-types)
    (call-next-method target
		      (resolve-damage-modifiers (max 1 amount)
						damage-types
						target)
		      :unblockable unblockable
		      :damage-types damage-types))
  (:method ((target actor) amount &key unblockable damage-types)
    (declare (ignore unblockable damage-types))
    (decf (health target) amount)
    amount)
  (:method :after ((target actor) amount &key unblockable damage-types)
    (declare (ignore unblockable damage-types))
    (when (deadp target)
      (destroy target)))
  (:method ((target combat-entity) amount &key unblockable damage-types)
    (declare (ignore damage-types))
    (unless unblockable
      (setf amount (max 1 (- amount (def target)))))
    (call-next-method target amount :unblockable unblockable)))

(defmethod (setf hunger) (value (obj player))
  (if (= value 0)
      (progn (setf (starvingp obj) t)
	     (damage obj 1 :unblockable t)
	     (setf (slot-value obj 'hunger) 8))
      (progn (when (> value (hunger obj))
	       (setf (starvingp obj) nil))
	     (setf (slot-value obj 'hunger) (min 80 value)))))

(defun eval-attack (attack-list)
  (labels ((calculate-damage (dmg-list)
	     (cond ((= (length dmg-list) 1)
		    (car dmg-list))
		    ((= (length dmg-list) 2)
		     (loop repeat (car dmg-list)
			   sum (roll (cadr dmg-list))))
		    ((= (length dmg-list) 3)
		     (+ (loop repeat (car dmg-list)
			      sum (roll (cadr dmg-list)))
			(caddr dmg-list)))))
	   (eat-attack (a b)
	     (if (numberp (car a))
		 (eat-attack (cdr a) (cons (car a) b))
		 (cons (calculate-damage (reverse b)) a)))
	   (find-statuses (list a b statusp)
	     (cond ((eq (car list) :status)
		    (find-statuses (cdr list) a b t))
		   ((car list)
		    (if statusp
			(find-statuses (cdr list) a (cons (car list) b) t)
			(find-statuses (cdr list) (cons (car list) a) b nil)))
		   (t (list a b)))))
    (let* ((temp (eat-attack attack-list nil))
	   (dmg (car temp))
	   (foo (find-statuses (cdr temp) nil nil nil))
	   (dmg-types (car foo))
	   (statuses (cdr foo)))
      `((dmg ,dmg) (dmg-types ,@dmg-types) (statuses ,@statuses)))))

(defgeneric get-attacks (obj &key for-display rangedp)
  (:method ((obj player) &key for-display rangedp)
    (declare (ignore rangedp))
    (let* ((weapon (gethash 'hand (equips obj)))
	   (attack (if weapon
		       (list (if for-display
				 (atk weapon)
				 (append (atk weapon)
					 (list :status)
					 (onetime-effects weapon)
					 (statuses weapon))))
		       '((1 bludgeoning)))))
      (unless for-display
	(setf (onetime-effects weapon) nil))
      attack))
  (:method ((obj enemy) &key for-display rangedp)
    (declare (ignore for-display))
    (let ((weapon (gethash 'hand (equips obj))))
      (if weapon
	  (if rangedp
	      (when (rangedp (atk weapon))
		(list (atk weapon)))
	      (list (atk weapon)))
	  (if (listp (car (atk obj)))
	      (loop for atk in (atk obj)
		    if rangedp
		      when (rangedp atk)
			collect atk
		    else collect atk)
	      (if rangedp
		  (when (rangedp (atk obj))
		    (list (atk obj)))
		  (list (atk obj))))))))

(defgeneric rangedp (obj)
  (:method ((obj equipment))
    (member 'ranged (atk obj)))
  (:method ((obj list))
    (member 'ranged obj))
  (:method ((obj enemy))
    (loop for atk in (get-attacks obj)
	  thereis (member 'ranged atk))))

(defgeneric attack (a d &key rangedp)
  (:method ((a combat-entity) (d combat-entity) &key rangedp)
    (let ((accuracy (roll 20)))
      (if (and (>= (+ accuracy (dex a)) (+ (- 6 (def d)) (dex d)))
	       (> accuracy 1))
	  (mapc (lambda (atk)
		  (print-to-log "~a hit ~a for ~d damage~a"
				(name a)
				(name d)
				(damage d
					(+ (cadr (assoc 'dmg atk)) (str a))
					:damage-types (cdr
						       (assoc 'dmg-types atk)))
				(if (deadp d)
				    ", killing it"
				    ""))
		  (mapc (lambda (status)
			  (when status
			    (apply-to status d)))
			(cdr (assoc 'statuses atk))))
		(mapcar #'eval-attack (get-attacks a :rangedp rangedp)))
	  (print-to-log "~a missed~&" (name a)))))
  (:method ((a combat-entity) (d actor) &key rangedp)
    (declare (ignore rangedp))
    (when (destructible d)
      (let ((attack (eval-attack (car (get-attacks a)))))
	(print-to-log "~a hit a ~a for ~d damage~a~&"
		      (name a)
		      (name d)
		      (damage d (+ (cadr (assoc 'dmg attack)) (str a))
			      :damage-types (cdr (assoc 'dmg-types attack)))
		      (if (deadp d)
			  ", destroying it"
			  ""))))))

(defun get-player-lines ()
  (list
   (apply-color (log-to-string "~a (~c)" (name *player*) (display-char *player*))
		(color *player*))
   (log-to-string "str: ~2d dex: ~2d" (str *player*) (dex *player*))
   (log-to-string "def: ~2d atk: ~{~a ~}" (def *player*)
		  (car (get-attacks *player* :for-display t)))
   (log-to-string "health: ~d/~d" (health *player*) (max-health *player*))
   (log-to-string "xp: ~d/~d" (xp *player*) (xp-bound *player*))
   (log-to-string "hunger: ~{~c~}" (loop for x below 10
					 collect (if (<= x (ash (hunger *player*) -3))
						     #\/
						     #\-)))
   (if (starvingp *player*) "you are starving!" "")))

(defgeneric check (dc stat creature)
  (:method (dc stat (creature combat-entity))
    (>= (+ (roll 20) (funcall stat creature)) dc))
  (:method (dc stat (creature actor))
    nil))

(defmacro save (dc stat creature failure &optional (success nil))
  `(if (check ,dc (quote ,stat) ,creature)
       ,success
       ,failure))

;; Return an item, chosen by the player, from the given list
;; If the list items are not printable, pass a naming-function that gets a
;; printable name from the list item.
(defun get-item-from-list (lst &key
				 (naming-function (lambda (x) x))
				 (exit-option t)
				 (what "object"))
  (labels ((print-list (l i new-list)
	     (if l
		 (let ((n (funcall naming-function (car l))))
		   (if n
		       (progn (print-to-screen "~%~d) ~a" i n)
			      (print-list (cdr l) (1+ i) (cons (car l) new-list)))
		       (print-list (cdr l) i new-list)))
		 (reverse new-list)))
	   (pick-item ()
	     (fresh-line)
	     (print-to-screen "Choose an ~a: " what)
	     (let ((choice (if (<= (length lst) 10)
			       (digit-char-p (custom-read-char))
			       (parse-integer (read-line)))))
	       (cond ((and choice (< choice (length lst)))
		      (nth choice lst))
		     ((and choice
			   (= choice (length lst))
			   exit-option)
		      nil)
		     (t
		      (print-to-screen "~%That was an invalid choice")
		      (pick-item))))))
    (setf lst (print-list lst 0 nil))
    (when exit-option
      (print-to-screen "~%~d) cancel" (length lst)))
    (pick-item)))

(defun get-item-from-inventory ()
  (get-item-from-list
   (short-inventory)
   :naming-function (lambda (i)
		      (log-to-string "~a~a"
				     (if (> (num-in-inventory i) 1)
					 (log-to-string
					  "~dx "
					  (num-in-inventory i))
					 "")
				     (name i)))))

(defmacro with-item-from-inventory (&body body)
  `(if (= (length *inventory*) 0)
       (print-to-log "you have nothing in your inventory")
       (let ((item (get-item-from-inventory)))
	 (when item
	   ,@body))))

(defun has-los (to from distance)
  (let ((dx (- (car to) (car from)))
        (dy (- (cdr to) (cdr from))))
    (labels ((get-pos-on-line (m)
              (cons (round (+ (car from) (* m dx)))
                    (round (+ (cdr from) (* m dy)))))
	     (on-board-p (m)
	       (let ((pos (get-pos-on-line m)))
		 (and (gethash pos (board))
		      (not (member pos (walls) :test #'equal))))))
      (and (or (< distance 0)
	       (>= distance (+ (abs dx) (abs dy))))
           (loop for x below (abs dx)
                 always (on-board-p (/ x (abs dx))))
           (loop for y below (abs dy)
                 always (on-board-p (/ y (abs dy))))))))

(defun update-spaces-found ()
  (mapc (lambda (pos)
	  (setf (gethash pos (board)) 'found))
	*light-zone*))

(defun update-los ()
  (setf *light-zone*
	(loop for p being the hash-keys of (board)
	      when (has-los (pos *player*) p *sight-distance*)
		collect p))
  (update-spaces-found))

(defun change-layer (direction)
  (unless (or (and (= *layer-index* 0) (= direction -1))
	      (and (= (+ *layer-index* direction) (length *layers*))))
    (incf *layer-index* direction)
    (setf *current-layer* (nth *layer-index* *layers*))
    (if (= direction -1)
	(setf (pos *player*) (down-ladder-pos *current-layer*))
	(setf (pos *player*) (up-ladder-pos *current-layer*)))
    (update-los)
    t))

(defgeneric interact (a b)
  (:method (a b)) ; do nothing by default
  (:method :after (a (b actor))
    (if (consumable b)
	(destroy b)))
  (:method ((a player) (b enemy))
    (attack a b))
  (:method ((a player) (b ladder))
    (when (change-layer (direction b))
      (if (= (direction b) -1)
	  (print-to-log "you climb up the ladder to the previous dungeon level")
	  (print-to-log "you climb down the ladder to the next dungeon level"))))
  (:method ((a player) (b pickup))
    (when (add-to-inventory (equipment b))
      (print-to-log "You have picked up a ~a~&" (name b))))
  (:method ((a player) (b secret-door))
    (unless (eq (color b) 'grey)
      (setf (color b) 'grey)
      (incf (xp *player*) 2)
      (print-to-log "you have discovered a secret door")))
  (:method ((a player) (b corpse))
    (loop for item in (loot b)
	  when (add-to-inventory item)
	    do (print-to-log "you have picked up a ~a~&" (name item))))
  (:method ((a enemy) (b player))
    (attack a b)))

(defgeneric visiblep (obj)
  (:method :around (obj)
    (if (= *sight-distance* -1)
	t
	(call-next-method)))
  (:method ((obj list))
    (member obj *light-zone* :test 'equal))
  (:method ((obj actor))
    (cond ((hiddenp obj) nil) ; if it's hidden, it's not visible
	  ((persistent-visiblity-p obj) ; visible if any adjacent space has been seen
	   (loop for d in (list +left+ +right+ +up+ +down+)
		   thereis (eq (gethash (add-pos (pos obj) d) (board)) 'found)))
	  (t (visiblep (pos obj)))))) ; visible by position

;; returns a direction value pair chosen by the user.
(defun get-direction (&key include-zero (cancel t))
  (print-to-screen "~%Pick a direction (w, a, s, d~a~a): "
		(if include-zero ", (h)ere" "")
		(if cancel ", (c)ancel" ""))
  (let ((input (custom-read-char)))
    (cond ((equal input #\a)
	   +left+)
	  ((equal input #\d)
	   +right+)
	  ((equal input #\w)
	   +up+)
	  ((equal input #\s)
	   +down+)
	  ((and (equal input #\h) include-zero)
	   +zero+)
	  ((and (equal input #\c) cancel)
	   nil)
	  (t
	   (print-to-screen "~%That was not a direction")
	   (get-direction)))))

(defun get-number-input (bound prompt)
  (print-to-screen "~%~a (0~a): "
		   prompt
		   (if (= bound 0)
		       ""
		       (log-to-string "-~d" bound)))
  (let ((input (digit-char-p (custom-read-char))))
    (if (and input (<= 0 input bound))
	input
	(progn (print-to-screen "~%invalid input")
	       (get-number-input bound prompt)))))

(defgeneric find-actor-at (a &rest actors-to-ignore)
  (:method ((a list) &rest actors-to-ignore)
    (loop for actor in (actors)
	  unless (member actor actors-to-ignore :test #'equal)
	    when (equal a (pos actor))
	      return actor))
  (:method ((a actor) &rest actors-to-ignore)
    (apply #'find-actor-at (pos a) a actors-to-ignore)))

(defgeneric find-all-actors-at (a &rest actors-to-ignore)
  (:method ((a list) &rest actors-to-ignore)
    (loop for actor in (actors)
	  unless (member actor actors-to-ignore :test #'equal)
	    when (equal a (pos actor))
	      collect actor))
  (:method ((a actor) &rest actors-to-ignore)
    (apply #'find-all-actors-at (pos a) a actors-to-ignore)))

(defgeneric throw-at (item target)
  (:method :before ((item equipment) target)
    (declare (ignore target))
    (remove-from-inventory item)
    (print-to-log "you threw ~a~%" (description item)))
  (:method ((item equipment) target))
  (:method :after ((item equipment) (target list))
    (unless (breakable item)
      (make-pickup item target)))
  (:method :after ((item equipment) (target actor))
    (unless (breakable item)
      (make-pickup item (pos target)))))

(defun find-solid-actor-at (a &rest actors-to-ignore)
  (loop for actor in (apply #'find-all-actors-at a actors-to-ignore)
	when (solid actor)
	  return actor))

(defun choose-actor-at (pos)
  (let ((actor-list (remove nil (loop for actor in (find-all-actors-at pos)
				      collect (if (and (not (hiddenp actor))
						       (description actor))
						  actor)))))
    (if (<= (length actor-list) 1)
	(if (and (car actor-list) (not (hiddenp (car actor-list))))
	    (car actor-list)
	    nil)
	(get-item-from-list (find-all-actors-at pos)
			    :naming-function (lambda (x)
					       (if (hiddenp x)
						   nil
						   (name x)))))))

(defmacro for-each-adjacent-actor (pos &body body)
  (let ((p (gensym)))
    `(loop for ,p in (mapcar (lambda (x) (add-pos ,pos x))
			     (list +right+ +left+ +down+ +up+ +zero+))
	   do (loop for actor in (find-all-actors-at ,p)
		    do (progn ,@body)))))

(defgeneric move (obj distance)
  (:method ((obj actor) (distance list))
    (let* ((newpos (add-pos (pos obj) distance))
	   (collider (find-solid-actor-at newpos obj)))
      (if collider
	  (unless (interact-action-only collider)
	      (interact obj collider))
	  (when (gethash newpos (board))
	    (setf (pos obj) newpos)
	    (loop for actor in (find-all-actors-at obj)
		  unless (interact-action-only actor)
		    do (interact obj actor))))))
  (:method ((obj player) (distance list))
    (call-next-method)
    (update-los)))

;;; Use breadth-first search to find shortest path between the two input points
(defun find-path (from to)
  (let ((came-from (make-hash-table :test 'equal)))
    (setf (gethash from came-from) t)
    (labels ((neighbors (pos)
	       (loop for direction in (list +left+ +right+ +up+ +down+)
		     collect (let ((newpos (add-pos pos direction)))
			       (if (gethash newpos (board))
				   (if (and (find-solid-actor-at newpos)
					    (not (equal newpos to)))
				       nil
				       newpos)
				   nil))))
	     (iterate (frontier)
	       (let ((current (car frontier)))
		 (when (and current
			    (not (equal current to)))
		   (loop for neighbor in (neighbors current)
			 when neighbor
			   do (unless (gethash neighbor came-from)
				(setf (gethash neighbor came-from)
				      current)
				(setf frontier
				      (append frontier
					      (list neighbor)))))
		   (iterate (cdr frontier)))))
	     (build-path (path pos)
	       (if (equal pos from)
		   path
		   (build-path (cons pos path) (gethash pos came-from)))))
      (iterate (list from))
      (if (gethash to came-from)
	  (build-path '() to)
	  (list from)))))

(defun step-towards (to from)
  (sub-pos (car (find-path (pos from) (pos to))) (pos from)))

(defgeneric update (obj)
  (:method :around ((obj enemy))
    (when (and (not (enabled obj))
	       (has-los (pos obj) (pos *player*) -1))
      (setf (enabled obj) t))
    (when (and (enabled obj)
	       (< (mod *player-actions* (spd obj)) 1))
      (call-next-method)))
  (:method ((obj enemy))
    (if (and (rangedp obj)
	     (has-los (pos obj) (pos *player*) 4))
	(attack obj *player* :rangedp (> (distance (pos obj) (pos *player*)) 1))
	(move obj (step-towards *player* obj))))
  (:method ((obj player))
    (decf (hunger *player*))
    (decf (heal-clock *player*)))    
  (:method ((obj corpse))
    (decf (decay-time obj))
    (when (<= (decay-time obj) 0)
      (destroy obj)
      (setf (bone-type (make-bones (pos obj))) (corpse-type obj))))
  (:method ((obj status))
    (decf (duration obj))
    (on-update obj)
    (when (<= (duration obj) 0)
      (destroy obj))))

(defun print-board ()
  (let ((actor-chars (make-hash-table :test 'equal))
	(wall-positions ()))
    (loop for actor in (actors)
	  when (visiblep actor)
	    do (setf (gethash (pos actor) actor-chars) (get-ascii actor))
	  when (and (wallp actor)
		    (loop for direction in (list +left+ +right+ +up+ +down+)
			    thereis (eq (gethash (add-pos (pos actor) direction) (board))
					'found)))
	    do (push (pos actor) wall-positions))
    (labels ((on-board (pos) (gethash pos (board)))
	     (is-wall (pos) (member pos wall-positions :test #'equal))
	     (foundp (pos) (eq (on-board pos) 'found))
	     (get-char (pos)
	       (if (on-board pos) ; is the cell on the board?
		   (let ((c (gethash pos actor-chars)))
		     (cond (c c) ; if there's an actor char, use it
			   ((visiblep pos) ; if it's visible, show period
			    #\.)
			   ; if *show-found-spaces*, show all found spaces in grey
			   ((and *show-found-spaces* *in-terminal* (foundp pos))
			    (apply-color #\. 'grey))
			   (t ; otherwise, show a space
			    #\space)))
		   ;; if the cell is not on the board, check all
		   ;; adjacent cells and add walls as necessary.
		   (cond ((or (is-wall (add-pos pos +up+))
			      (is-wall (add-pos pos +down+)))
			  #\|) ; vertically adjacent secret door
			 ((or (is-wall (add-pos pos +left+))
			      (is-wall (add-pos pos +right+)))
			  #\-) ; horizontally adjacent secret door
			 ((or (foundp (add-pos pos +left+))
			      (foundp (add-pos pos +right+)))
			  #\|) ; vertical wall
			 ((or (foundp (add-pos pos +up+))
			      (foundp (add-pos pos +down+)))
			  #\-) ; horizontal wall
			 (t #\space)))))
      ;; print the board
      (let ((player-info (get-player-lines)))
	(loop for y from -1 to (+ (cdr (board-size)) 1)
	      do (format t "~{~a~} ~a~%"
			 (loop for x from -1 to (+ (car (board-size)) 1)
			       collect (get-char (cons x y)))
			 (if (and (>= y 0) (< y (length player-info)))
			     (nth y player-info)
			     "")))))))

(defun print-inventory ()
  (print-to-log "~t~a ~30t~a~%" "INVENTORY" "EQUIPPED")
  (let* ((inventory-list (mapcar (lambda (item)
				  (log-to-string "~a~a"
						 (if (> (num-in-inventory item) 1)
						     (log-to-string
						      "~dx "
						      (num-in-inventory item))
						     "")
						 (name item)))
				(short-inventory)))
	(equipped-list (loop for eq-slot being the hash-keys of (equips *player*)
			     collect (let ((item (gethash eq-slot (equips *player*))))
				       (when item
					 (log-to-string "~a: ~a"
							eq-slot (name item)))))))
    (mapc (lambda (a b)
	    (print-to-log "~t~a ~30t~a~%" a b))
	  (loop for x below *inventory-size*
		collect (let ((val (nth x inventory-list)))
			  (if val val "")))
	  (loop for x below *inventory-size*
		collect (let ((val (nth x equipped-list)))
			  (if val val ""))))))

(defun input (cmd)
  (let ((action (gethash cmd *actions*)))
    (when action
      (funcall action))))

;;; iterate through the list of dynamic actors and update them
(defun update-all-actors ()
  (update *player*)
  (mapc (lambda (actor)
	  (update actor))
	(append (dynamic-actors) *statuses*)))

;; clears the terminal if possible
(defun clear-terminal ()
  (when *in-terminal*
    (format t "~cc" #\esc)))

(defun print-log (&optional (clear t))
  (format t "~{~a~&~}" *log*)
  (when clear
    (setf *log* '())))

(defun level-up ()
  (print-to-screen "~%LEVEL UP!")
  (decf (xp *player*) (xp-bound *player*))
  (incf (xp-bound *player*) (xp-bound *player*))
  (let ((health-increase (roll 10))
	(upgrade-stat (get-item-from-list '(dex str)
					  :what "stat" :exit-option nil)))
    (incf (max-health *player*) health-increase)
    (incf (health *player*) health-increase)
    (eval `(incf (,upgrade-stat *player*))))
  (when *level-up-pending*
    (level-up)))

(defun game-loop (&optional cmd)
  (unless (equal cmd #\q)
    ;; only update if input was a valid command
    (when (input cmd)
      (update-all-actors))
    (clear-terminal)
    (print-board)
    (print-log)
    (when *level-up-pending*
      (level-up))
    (if (deadp *player*)
	(print-to-screen "~%~a has died.~%~%" (name *player*))
	(game-loop (custom-read-char)))))

(defun create-new-player ()
  (let ((p-name (progn (print-to-screen "enter the name of your character: ")
		       (read-line)))
	(p-color 'white)
	(p-char #\P))
    (cond ((equal p-name "F")
	   (setf p-color 'red)
	   (setf p-name "Foobar")
	   (setf p-char #\F))
	  (*in-terminal*
	   (setf p-color (get-item-from-list *color-list*
					     :naming-function
					     (lambda (x)
					       (apply-color
						(log-to-string "~a" x)
						x))
					     :what "color"
					     :exit-option nil))
	   (setf p-char (progn (print-to-screen "~%enter a character: ")
			       (read-char))))
	  (t
	   (setf p-char (progn (print-to-screen "~%enter a character: ")
			       (read-char)))))
    (setf (name *player*) p-name)
    (setf (color *player*) p-color)
    (setf (display-char *player*) p-char)))

(defun create-dungeon (dungeon-depth)
  (mapcar #'make-layer
	  (loop for layer downfrom dungeon-depth to 1
		collect (let* ((i (random 2))
			       (size (if (= i 0)
					 '(50 . 20)
					 '(60 . 20)))
			       (depth (if (= i 0)
					  3
					  4)))
			  (generate-dungeon size depth (spawn-list layer))))))

(defun start ()
  (create-dungeon 3)
  (setf *current-layer* (car *layers*))
  (update-los)
  (create-new-player)
  (game-loop))
