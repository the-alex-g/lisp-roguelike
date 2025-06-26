(macrolet ((define-setter-getter-pairs (&rest names)
	     `(progn ,@(loop for name in names
			     collect `(defgeneric (setf ,name) (value obj))
			     collect `(defgeneric ,name (obj)))))
	   (define-stat-setters-and-getters (&rest names)
	       `(progn ,@(apply #'append (loop for name in names
					collect (let ((bonus-name (read-from-string
								   (format nil "~a+" name)))
						      (die-name (read-from-string
								 (format nil "~a-die" name))))
						  `((defgeneric (setf ,bonus-name) (value obj))
						    (defgeneric ,bonus-name (obj))
						    (defgeneric (setf ,die-name) (value obj))
						    (defgeneric ,die-name (obj)))))))))
  (define-setter-getter-pairs resistances immunities
    absorbances vulnerabilities allies enemies types)
  (define-stat-setters-and-getters str dex con knl per det cha spd))

(defgeneric drop-corpse (obj))

(defgeneric kill (obj killer)
  (:method (obj killer)))

(defgeneric wallp (obj)
  (:method (obj) nil))

(defgeneric playerp (obj)
  (:method (obj) nil))

(defgeneric corpsep (obj)
  (:method (obj) nil))

(defgeneric sell-price (item))

(defgeneric color (obj)
  (:documentation "returns a 256-value color")
  (:method (obj) 7)
  (:method ((obj number)) obj))

(defgeneric bg-color (obj)
  (:method ((obj (eql nil))) nil)
  (:method (obj) nil)
  (:method ((obj symbol)) nil))

(defgeneric display-char (obj &key &allow-other-keys)
  (:method ((obj character) &key &allow-other-keys) obj)
  (:method ((obj (eql nil)) &key &allow-other-keys) #\space))

(defgeneric deadp (obj))

(defgeneric (setf deadp) (value obj))

(defgeneric death (obj)
  (:documentation "a string describing the object's destruction"))

(defgeneric weapons (obj &key &allow-other-keys))

(defgeneric description (obj))

(defgeneric identify (obj))

(defgeneric meat (obj))

(defgeneric breaksp (obj &optional offset))

(defgeneric remove-status (status)
  (:method (status)))

(defgeneric look-at (object))

(defgeneric quaffablep (obj)
  (:method (obj) nil))

(defgeneric zappablep (obj)
  (:method (obj) nil))

(defgeneric movement-cost (object &rest keys &key &allow-other-keys)
  (:method (object &rest keys &key &allow-other-keys) 0)
  (:method ((object (eql nil)) &rest keys &key &allow-other-keys) 0))

(defgeneric apply-to (subj obj)
  (:method (subj obj)))

(defgeneric get-attack (weapon attacker))

(defgeneric damage (defender damage &optional blockablep)
  (:method (defender damage &optional blockablep)))

(defgeneric visiblep (pos from))

(defgeneric attack (defender attacker)
  (:method (defender attacker)))

(defgeneric pickup (item)
  (:documentation "remove item from board and add it to inventory")
  (:method (item)))

(defgeneric interact (object actor)
  (:documentation "function to call when player takes the interact action on an object")
  (:method (object actor)))

(defgeneric heal (actor amount &key &allow-other-keys)
  (:documentation "increases health and prints if appropriate"))

(defgeneric trigger (object activator)
  (:documentation "make a thing happen to the activator")
  (:method (object activator)))

(defgeneric move-into (passive active repositioningp)
  (:documentation "resolves any effects that occur when active moves into passive and returns cost
of interaction")
  (:method ((passive (eql nil)) active repositioningp))
  (:method (passive active repositioningp)))

(defgeneric reposition (obj new-pos)
  (:documentation "moves a solid object into a new position, calling (move-into). If the position
is occupied, the object is not actually moved. Returns the cost of the movement."))
  
(defgeneric move (obj direction)
  (:documentation "moves object in direction using #'reposition. Returns the cost of the movement."))

(defgeneric act (obj &key &allow-other-keys)
  (:documentation "performs an action and returns action's cost")
  (:method (obj &key &allow-other-keys)))

(defgeneric update (obj)
  (:documentation "increments energy
on enemies, calls #'act until object runs out of energy")
  (:method (obj)))

(defgeneric throw-at (target obj thrower)
  (:method (target obj thrower)))

(defgeneric surrounding-name (obj))

(defgeneric remove-from-inventory (item &key &allow-other-keys))

(defgeneric add-to-inventory (item))

(defgeneric unequip (item actor &key to)
  (:method (item actor &key to)
    (declare (ignore item actor to))))

(defgeneric equip (item actor)
  (:method (item actor)))

(defgeneric hostilep (obj to)
  (:method (obj to) nil))

(defgeneric alliedp (obj to)
  (:method (obj to) nil))

(defgeneric stationaryp (obj)
  (:method (obj) obj))

(defgeneric (setf health) (value obj))

(defgeneric (setf con) (value obj))

(defgeneric (setf hunger) (value obj))

(defgeneric (setf illumination) (value obj))

(defgeneric level (obj)
  (:method (obj) 0))

(defgeneric get-loot (obj))

(defgeneric evadesp (obj dc)
  (:method (obj dc) nil))

(defgeneric evd+ (obj)
  (:method (obj) 0))

(defgeneric make-hostile (obj to))

(defgeneric reanimate (obj &optional master)
  (:method (obj &optional master)))

(defgeneric cast-spell (spell obj &key &allow-other-keys)
  (:method (spell obj &key &allow-other-keys)))

(defgeneric zap (obj zapper))

(defgeneric checkp (stat obj dc)
  (:method (stat obj dc) nil))

(defgeneric place-into (place obj)
  (:documentation "called when an object is placed using the (place) function. If this method
returns true, placement continues normally.")
  (:method (place obj) t))

(defgeneric cook (obj))

(defgeneric can-attack-p (weapon owner &optional for-attack)
  (:method (weapon owner &optional for-attack) t))

(defgeneric resolve-attack (weapon owner)
  (:method (weapon owner) nil))
