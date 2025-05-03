(macrolet ((define-setter-getter-pairs (&rest names)
	     `(progn ,@(loop for name in names
			     collect `(defgeneric (setf ,name) (value obj))
			     collect `(defgeneric ,name (obj))))))
  (define-setter-getter-pairs resistances immunities
    absorbances vulnerabilities allies enemies types))

(defgeneric drop-corpse (obj))

(defgeneric kill (obj)
  (:method (obj)))

(defgeneric wallp (obj)
  (:method (obj) nil))

(defgeneric playerp (obj)
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
  (:method ((obj character) &key &allow-other-keys) obj))

(defgeneric deadp (obj))
  
(defgeneric death (obj)
  (:documentation "a string describing the object's destruction"))

(defgeneric weapons (obj))

(defgeneric description (obj))

(defgeneric identify (obj))

(defgeneric meat (obj))

(defgeneric breaksp (obj &optional offset))

(defgeneric remove-status (status)
  (:method (status)))

(defgeneric look-at (object))

(defgeneric quaffablep (obj)
  (:method (obj) nil))

(defgeneric movement-cost (object)
  (:method (object) 0)
  (:method ((object (eql nil))) 0))

(defgeneric apply-to (subj obj)
  (:method (subj obj)))

(defgeneric get-attack (weapon attacker))

(defgeneric damage (defender amount types &optional statuses))

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

(defgeneric move-into (passive active)
  (:documentation "resolves any effects that occur when active moves into passive")
  (:method ((passive (eql nil)) active))
  (:method (passive active)))

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

(defgeneric alignment (obj))

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
