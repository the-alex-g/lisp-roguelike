(defstruct attack dmg to-hit source types statuses)

(defstruct layer
  (solid-actors (make-hash-table :test #'equal))
  (non-solid-actors (make-hash-table :test #'equal))
  glowing-actors
  up-ladder-pos
  down-ladder-pos)

(defclass actor ()
  ((display-char :initform #\? :initarg :display-char :writer (setf display-char))
   (illumination :initform 0 :initarg :illumination :reader illumination)
   (pos :initform +zero+ :initarg :pos :accessor pos)
   (name :initform "" :initarg :name :accessor name)
   (color :initform 30 :initarg :color :writer (setf color))
   (hiddenp :initform nil :initarg :hiddenp :accessor hiddenp)
   (description :initform nil :initarg :description :writer (setf description))))

(defclass creature (actor)
  ((dex :initform 0 :initarg :dex :accessor dex)
   (str :initform 0 :initarg :str :accessor str)
   (con :initform 0 :initarg :str :reader con)
   (int :initform 0 :initarg :int :accessor intl)
   (per :initform 0 :initarg :per :accessor per)
   (cha :initform 0 :initarg :cha :accessor cha)
   (spd :initform 1 :initarg :spd :accessor spd)
   (health :initform 1 :initarg :health :reader health)
   (armor :initform 0 :initarg :armor :accessor armor)
   (evasion :initform 0 :initarg :evd :writer (setf evasion))
   (max-health :initform 10 :reader max-health)
   (equipment :initform (make-hash-table) :accessor equipment)
   (slot-nums :initform '((hand 2) (misc 3) (body 1)) :initarg :slot-nums :accessor slot-nums)
   (statuses :initform nil :accessor statuses)
   (resistances :initform '() :initarg :resist :accessor resistances)
   (immunities :initform '() :initarg :immune :accessor immunities)
   (vulnerablities :initform '() :initarg :vulnerable :accessor vulnerabilities)
   (absorbances :initform '() :initarg :absorb :accessor absorbances)))

(defclass player (creature)
  ((hunger :initform 100 :reader hunger)
   (max-hunger :initform 100 :accessor max-hunger)))

(defclass enemy (creature)
  ((energy :initform 0 :accessor energy)
   (target-pos :initform nil :accessor target-pos)
   (meat :initform nil :accessor meat :initarg :meat)
   (morale :initform 6 :accessor morale :initarg :morale)))

(defclass equipment (actor)
  ((atk :initform '(1 3 0 0 bludgeoning) :initarg :atk :accessor atk)
   (range :initform 1 :initarg :range :accessor range)
   (size :initform 1 :initarg :size :accessor size)
   (price :initform 2 :initarg :price :accessor price)
   (weaponp :initform nil :initarg :weaponp :accessor weaponp)
   (shopkeeper :initform nil :accessor shopkeeper)
   (equip-slot :initform 'hand :initarg :slot :accessor equip-slot)))

(defclass status ()
  ((energy :initform 0 :accessor energy)
   (spd :initform 1 :initarg :spd :accessor spd)
   (target :initform nil :accessor target)
   (name :initform 'status :accessor name :initarg :name)
   (duration :initform 3 :initarg :duration :accessor duration)))
