(define-condition crash-signalled-condition (condition)
  ((original-error :initarg :error :accessor original-error)))

(defstruct damage
  (amount 0 :type fixnum)
  source
  types
  (statuses nil :type list))

(defstruct (attack (:include damage))
  (to-hit 0 :type fixnum))

(defstruct spell
  function
  (cost 1)
  (requires-target-p nil)
  name)

(defstruct layer
  (solid-actors (make-hash-table :test #'equal))
  (non-solid-actors (make-hash-table :test #'equal))
  (terrain (make-hash-table :test #'equal))
  glowing-actors
  up-ladder-pos
  down-ladder-pos)

(defclass actor ()
  ((display-char :initform #\? :initarg :char :writer (setf display-char))
   (illumination :initform 0 :initarg :illumination :reader illumination)
   (pos :initform nil :initarg :pos :accessor pos)
   (name :initform "" :initarg :name :accessor name)
   (color :initform 255 :initarg :color :writer (setf color))
   (hiddenp :initform nil :initarg :hiddenp :accessor hiddenp)
   (description :initform nil :initarg :description :writer (setf description))))

(defclass breakable (actor)
  ((break-chance :initform 50 :initarg :break-chance :accessor break-chance)))

(defclass furniture (breakable)
  ((bg-color :initform nil :initarg :bg-color :writer (setf bg-color))))

(defclass creature (actor)
  ((stats :initform nil :initarg :stats :accessor stats)
   (health :initform 1 :initarg :health :reader health)
   (armor :initform 0 :initarg :armor :accessor armor)
   (evasion :initform 0 :initarg :evd :writer (setf evasion))
   (max-health :initform 10 :reader max-health)
   (idle-time :initform 0 :accessor idle-time)
   (primary-stat :initform 'str+ :initarg :primary-stat :reader primary-stat)
   (equipment :initform (make-hash-table) :accessor equipment)
   (natural-weapons :initform nil :accessor natural-weapons :initarg :natural-weapons)
   (allies :initform 0 :initarg :allies)
   (enemies :initform 0 :initarg :enemies)
   (types :initform 0 :initarg :types)
   (slot-nums :initform '((hand 2) (misc 3) (body 1)) :initarg :slot-nums :accessor slot-nums)
   (statuses :initform nil :accessor statuses)
   (resistances :initform 0 :initarg :resist)
   (immunities :initform 0 :initarg :immune)
   (vulnerabilities :initform 0 :initarg :vulnerable)
   (absorbances :initform 0 :initarg :absorb)
   (darkvisionp :initform t :accessor darkvisionp :initarg :darkvisionp)))

(defclass player (creature)
  ((hunger :initform 100 :reader hunger)
   (alignment :initform 'g)
   (darkvisionp :initform nil)
   (max-hunger :initform 100 :accessor max-hunger)))

(defclass enemy (creature)
  ((energy :initform 0 :accessor energy)
   (idle-behavior :initform 'smart-wander :accessor idle-behavior :initarg :idle)
   (target-pos :initform nil :accessor target-pos)
   (meat :initform nil :writer (setf meat) :initarg :meat)
   (xp :initform 1 :reader xp :initarg :xp)
   (loot :initform nil :accessor loot :initarg :loot)
   (morale :initform 0 :accessor morale :initarg :morale)))

(defclass equipment (breakable)
  ((atk :initform '(1 3 0 0 bludgeoning) :initarg :atk :accessor atk)
   (burn-time :initform 0 :initarg :burn-time :accessor burn-time)
   (range :initform 1 :initarg :range :accessor range)
   (size :initform 1 :initarg :size :accessor size)
   (price :initform 2 :initarg :cost :accessor price)
   (weaponp :initform nil :initarg :weaponp :accessor weaponp)
   (shopkeeper :initform nil :accessor shopkeeper)
   (equip-slot :initform 'hand :initarg :slot :accessor equip-slot)))

(defclass secret-equipment (equipment)
  ((cover-name :initform "" :initarg :cover-name :accessor cover-name)
   (identifiedp :initform nil :accessor identifiedp)))

(defclass status ()
  ((energy :initform 0 :accessor energy)
   (spd+ :initform 1 :initarg :spd :accessor spd+)
   (target :initform nil :accessor target)
   (name :initform 'status :accessor name :initarg :name)
   (duration :initform 3 :initarg :duration :accessor duration)))
