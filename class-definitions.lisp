(defclass display-object ()
  ((display-char :accessor display-char :initarg :display-char :initform #\#)
   (name :initform "" :accessor name :initarg :name)
   (description :accessor description :initarg :description)
   (color :initform 'white :accessor color :initarg :color)))

(defclass destructible-object ()
  ((resist :initform nil :initarg :resist)
   (immune :initform nil :initarg :immune)
   (vulnerable :initform nil :initarg :vulnerable)
   (health :initform 1 :initarg :health :accessor health)))

(defclass stat-object (destructible-object)
  ((def :initform 0 :accessor def :initarg :def)
   (str :initform 0 :accessor str :initarg :str)
   (dex :initform 0 :accessor dex :initarg :dex)))

(defclass equipment (display-object stat-object)
  ((atk :initform '(1 bludgeoning) :accessor atk :initarg :atk)
   (weaponp :initform nil :accessor weaponp :initarg :weaponp)
   (breakable :initform nil :accessor breakable :initarg :breakable)
   (throw-distance :initform 2
		   :accessor throw-distance :initarg :throw-distance)
   (burn-time :initform 0 :accessor burn-time :initarg :burn-time)
   (consumable :initform nil :accessor consumable :initarg :consumable)
   (secretp :initform nil :accessor secretp :initarg :secretp)
   (identifiedp :initform t :accessor identifiedp :allocation :class)
   (container :initform nil :accessor container)
   (fake-name :accessor fake-name :initarg :fake-name)
   (equip-slot :initform 'hand :accessor equip-slot :initarg :equip-slot)
   ;; set new initforms
   (display-char :initform #\*)
   (health :initform 0)))

(defclass actor (destructible-object display-object)
  ((pos
    :initarg :pos
    :accessor pos)
   (dynamicp :initform nil :initarg :dynamicp :accessor dynamicp)
   (solid
    :initform t
    :initarg :solid
    :accessor solid)
   (interact-action-only
    :initform nil
    :initarg :interact-action-only
    :accessor interact-action-only)
   (persistent-visiblity-p
    :initform nil
    :initarg :persistent-visiblity-p
    :accessor persistent-visiblity-p)
   (wallp :initform nil :initarg :wallp :accessor wallp)
   (hiddenp
    :initform nil
    :initarg :hiddenp
    :accessor hiddenp)
   (destructible :initform t
		 :initarg :destructible
		 :accessor destructible)
   (consumable
    :initform nil
    :initarg :consumable
    :accessor consumable)))

(defclass combat-entity (actor stat-object)
  ((equips :initform (make-hash-table) :accessor equips :initarg :equips)))

(defclass player (combat-entity)
  ((heal-clock :initform 10
	       :reader heal-clock)
   (starvingp :initform nil
	      :accessor starvingp)
   (hunger :initform 80
	   :accessor hunger)
   (xp :initform 0 :reader xp)
   (max-health :initform 10 :accessor max-health)
   (xp-bound :initform 10 :accessor xp-bound)))

(defclass enemy (combat-entity)
  ((spd ;; speed of 1 is the same as the player
        ;; speed of 2 is half as fast as the player
    :initform 1.2
    :initarg :spd
    :accessor spd)
   (atk :initform '(1 bludgeoning) :initarg :atk :accessor atk)
   (dynamicp :initform t)
   (xp :initform 1 :initarg :xp :accessor xp)
   (loot
    :initform '()
    :initarg :loot
    :accessor loot)
   (enabled
    :initform nil
    :accessor enabled)))

(defclass layer ()
  ((board :initarg :board)
   (dynamic-actors :initform '())
   (actors :initform '())
   (up-ladder-pos :initarg :up-ladder-pos :accessor up-ladder-pos)
   (down-ladder-pos :initarg :down-ladder-pos :accessor down-ladder-pos)
   (board-size :initarg :board-size)))

(defclass status ()
  ((duration :accessor duration :initarg :duration)
   (on-applied)
   (on-update)
   (on-removed)
   (target :initform nil :accessor target)))

(defclass pickup (actor)
  ((consumable :initform t)
   (solid :initform nil)
   (equipment :initarg :equipment :accessor equipment)))
