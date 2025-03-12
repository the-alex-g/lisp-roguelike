(defequipment sword () :atk '(1 6 0 0 slashing) :weaponp t)
(defequipment dagger () :atk '(1 4 0 0 piercing) :weaponp t)
(defequipment food ((sustenance (roll 2 10 10))))
(defequipment kobold-meat () :sustenance (roll 1 10 5) :inherit food)
(defequipment fist () :atk '(1 3 -1 0 bludgeoning) :weaponp t)

(defactor corpse #\c (loot) :solidp nil)
(defactor ladder #\# (direction) :solidp nil)

(defenemy goblin #\g () :color 32 :health (roll 1 4 1) :equips (make-dagger) :dex 1
  :meat (roll 2 10 10))
(defenemy kobold #\k () :color 91 :health (roll 1 4 1) :equips (make-dagger) :str 1 :dex 1 :armor 1
  :meat (make-kobold-meat))

(defstatus resting :duration -1 :speed 0.2)
(defstatus frightened)
(defstatus brave)
(defstatus immobilized)
