(defequipment sword () :atk '(1 6 0 0 slashing) :weaponp t)
(defequipment dagger () :atk '(1 4 0 0 piercing) :weaponp t)
(defequipment food ((sustenance (roll 2 10 10))))

(defenemy goblin #\g () :color 32 :health (roll 1 4 1) :equips (make-dagger) :dex 1)
(defenemy kobold #\k () :color 91 :health (roll 1 4 1) :equips (make-dagger) :str 1 :dex 1 :armor 1)

(defstatus resting :duration -1 :speed 5)
(defstatus frightened)
(defstatus brave)
(defstatus immobilized)
