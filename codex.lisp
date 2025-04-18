;; EQUIPMENT

(defabstract secret-equipment potion () :char #\p)
(defabstract equipment weapon () :weaponp t)

(add-to-shop
 (defequipment sword () :atk '(1 6 0 0 slashing) :inherit weapon)
 (defequipment sword-+1 () :atk '(1 6 1 1 slashing) :inherit weapon)
 (defequipment dagger () :atk '(1 4 0 0 piercing) :inherit weapon)
 (defequipment bow () :atk '(1 4 0 0 piercing) :inherit weapon :range 4 :size 2)
 (defequipment food ((sustenance (roll 2 10 10))) :description "recovers 12-30 hunger")
 (defequipment warclub () :atk '(2 6 0 0 bludgeoning) :inherit weapon :size 2)
 (defsecretequipment healing-potion
     ((blue-potion :color 34)
      (green-potion :color 32)
      (red-potion :color 31))
   ((healing (roll 2 4))) :inherit potion))
(defequipment kobold-meat ()
  :sustenance (roll 1 10 5) :inherit food :description "recovers 6-15 hunger")
(defequipment goblin-meat ()
  :sustenance (roll 2 10 10) :inherit food :description "recovers 12-30 hunger")
(defequipment fist () :atk '(1 3 -1 0 bludgeoning) :inherit weapon)
(defequipment gold (amount) :solidp nil :color 'yellow :char #\*)
(defequipment crude-bow () :inherit bow :atk '(1 3 0 -1 piercing))

(defun make-gold (&optional (amount 1))
  (let ((gold (make-instance 'gold)))
    (setf (amount gold) amount)
    gold))

;; ACTORS

(defabstract actor trap ((trigger-chance 100)
			 (find-dc 10)
			 (avoid-dc 10)
			 (searchedp nil))
  :solidp nil :color 31 :hiddenp t)

(defactor corpse #\c (loot) :solidp nil)
(defactor ladder #\# (direction) :solidp nil)
(defactor pit-trap #\! () :inherit trap :solidp nil)
(defactor shopkeeper-pedestal #\I () :solidp nil)

;; ENEMIES

(defenemy goblin #\g () :color 32 :health (roll 1 4 1) :equips (make-dagger) :dex 1
  :meat (make-goblin-meat)
  :loot (list (make-gold (roll 1 6)) (make-dagger)))
(defenemy goblin-archer #\g () :inherit goblin
  :equips (make-crude-bow)
  :loot (list (make-gold (roll 2 4)) (make-crude-bow)))
(defenemy kobold #\k () :color 91 :health (roll 1 4 1) :equips (make-dagger) :str 1 :dex 1 :armor 1
  :meat (make-kobold-meat))
(defenemy troll #\T () :color 96 :health (roll 2 8) :equips (make-warclub)
  :str 2 :dex -1 :armor 1 :spd 3/4 :con 2 :meat (roll 2 10 20))
(defenemy shopkeeper #\U ((domain 4) (enragedp nil)) :color 'purple :health (roll 3 10 5)
  :equips (list (make-sword-+1) (make-sword-+1)))

;; STATUSES

(defstatus resting :duration -1 :speed 0.2)
(defstatus frightened)
(defstatus brave)
(defstatus immobilized)
