;; EQUIPMENT

(defabstract secret-equipment potion () :char #\p :break-chance 75)
(defabstract equipment weapon () :weaponp t :break-chance 2)

(add-to-shop
 (defequipment sword () :atk '(1 6 0 0 slashing) :inherit weapon :char #\^)
 (defequipment sword-+1 () :atk '(1 6 1 1 slashing) :inherit weapon :break-chance 1 :char #\^)
 (defequipment dagger () :atk '(1 4 0 0 piercing) :inherit weapon :char #\^)
 (defequipment bow () :atk '(1 4 0 0 piercing) :inherit weapon :range 4 :char #\) :size 2
		      :burn-time 25)
 (defequipment food ((sustenance (roll 2 10 10)) (cooking 0)) :description "recovers 12-30 hunger"
   :break-chance 100 :char #\")
 (defequipment warclub () :atk '(2 6 0 0 bludgeoning) :inherit weapon :size 2
			  :char #\& :burn-time 50)
 (defequipment faggot () :atk '(1 3 0 0 bludeoning) :char #\& :burn-time 100)
 (defequipment quiver ((arrows 20)) :char #\q
				    :constructor ((&optional (amount 20))
						  (setf (arrows quiver) amount)))
 (defsecretequipment wand (unidentified-wand) ((spell (randnth *spells*))
					       (charges (roll 1 4)))
   :char #\/ :burn-time 5
   :constructor ((&rest spell-options)
		 (when spell-options
		   (setf (spell wand) (randnth spell-options)))))
 (defsecretequipment healing-potion
     ((blue-potion :color '(0 1 5))
      (green-potion :color 'green-4)
      (red-potion :color 'red-4))
   ((healing (roll 2 4))) :inherit potion))
(defequipment kobold-meat ()
  :sustenance (roll 1 10 5) :inherit food :description "recovers 6-15 hunger")
(defequipment goblin-meat ()
  :sustenance (roll 2 10 10) :inherit food :description "recovers 12-30 hunger")
(defequipment fist () :atk '(1 3 -1 0 bludgeoning) :inherit weapon :break-chance -100)
(defequipment gold (amount) :solidp nil :color 'yellow-4 :char #\*
  :constructor ((&optional (amount 1))
		(setf (amount gold) amount)))
(defequipment crude-bow () :inherit bow :atk '(1 3 0 -1 piercing) :break-chance 3)
(defequipment debt () :break-chance -100000)
(defequipment sprout-bomb () :char #\* :color '(4 2 5) :break-chance 100)

;; FURNITURE

(defabstract actor hazard ())
(defabstract hazard trap ((trigger-chance 100)
			 (find-dc 10)
			 (avoid-dc 10)
			 (searchedp nil))
  :color 'red :hiddenp t)
(defabstract actor remains ((reanimateablep t)))
(defactor corpse #\c (loot (decay-time 100)) :inherit remains)
(defactor bones #\x () :inherit remains)
(defactor ladder #\# (direction))
(defactor pit-trap #\! () :inherit trap)
(defactor fire #\& ((fuel 0)) :color '(5 3 0) :illumination 3 :inherit hazard)
(defactor table #\space () :bg-color '(2 1 0) :neighbors 8 :inherit furniture)
(defactor brazier #\u () :color '(5 4 0) :illumination 3
  :neighbors '(5 3) :inherit breakable :solidp t)

;; ENEMIES

(define-mask-set '(good evil goblin troll kobold sprout demon undead humanoid))
(let ((mask-living (mask-all '(undead demon))))
  (defabstract enemy undead ()
    :types '(undead)
    :morale 'fearless
    :allies '(undead)
    :enemies mask-living)
  
  (defenemy goblin #\g ()
    :types '(goblin evil humanoid)
    :allies '(goblin troll)
    :enemies '(good)
    :color 'green-4
    :health (roll 1 4 1)
    :equips (make-dagger)
    :dex 1
    :meat (make-goblin-meat)
    :loot `(((50 ,(make-gold 1)))))
  (defenemy goblin-archer #\g ((arrows 10))
    :inherit goblin
    :equips (make-crude-bow)
    :morale -0.5)
  (defenemy kobold #\k ()
    :types '(kobold evil humanoid)
    :allies '(kobold)
    :enemies '(good troll)
    :color '(4 1 0)
    :natural-weapons '(1 3 0 0 piercing)
    :health (roll 1 4 1)
    :equips (make-dagger)
    :str 1 :dex 1 :armor 1
    :meat (make-kobold-meat))
  (defenemy troll #\T ()
    :type '(troll evil)
    :enemies '(good)
    :color '(0 3 5)
    :health (roll 2 8)
    :equips (make-warclub)
    :str 2 :dex -1 :armor 1 :spd 3/4 :con 2
    :meat (roll 2 10 20))
  (defenemy sprout #\% ()
    :types '(sprout)
    :allies '(sprout)
    :enemies (mask-all '(sprout))
    :color '(1 5 0)
    :health (roll 1 4)
    :int -4
    :morale 'fearless
    :vulnerable '(fire slashing)
    :resist '(bludgeoning piercing)
    :natural-weapons '(1 1 0 0 slashing))
  (defenemy grenadier-sprout #\% ()
    :inherit sprout
    :color '(4 2 5))
  (defenemy sprout-hulk #\H ()
    :inherit sprout
    :health (roll 4 4 2)
    :natural-weapons '(1 8 0 0 bludgeoning)
    :str 2 :dex -1)
  (defenemy shopkeeper #\U ((domain 4) targets home)
    :types '(demon)
    :color 'purple-4
    :health (roll 3 10 5)
    :idle #'no-idle
    :str 3 :dex 2 :int 4 :wis 3 :cha 4
    :equips `(,(make-sword-+1) ,(make-sword-+1)))
  (defenemy zombie #\z ()
    :inherit undead
    :color '(2 3 2)
    :health (roll 1 8)
    :idle #'wander
    :str 1 :con 1 :dex -2 :int -4 :wis -4 :cha -4 :spd 3/5)
  (defenemy ghoul #\g ()
    :inherit undead
    :color '(4 5 4)
    :health (roll 1 6 2)
    :dex 1 :int -2 :cha -4 :spd 5/4)
  (defenemy skeleton #\s ()
    :inherit undead
    :color '(5 5 3)
    :health (roll 1 6)
    :vulnerable '(bludgeoning)
    :int -4 :wis -4 :cha -4)
  (defenemy necromancer #\h ()
    :types '(undead humanoid)
    :allies '(undead)
    :enemies mask-living
    :color '(1 2 3)
    :health (roll 1 8)
    :str -1 :int 3 :wis 3
    :mana-multiplier 2
    :primary-stat 'intl
    :morale -2
    :loot `(((50 ,(make-gold (roll 1 2))))
	    ((75 ,(make-wand *enervate* *animate-dead* *life-drain*))))))

;; STATUSES

(defstatus resting :duration -1 :speed 0.2)
(defstatus frightened)
(defstatus brave)
(defstatus immobilized)
(defstatus elevated :duration -1)
(defstatus weak)
(defstatus clumsy)

;; TERRAINS

(defterrain 'standard #\. :color 250)
(defterrain 'difficult #\. :cost 2 :color 'green-4)
