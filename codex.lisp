
;;; DEFINE MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defequipment herb ((hunger (roll 5))) :consumable t :burn-time 1)
(defmacro defherb (real-name &rest slots)
  (let* ((herb-config (randnth '((herb #\v) (lichen #\_) (fungus #\f)
				 (wort #\w) (cress #\%))))
	 (herb-type (car herb-config))
	 (herb-char (cadr herb-config))
	 (herb-color (random-color)))
    `(defequipment ,real-name nil ,@slots
       :color (quote ,herb-color) :display-char ,herb-char
       :fake-name ,(log-to-string "~a ~a" herb-color herb-type)
       :inherit herb :identifiedp nil)))

(defenemy ooze #\o ((split-damage-types '(slashing)) (build-function)))
(defmacro defooze (name &rest args)
  `(defenemy ,name #\o ,@args
     :build-function (quote ,(read-from-string
			      (concatenate 'string "make-" (symbol-name name))))
     :inherit ooze))

;;; DEFINE ACTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defactor trap #\! ((atk '(1 4 piercing)) (save-dc 10) (discoverable t)
					  (one-use-p nil) (verb 'triggered)
					  (trigger-chance 50))
  :interact-action-only nil :solid nil :destructible nil :color 'red :hiddenp t
  :description "a cunning trap")
(defactor fire #\^ (burn-time (dmg 6)) :interact-action-only nil
  :solid nil :destructible nil
  :color 'red :description "a fire" :dynamicp t)
(defactor acid-pool #\@ nil :hiddenp nil :description "a pool of acid"
  :verb "stepped in" :interact-action-only nil :one-use-p t :trigger-chance 100
  :atk '(1 4 acid) :save-dc 15 :color 'green :inherit trap)

;;; DEFINE EQUIPMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defequipment food ((hunger (+ 20 (random 11))) (poisonp nil) (cookedp 0))
  :health (if (= (random 5) 0) 1 0) :consumable t
  :description "food")
(defequipment rat-meat () :hunger (+ 10 (random 6)) :secretp t
  :inherit food :description "rat meat")
(defequipment poison-rat-meat () :poisonp t
  :health (roll 3) :hunger (+ 6 (random 4))
  :inherit rat-meat :fake-name "rat meat")
(let ((bomb-color (random-color)))
  (defequipment bomb ((explode-damage (+ (roll 4) (roll 4))))
    :identifiedp nil :fake-name (log-to-string "~a potion" bomb-color)
    :throw-distance 3 :breakable t))
(defequipment faggot () :burn-time (+ 10 (random 11)) :atk '(1 2 bludgeoning)
  :weaponp t)
(defequipment coal () :burn-time (+ 20 (random 11)))
(defherb healing-herb :health (roll 4))
(defherb poison-herb :health (roll 4))
(defequipment ranged-weapon (range) :dex -2 :inherit weapon)
(defequipment bow () :atk '(1 4 piercing) :range 4 :burn-time 10
  :description "a bow" :inherit ranged-weapon)
(defequipment sword nil :atk '(1 6 slashing)
  :description "a sword" :inherit weapon)
(defequipment big-sword nil :atk '(1 8 slashing)
  :description "a big sword" :inherit weapon)
(defequipment leather-armor nil :def 1
  :description "leather armor" :equip-slot 'body)

;;; DEFINE MONSTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defenemy goblin #\g () :atk '(1 4 piercing) :health (1+ (roll 3))
  :str -1 :dex 1 :color 'green
  :xp 3 :description "a goblin with a sharp dagger")
(defenemy rat #\r () :atk '(1 2 piercing) :health (roll 2)
  :dex 2 :color 'dark-red
  :loot '((60 make-rat-meat)
	  (40 make-poison-rat-meat))
  :description "a giant rat")
(defenemy ogre #\O () :atk '(1 6 bludgeoning) :health (+ 4 (roll 2))
  :str 2 :dex -2 :color 'orange :speed 1.75
  :xp 8 :description "a hulking ogre")
(defooze grey-slime () :atk '((1 4 acid) (1 4 acid)) :health (+ 4 (roll 4))
  :dex -2 :spd 2 :color 'grey :xp 4 :description "a pool of grey slime")
