(defparameter *undead-layer* 3)

;;; DEFINE MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-random-name (variations &body body)
  `(let* ((chosen-variations (mapcar #'randnth ,variations))
	  (chosen-name (log-to-string "~{~a~^ ~}" chosen-variations)))
     ,@body))

(defmacro define-secret-equipment (variations name new-slots &rest slots)
  `(with-random-name ,variations
     (defequipment ,name ,new-slots
       :fake-name chosen-name :identifiedp nil ,@slots)))

(defequipment herb ((hunger (roll 5))) :consumable t :burn-time 1)
(defmacro defherb (name &rest slots)
  `(define-secret-equipment (list *color-list* '(herb lichen fungus wort cress))
     ,name nil
     :inherit herb
     :color (car chosen-variations)
     :display-char (cadr (assoc (cadr chosen-variations)
				'((herb #\v) (lichen #\_) (fungus #\f)
				  (wort #\w) (cress #\%))))
     ,@slots))

(defenemy ooze #\o ((split-damage-types '(slashing)) (build-function)))
(defmacro defooze (name &rest args)
  `(defenemy ,name #\o ,@args
     :build-function (quote ,(read-from-string
			      (concatenate 'string "make-" (symbol-name name))))
     :inherit ooze))

(defequipment potion () :consumable t)
(defequipment bottle (contents) :breakable t :throw-distance 3)
(defmacro defpotion (name new-slots &rest slots)
  `(progn (define-secret-equipment (list *color-list* (list "potion"))
	    ,name ,new-slots
	    :inherit potion :breakable t ,@slots)
	  (let ((old-function #',(constructor name)))
	    (setf (fdefinition (quote ,(constructor name)))
		  (lambda (&key without-bottle)
		    (if without-bottle
			(funcall old-function)
			(let ((bottle (make-bottle)))
			  (setf (contents bottle) (funcall old-function))
			  bottle)))))))

;;; DEFINE ACTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-spawn
 'trap 'common "1+"
 (defactor trap #\! ((atk '(1 4 piercing)) (save-dc 10) (discoverable t)
					   (one-use-p nil) (verb 'triggered)
					   (trigger-chance 50))
  :interact-action-only nil :solid nil :destructible nil :color 'red :hiddenp t
  :description "a cunning trap"))
(defactor fire #\^ (burn-time (dmg 6)) :interact-action-only nil
  :solid nil :destructible nil
  :color 'red :description "a fire" :dynamicp t)
(add-to-spawn
 'treasure 'uncommon "1+"
 (defactor glowing-mushroom-actor #\f ((glow-radius 2))
   :consumable t :secretp t :fake-name "glowing mushrooms"
   :solid nil :color 'sky-blue :description "glowing mushrooms" :dynamicp t))
(defactor acid-pool #\@ nil :hiddenp nil :description "a pool of acid"
  :verb "stepped in" :interact-action-only nil :one-use-p t :trigger-chance 100
  :atk '(1 4 acid) :save-dc 15 :color 'green :inherit trap)

;;; DEFINE EQUIPMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-spawn
 'treasure 'common "1+"
 (defequipment food ((hunger (+ 20 (random 11))) (poisonp nil) (cookedp 0))
   :health (if (= (random 5) 0) 1 0) :consumable t
   :description "food"))
(defequipment rat-meat () :hunger (+ 10 (random 6)) :secretp t
  :inherit food :description "rat meat")
(defequipment poison-rat-meat () :poisonp t
  :health (roll 3) :hunger (+ 6 (random 4))
  :inherit rat-meat :fake-name "rat meat")
(defequipment glowing-mushrooms ()
  :throw-distance 3 :hunger (+ 8 (random 6)) :inherit food
  :description "a clump of glowing mushrooms" :burn-time (roll 10))
(defpotion explosive-potion ((explode-damage (+ (roll 4) (roll 4)))))
(defpotion healing-potion () :health (+ (roll 4) (roll 4)))
(defpotion poison-potion () :health (+ (roll 4) (roll 4)))
(add-to-spawn
 'treasure 'uncommon "1+"
 (defequipment faggot () :burn-time (+ 10 (random 11)) :atk '(1 2 bludgeoning)
  :weaponp t))
(defequipment coal () :burn-time (+ 20 (random 11)))
(add-to-spawn 'treasure 'common "1+"
	      (defherb healing-herb :health (roll 4))
	      (defherb poison-herb :health (roll 4)))
(defequipment ranged-weapon (range) :dex -2 :inherit weapon)
(defequipment bow () :atk '(1 4 piercing ranged) :range 4 :burn-time 10
  :description "a bow" :inherit ranged-weapon)
(defequipment dagger nil :atk '(1 4 piercing) :description "a dagger"
  :inherit weapon)
(defequipment sword nil :atk '(1 6 slashing)
  :description "a sword" :inherit weapon)
(defequipment big-sword nil :atk '(1 8 slashing)
  :description "a big sword" :inherit weapon)
(defequipment rusty-sword nil :atk '(1 6 -1 slashing)
  :description "a rusty sword" :inherit weapon)
(defequipment leather-armor nil :def 1
  :description "leather armor" :equip-slot 'body)

;;; DEFINE MONSTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-spawn
 'monster 'common (layers 'below 0 'excluding *undead-layer*)
 (defenemy goblin #\g () :equips #'make-dagger :health (1+ (roll 3))
			 :str -1 :dex 1 :color 'green
			 :xp 3 :description "a goblin with a sharp dagger"))

(add-to-spawn
 'monster 'uncommon (layers 'below 0 'excluding *undead-layer*)
 (defenemy goblin-archer #\g () :equips #'make-bow :inherit goblin
   :description "a goblin with a bow"))
(add-to-spawn
 'monster 'common (layers 'below 0)
 (defenemy rat #\r () :atk '(1 2 piercing) :health (roll 2)
		      :dex 2 :color 'dark-red
		      :loot '((60 make-rat-meat)
			      (40 make-poison-rat-meat))
		      :description "a giant rat"))
(add-to-spawn
 'monster 'uncommon (layers 'below 1 'excluding *undead-layer*)
 (defenemy ogre #\O () :atk '(1 6 bludgeoning) :health (+ 4 (roll 2))
		       :str 2 :dex -2 :color 'orange :speed 1.75
		       :xp 8 :description "a hulking ogre"))
(add-to-spawn
 'monster 'rare (layers 'below 1)
 (defooze grey-slime () :atk '((1 4 acid) (1 4 acid)) :health (+ 4 (roll 4))
			:dex -2 :spd 2 :color 'grey :xp 4
			:description "a pool of grey slime"))
(add-to-spawn
 'monster 'common (layers 'on *undead-layer*)
 (defenemy skeleton #\s () :health (roll 6) :equips #'make-rusty-sword
			   :vulnerable '(bludgeoning holy) :xp 2
			   :description "a skeleton with a rusty sword"))
(add-to-spawn
 'monster 'common (layers 'on *undead-layer*)
 (defenemy zombie #\z () :health (+ 4 (roll 6)) :xp 4
			 :atk '(1 6 bludgeoning) :spd 2 :dex -2 :str 1
			 :vulnerable 'holy :description "a rotting zombie"))
(add-to-spawn
 'monster 'rare (layers 'on *undead-layer*)
 (defenemy wraith #\W () :health (+ 2 (roll 4)) :xp 8 :atk '(1 6 1 necrotic)
			 :dex 2 :vulnerable 'holy :description "a wraith"
			 :resist '(piercing slashing bludgeoning)
			 :color 'grey))
