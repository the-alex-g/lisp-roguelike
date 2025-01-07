(defparameter *undead-layer* 3)
(defconstant +gems+ '(peridot sapphire ruby emerald agate amber amethyst aquamarine diamond quartz
		      chalcedony beryl pearl garnet bloodstone jade jasper jet lapis-lazuli
		      malachite moonstone obsidian opal onyx tourmaline spinel sunstone topaz
		      turquoise zircon serpentine))
;; https://www.gemsociety.org/gemstone-encyclopedia/

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

(defequipment potion () :consumable t :price (+ 7 (random 9)))
(defequipment bottle (contents) :breakable t :throw-distance 3)
(defmacro defpotion (name new-slots &rest slots)
  `(progn (define-secret-equipment (list *color-list* (list "potion"))
	    ,name ,new-slots :color (car chosen-variations)
	    :inherit potion :breakable t ,@slots)
	  (let ((old-function #',(constructor name)))
	    (setf (fdefinition (quote ,(constructor name)))
		  (lambda (&key without-bottle)
		    (if without-bottle
			(funcall old-function)
			(let ((bottle (make-bottle)))
			  (setf (contents bottle) (funcall old-function))
			  bottle)))))
	  ;; return the make-name-pickup function
	  #',(read-from-string (format nil "make-~a-pickup" (symbol-name name)))))

(defequipment ring () :price 20 :equip-slot 'ring :display-char #\o)
(defmacro resistance-ring (resistance)
  `(define-secret-equipment (list +gems+ (list "ring"))
       ,(read-from-string (concatenate 'string "ring-of-" (symbol-name resistance) "-resistance")) ()
     :inherit ring :resist (quote ,resistance)))
(defmacro stat-ring (name stat bonus)
  `(define-secret-equipment (list +gems+ (list "ring"))
       ,(read-from-string (concatenate 'string "ring-of-" (symbol-name name))) ()
     :inherit ring ,(make-keyword stat) ,bonus))

;;; DEFINE STATUSES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-snake-venom ()
  (make-status 3 :on-update (save 10 str target
				  (print-to-log "~a took ~d damage from poison"
						(name target)
						(damage target 1 :unblockable t
								 :damage-types '(poison)))
				  (destroy status))
		 :on-removed (print-to-log "~a recovered from poison" (name target))))
(defun make-spider-venom ()
  (make-status 6 :on-update (save 12 str target nil
				  (destroy status))
		 :on-applied (decf (str target) 2)
		 :on-removed (progn (print-to-log "~a recovered from poison" (name target))
				    (incf (str target) 2))))

;;; DEFINE ACTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defactor trap #\! ((atk '(1 4 piercing)) (save-dc 10) (discoverable t)
					  (one-use-p nil) (verb 'triggered)
					  (trigger-chance 50))
  :interact-action-only nil :solid nil :destructible nil :color 'red :hiddenp t)
(add-to-spawn
 'trap 'common "1+"
 (defactor spike-trap #\! () :inherit trap))
(add-to-spawn
 'trap 'common "1+"
 (defactor dart-trap #\> () :one-use-p t :save-dc 15 :inherit trap))
(defactor fire #\^ (burn-time (dmg 6)) :interact-action-only nil
  :solid nil :destructible nil
  :color 'red :dynamicp t)
(add-to-spawn
 'treasure 'uncommon "1+"
 (defactor glowing-mushroom-actor #\f ((glow-radius 2))
   :consumable t :name "glowing mushrooms"
   :solid nil :color 'sky-blue :dynamicp t))
(defactor acid-pool #\@ nil :hiddenp nil
  :verb "stepped in" :interact-action-only nil :one-use-p t :trigger-chance 100
  :atk '(1 4 acid) :save-dc 15 :color 'green :inherit trap)
(defactor webbing #\W (webbingp) :health (roll 6) :vulnerable 'fire)

;;; DEFINE EQUIPMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-shop 'common
	     (add-to-spawn
	      'treasure 'common "1+"
	      (defequipment food ((hunger (+ 20 (random 11))) (poisonp nil) (cookedp 0))
		:health (if (= (random 5) 0) 1 0) :consumable t
		:description "food"))
	     (list ;; fuel
	      (add-to-spawn
	       'treasure 'uncommon "1+"
	       (defequipment faggot () :burn-time (+ 10 (random 11)) :atk '(1 2 bludgeoning)
				       :weaponp t))
	      (defequipment coal () :burn-time (+ 20 (random 11))))
	     (list ;; weapons
	      (defequipment bow () :atk '(1 4 piercing ranged) :range 4 :burn-time 10
				   :inherit ranged-weapon :price 5)
	      (defequipment dagger nil :atk '(1 4 piercing) :inherit weapon :price 3)
	      (defequipment sword nil :atk '(1 6 slashing) :inherit weapon :price 5)
	      (defequipment greatsword nil :atk '(1 8 slashing) :inherit weapon :price 10))
	     (defequipment leather-armor nil :def 1 :equip-slot 'body :price 4))
(add-to-shop 'uncommon
	     (defequipment identify-scroll () :consumable t :price 10)
	     (defequipment chainmail nil :def 2 :equip-slot 'body :price 10)
	     (list ;; weapons
	      (defequipment +1-sword nil :atk '(1 6 1 magic slashing) :price 10 :inherit weapon)
	      (defequipment +1-bow nil :atk '(1 4 1 piercing magic ranged) :range 5
				       :inherit ranged-weapon :burn-time 10 :price 10)
	      (defequipment +1-greatsword nil :atk '(1 8 1 magic slashing) :inherit weapon
					      :price 20))
	     (list ;; potions
	      (defpotion explosive-potion ((explode-damage (+ (roll 4) (roll 4)))))
	      (defpotion healing-potion () :health (+ (roll 4) (roll 4)))
	      (defpotion poison-potion () :health (+ (roll 4) (roll 4))))
	     (list ;; rings
	      (resistance-ring fire)
	      (resistance-ring acid)
	      (resistance-ring lightning)
	      (stat-ring strength str 1)
	      (resistance-ring cold)))
(defequipment rusty-sword nil :atk '(1 6 -1 slashing) :inherit weapon)
(defequipment rat-meat () :hunger (+ 10 (random 6)) :secretp t :inherit food)
(defequipment poison-rat-meat () :poisonp t
  :health (roll 3) :hunger (+ 6 (random 4))
  :inherit rat-meat :fake-name "rat meat")
(defequipment ranged-weapon (range) :dex -2 :inherit weapon)
(add-to-spawn 'treasure 'common "1+"
	      (defherb healing-herb :health (roll 4))
	      (defherb poison-herb :health (roll 4)))
(defequipment glowing-mushrooms ()
  :throw-distance 3 :hunger (+ 8 (random 6)) :inherit food :burn-time (roll 10))
(defequipment gold ((amount (roll 6))))

;;; DEFINE MONSTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-spawn
 'monster 'common (layers 'below 0 'excluding *undead-layer*)
 (defenemy goblin #\g () :equips #'make-dagger :health (1+ (roll 3))
			 :str -1 :dex 1 :color 'green
			 :loot '((80 make-gold))
			 :xp 3))

(add-to-spawn
 'monster 'uncommon (layers 'below 0 'excluding *undead-layer*)
 (defenemy goblin-archer #\g () :equips #'make-bow :inherit goblin))
(add-to-spawn
 'monster 'common (layers 'below 0)
 (defenemy rat #\r () :atk '(1 2 piercing) :health (roll 2)
		      :dex 2 :color 'dark-red
		      :loot '((60 make-rat-meat)
			      (40 make-poison-rat-meat))))
(add-to-spawn
 'monster 'uncommon (layers 'below 1 'excluding *undead-layer*)
 (defenemy ogre #\O () :atk '(1 6 bludgeoning) :health (+ 4 (roll 2))
		       :str 2 :dex -2 :color 'orange :speed 1.75
		       :xp 8))
(add-to-spawn
 'monster 'rare (layers 'below 1)
 (defooze grey-slime () :atk '((1 4 acid) (1 4 acid)) :health (+ 4 (roll 4))
			:dex -2 :spd 2 :color 'grey :xp 4))
(add-to-spawn
 'monster 'common (layers 'on *undead-layer*)
 (defenemy skeleton #\s () :health (roll 6) :equips #'make-rusty-sword
			   :vulnerable '(bludgeoning holy) :xp 2)
 (defenemy zombie #\z () :health (+ 4 (roll 6)) :xp 4
			 :atk '(1 6 bludgeoning) :spd 2 :dex -2 :str 1
			 :vulnerable 'holy))
(add-to-spawn
 'monster 'rare (layers 'on *undead-layer*)
 (defenemy wraith #\W () :health (+ 2 (roll 4)) :xp 8 :atk '(1 6 1 necrotic)
			 :dex 2 :vulnerable 'holy
			 :resist '(piercing slashing bludgeoning)
			 :color 'grey))
(add-to-spawn
 'monster 'uncommon (layers 'below 0 'excluding *undead-layer*)
 (defenemy snake #\s () :health (roll 4) :xp 2
			:atk `(1 piercing :status ,(make-snake-venom))
			:dex 1 :color 'pale-green))
(defenemy spider #\s ((web-cooldown 0)))
(add-to-spawn
 'monster 'uncommon (layers 'below 1 'excluding *undead-layer*)
 (defenemy giant-spider #\S ()
   :health (+ 4 (roll 4)) :xp 4 :atk `(1 4 piercing :status ,(make-spider-venom))
   :dex 1 :def 1 :str 1 :inherit spider))
   
