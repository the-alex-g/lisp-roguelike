(defparameter *spells* nil)
(defparameter *identified-wands* nil)

(defmacro defspell (name (cost requires-target-p) &body body)
  (let ((spell (gensym)))
    `(let ((,spell (make-spell :requires-target-p ,requires-target-p
			       :name ',name
			       :cost ,cost
			       :function (lambda (caster ,@(if requires-target-p
							       '(target spell-die)
							       '(spell-die)))
					   (let ((spell-damage (roll* spell-die 5 :base 1))
						 (spell-duration (roll 1 spell-die (knl+ caster))))
					     ,@body)))))
       (push ,spell *spells*)
       (defparameter ,(read-from-string (format nil "*~a*" name)) ,spell)
       (defun ,name (caster ,@(if requires-target-p '(target &key die) '(&key die)) succeedp)
	 (cast-spell ,spell caster ,@(if requires-target-p
					 '(:target target :die die)
					 '(:die die))
		     :succeedp succeedp)))))

(defun spell-die (index)
  (cond ((< index 0) 4)
	((> index 4) 12)
	(t (+ 4 (* 2 index)))))

(defun best-spell-die (bonus)
  (cond ((<= bonus 0) 4)
	((<= bonus 1) 6)
	((<= bonus 3) 8)
	((<= bonus 6) 10)
	(t 12)))

(defun spell-fail (obj roll)
  (when (playerp obj)
    (print-to-log "your spell fails")))

(defspell animate-dead (2 nil)
  (let ((animated-dead (get-actors-in-los-of caster nil t 3 (reanimate actor caster))))
    (if (visiblep caster *player*)
	(print-to-log "~[~;there is no effect~;~
                        beams of dark energy lance out from ~a, animating ~d undead minions~]"
		      (cond (animated-dead 2) ((playerp caster) 1) (t 0))
		      (name caster)
		      (length animated-dead))
	(let ((visible-dead (loop for undead in animated-dead
				  when (visiblep undead *player*) collect undead)))
	  (when visible-dead
	    (print-to-log "beams of dark energy strike ~d corpses, animating them as undead minions"
			  (length visible-dead)))))
    animated-dead))

(defspell life-drain (0 t)
  (let* ((damage (if (checkp #'con+ target (+ spell-die (knl+ caster) 2))
		     (damage target (make-damage :source caster
						 :amount (max 1 (1- spell-damage))
						 :types '(necrotic))
			     nil)
		     0))
	 (previous-health (health caster))
	 (health-gain (- (incf (health caster)) previous-health)))
    (print-if-visible caster target
		      ("~a fires a beam of red energy at ~a, dealing ~d damage~
                        ~:[~;~%you regain ~d health~]"
		       (name caster) (name target) damage (playerp caster) health-gain)
		      ("~a fires a beam of red energy" (name caster))
		      ("~a is struck by a beam of red energy, taking ~d damage"
		       (name target) damage))
    damage))

(defspell enervate (0 t)
  (let ((damage (damage target
			(make-damage :source caster
				     :amount spell-damage
				     :types '(necrotic)
				     :statuses (unless (checkp #'con+ target spell-die)
						 (list (make-drain-status
							:ability (if (= (random 2) 0)
								     'str+
								     'dex+))))))))
    (print-if-visible caster target
		      ("~a fires a beam of dark energy at ~a, dealing ~d damage"
		       (name caster) (name target) damage)
		      ("~a fires a beam of dark energy" (name caster))
		      ("~a is struck by a beam of dark energy, taking ~d damage"
		       (name target) damage))
    (if (equal target *player*)
	(print-to-log "you feel unwell..."))
    damage))
