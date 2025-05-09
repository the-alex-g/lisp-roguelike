(defparameter *spells* nil)
(defparameter *identified-wands* nil)

(defmacro defspell (name cost requires-target-p &body body)
  (let ((spell (gensym)))
    `(let ((,spell (make-spell :requires-target-p ,requires-target-p
			       :name ',name
			       :function (lambda ,(if requires-target-p
						      '(caster target)
						      '(caster))
					   ,@body))))
       (push ,spell *spells*)
       (defparameter ,(read-from-string (format nil "*~a*" name)) ,spell)
       (defun ,name (caster &optional target)
	 (cast-spell ,spell caster :target target)))))

(defspell animate-dead 2 nil
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

(defspell life-drain 1 t
  (let* ((damage (damage target (make-damage :source caster
					     :amount (if (checkp #'con target (+ 10 (intl caster)))
							 (roll 1 2)
							 (roll 1 4))
					     :types '(necrotic))))
	 (previous-health (health caster))
	 (health-gain (- (incf (health caster) damage) previous-health)))
    (print-if-visible caster target
		      ("~a fires a beam of red energy at ~a, dealing ~d damage~
                        ~:[~;~%you regain ~d health~]"
		       (name caster) (name target) damage (playerp caster) health-gain)
		      ("~a fires a beam of red energy" (name caster))
		      ("~a is struck by a beam of red energy, taking ~d damage"
		       (name target) damage))
    damage))

(defspell enervate 1 t
  (let ((damage (damage target
			(make-damage :source caster
				     :amount (roll 1 4)
				     :types '(necrotic)
				     :statuses (unless (checkp #'con target (+ 10 (intl caster)))
						 (list (if (= (random 2) 0)
							   (make-weak-status :duration 2)
							   (make-clumsy-status :duration 2))))))))
    (print-if-visible caster target
		      ("~a fires a beam of dark energy at ~a, dealing ~d damage"
		       (name caster) (name target) damage)
		      ("~a fires a beam of dark energy" (name caster))
		      ("~a is struck by a beam of dark energy, taking ~d damage"
		       (name target) damage))
    (if (equal target *player*)
	(print-to-log "you feel unwell..."))
    damage))
