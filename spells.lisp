(defparameter *spells* nil)

(defmacro defspell (name requires-target-p &body body)
  (let ((spell (gensym)))
    `(let ((,spell (make-spell :requires-target-p ,requires-target-p
			       :name ',name
			       :function (lambda ,(if requires-target-p
						      '(caster target)
						      '(caster))
					   ,@body))))
       (push ,spell *spells*)
       (defparameter ,(read-from-string (format nil "*~a*" name)) ,spell)
       (defun ,name (caster &optional target) (cast-spell ,spell caster :target target)))))

(defspell animate-dead nil
  (let ((animated-dead (get-actors-in-los-of caster nil t 3 (reanimate actor caster))))
    (if (visiblep caster *player*)
	(print-to-log "~:[there is no effect~;~
                        beams of dark energy lance out from ~a, animating ~d undead minions~]"
		      animated-dead
		      (name caster)
		      (length animated-dead))
	(let ((visible-dead (loop for undead in animated-dead
				  when (visiblep undead *player*) collect it)))
	  (when visible-dead
	    (print-to-log "beams of dark energy strike ~d corpses, animating them as undead minions"
			  (length visible-dead)))))
    animated-dead))

(defspell life-drain t
  (let* ((damage (damage target (make-damage :source caster
					     :amount (roll 1 4)
					     :types '(necrotic))))
	 (previous-health (health caster))
	 (health-gain (- (incf (health caster) damage) previous-health)))
    (cond ((equal caster *player*)
	   (print-to-log "you fire a beam of red energy at ~a, dealing ~d damage~%~
                          you regain ~d health"
			 (name target) damage health-gain))
	  ((visiblep caster *player*)
	   (print-to-log "~a fires a beam of red energy~:[~; at ~a, dealing ~d damage~]"
			 (name caster)
			 (visiblep target *player*)
			 (name target)
			 damage))
	  ((visiblep target *player*)
	   (print-to-log "~a is struck by a beam of red energy, taking ~d damage"
			 (name target)
			 damage)))
    damage))

(defspell enervate t
  (let ((damage (damage target
			(make-damage :source caster
				     :amount (roll 1 4)
				     :types '(necrotic)
				     :statuses (list (if (= (random 2) 0)
							 (make-weak-status :duration 2)
							 (make-clumsy-status :duration 2)))))))
    (cond ((visiblep caster *player*)
	   (print-to-log "~a fires a beam of dark energy~:[~; at ~a, dealing ~d damage~]"
			 (name caster) (visiblep target *player*) (name target) damage))
	  ((visiblep target *player*)
	   (print-to-log "~a is struck by a beam of dark energy, taking ~d damage"
			 (name target) damage)))
    (if (equal target *player*)
	(print-to-log "you feel unwell..."))
    damage))
