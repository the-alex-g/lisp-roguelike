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
       (defun ,name (caster) (cast-spell ,spell caster)))))

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
			  (length visible-dead)))))))
