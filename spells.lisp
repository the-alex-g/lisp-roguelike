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
  (loop-in-circle 3
		  when (has-los (pos caster) (vec+ (cons x y) (pos caster)))
		  do (reanimate (contents (vec+ (cons x y) (pos caster))) caster)))
