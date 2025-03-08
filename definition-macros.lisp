(defmacro defaction ((&rest keys) description &body body)
  (let ((key (gensym))
	(key-list (gensym)))
    `(let ((,key-list (ensure-list ',keys)))
       (loop for ,key in ,key-list
	     when (gethash ,key *action-descriptions*)
	       do (print-to-log "You're declaring the ~a action twice!" ,key)
	     do (setf (gethash ,key *actions*) (lambda () ,@body)))
       (setf (gethash (format nil "~{~c~#[~; or ~;, ~]~}" ,key-list)
		      *action-descriptions*)
	     ,description))))

;; initialize helper functions for macros
(labels ((build-slot (slt) ; creates slot information for new slots
	     (list (if (listp slt)
		       `(,(car slt) :accessor ,(car slt)
				    :initform ,(cadr slt)
				    :initarg ,(make-keyword (car slt)))
		       `(,slt :accessor ,slt
			      :initform nil
			      :initarg ,(make-keyword slt)))))
	   (reinit-slots (args slotlist &key (slotname nil slotnamep))
	     (if (> (length args) 0) ; cannot check for (car arg) because it might be nil
		 (if slotnamep
		     (reinit-slots (cdr args)
				   (cons (list (read-from-string slotname)
					       :initform (car args))
					 slotlist))
		     (reinit-slots (cdr args)
				   slotlist
				   :slotname (symbol-name (car args))))
		 slotlist)))
  ;; define new monster class and matching constructor function
  (defmacro defenemy (name display-char new-slots
		      &rest keys
		      &key (inherit 'enemy) equips
		      &allow-other-keys)
    ;; remove :inherit from key list to prevent odd behavior
    (remf keys :inherit)
    (remf keys :equips)
    `(progn
       ;; declare new monster class, including new keys and setting initform of
       ;; old values
       (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil)))
       ;; define constructor for new class
       (defun ,(constructor name) (pos)
	 (let ((new-enemy (make-instance ',name
					 :pos pos
					 :display-char ,display-char
					 :name ',name)))
	   (place new-enemy pos)
	   (mapc (lambda (i)
		   (equip i new-enemy))
		 (ensure-list ,equips))
	   (setf (slot-value new-enemy 'max-health)
		 (health new-enemy))
	   new-enemy))))

  ;; define class and constructor function for actor
  (defmacro defactor (name display-char new-slots
		      &rest keys
		      &key (inherit 'actor) ((:name name-override) nil name-overriden-p)
			(solidp t)
		      &allow-other-keys)
    (remf keys :inherit)
    (remf keys :name)
    `(progn (defclass ,name (,inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil)
					,(if name-overriden-p
					     `(name :initform ,name-override)
					     `(name :initform ',name))
					(display-char :initform ,display-char)))
	    (defun ,(constructor name) (pos)
	      (let ((new-actor (make-instance ',name)))
		(place new-actor pos :solid ,solidp)
		new-actor))))
  
  ;; define class, constructor function, and pickup generator function for equipment
  (defmacro defequipment (name new-slots
			  &rest keys
			  &key (inherit 'equipment)
			  &allow-other-keys)
    (remf keys :inherit)
    `(progn
       ;; define equipment class
       (defclass ,name (,inherit) (,@(mapcan #'build-slot new-slots)
				    ,@(reinit-slots keys nil)
				    (name :initform ',name)))
       ;; define constructor function
       (defun ,(constructor name) ()
	 (make-instance ',name))

       (defun ,(constructor name 'pickup) (pos)
	 (place (,(constructor name)) pos :solid nil)))))

(defmacro defstatus (name &key (duration 3) (speed 1))
  `(progn
     (defclass ,name (status)
       ((spd :initform ,speed)))
     (defun ,(constructor name 'status) (&key (duration ,duration) (name ',name))
       (make-instance ',name :duration duration :name name))))
