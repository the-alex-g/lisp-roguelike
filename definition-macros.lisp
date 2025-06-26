;; initialize helper functions for macros
(labels ((build-slot (slt) ; creates slot information for new slots
	     (list (if (listp slt)
		       `(,(car slt) :accessor ,(car slt)
				    :initform ,(cadr slt)
				    :initarg ,(make-keyword (car slt)))
		       `(,slt :accessor ,slt
			      :initform nil
			      :initarg ,(make-keyword slt)))))
	   (reinit-slots (args slotlist allocate-class &key (slotname nil slotnamep))
	     (if (> (length args) 0) ; cannot check for (car arg) because it might be nil
		 (if slotnamep
		     (reinit-slots (cdr args)
				   (let ((slot-symbol (read-from-string slotname)))
				     (cons (if (member slot-symbol allocate-class)
					       (list slot-symbol
						     :initform (car args)
						     :allocation :class)
					       (list slot-symbol
						     :initform (car args)))
					 slotlist))
				   allocate-class)
		     (reinit-slots (cdr args)
				   slotlist
				   allocate-class
				   :slotname (symbol-name (car args))))
		 slotlist))
	 (replace-keyword (existing new list)
	   (when (getf list existing)
	     (setf (getf list new) (getf list existing))
	     (remf list existing))
	   list))
  
  ;; define new monster class and matching constructor function
  (defmacro defenemy (name display-char new-slots
		      &rest keys
		      &key (inherit 'enemy) equips
		      &allow-other-keys)
    ;; remove unwanted keys from key list to prevent odd behavior
    (remf keys :inherit)
    (remf keys :equips)
    (setf keys (replace-keyword :evd :evasion keys))
    (setf keys (replace-keyword :idle :idle-behavior keys))
    (setf keys (replace-keyword :align :alignment keys))
    (let ((stats (loop for key in '(str dex con knl per cha det spd)
		       if (getf keys key)
			 collect (let* ((keyword (make-keyword key))
					(value (getf keys keyword)))
				   (remf keys keyword)
				   (if (listp value)
				       (list key value)
				       (list key (cons value 6))))
		       else if (eq key 'spd)
			      collect (list key (cons 1 6))
		       else
			 collect (list key (cons 0 6)))))
      `(progn
	 ;; declare new monster class, including new keys and setting initform of
	 ;; old values
	 (defclass ,name ,(list inherit) (,@(mapcan #'build-slot new-slots)
					  (stats :initform ',stats)
					  (name :initform ',name)
					  (display-char :initform ,display-char)
					  ,@(reinit-slots keys nil nil)))
	 ;; define constructor for new class
	 (defgeneric ,(constructor name) (pos)
	   (:method (pos)
	     (let ((new-enemy (make-instance ',name :pos pos)))
	       (place new-enemy pos)
	       (mapc (lambda (i)
		       (equip i new-enemy))
		     (ensure-list ,equips))
	       (setf (slot-value new-enemy 'max-health)
		     (health new-enemy))
	       new-enemy))))))

  ;; define class and constructor function for actor
  (defmacro defactor (name display-char new-slots
		      &rest keys
		      &key (inherit 'actor) ((:name name-override) nil name-overriden-p)
			(solidp nil) (neighbors 0)
		      &allow-other-keys)
    (remf keys :inherit)
    (remf keys :solidp)
    (remf keys :name)
    (remf keys :neighbors)
    `(progn (setf (gethash ',(constructor name) *neighbors-required*) ,neighbors)
	    (defclass ,name (,inherit) (,@(mapcan #'build-slot new-slots)
					,@(reinit-slots keys nil nil)
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
			  &key
			    (inherit 'equipment inheritp)
			    (allocate-class nil allocate-class-p)
			    (constructor nil constructorp)
			    (char #\? charp)
			  &allow-other-keys)
    (when inheritp
      (remf keys :inherit))
    (when allocate-class-p
      (remf keys :allocate-class))
    (when constructorp
      (remf keys :constructor))
    (when charp
      (remf keys :char)
      (setf (getf keys :display-char) char))
    (setf keys (replace-keyword :slot :equip-slot keys))
    `(progn
       ;; define equipment class
       (defclass ,name (,inherit) (,@(mapcan #'build-slot new-slots)
				    ,@(reinit-slots keys nil allocate-class)
				    (name :initform ',name)))
       ;; define constructor function
       (defun ,(constructor name) ,(when constructor (car constructor))
	 ,(if constructor
	      `(let ((,name (make-instance ',name)))
		 ,@(cdr constructor)
		 ,name)
	      `(make-instance ',name)))

       (defun ,(constructor name 'pickup) (pos)
	 (place (,(constructor name)) pos :solid nil))))
  

  (defmacro defabstract (inherit name new-slots &rest keys &key (char #\? charp) &allow-other-keys)
    (when charp
      (remf keys :char)
      (setf (getf keys :display-char) char))
    `(defclass ,name (,inherit)
       (,@(mapcan #'build-slot new-slots)
	,@(reinit-slots keys nil nil))))
  
  (defmacro defstatus (name new-slots &key (duration 3) (speed 1) (inherit 'status))
    `(progn
       (defclass ,name (,inherit)
	 ((spd+ :initform ,speed)
	  (duration :initform ,duration)
	  (name :initform ',name)
	  ,@(mapcan #'build-slot new-slots)))
       (defun ,(constructor name 'status) (&rest keys &key &allow-other-keys)
	 (apply #'make-instance ',name keys)))))

(defmacro defsecretequipment (name cover-names new-slots
			      &rest keys
			      &key (inherit 'secret-equipment)
			      &allow-other-keys)
  (remf keys :inherit)
  (let* ((cover-name (randnth cover-names))
	 (cover-name-slots (if (listp cover-name)
			       (cdr cover-name))))
    (loop for name-slot in cover-name-slots
	  when (keywordp name-slot)
	    do (remf keys name-slot))
    `(defequipment ,name ,new-slots ,@keys :inherit ,inherit 
       :cover-name ',(if (listp cover-name)
			 (car cover-name)
			 cover-name)
       :identifiedp nil
       :allocate-class '(identifiedp)
       ,@cover-name-slots)))

(defmacro defconsume (action &optional (item-type 'equipment))
  (let ((dont-remove-key (gensym)))
    `(defgeneric ,action (item actor)
       (:method (item (actor player))
	 (declare (ignore item actor))
	 (print-to-log "you can't ~a that" ',action)
	 ',dont-remove-key)
     (:method :around ((item ,item-type) (actor player))
       (if (shopkeeper item)
	   (print-to-log "you must buy that before ~aing it" ',action)
	   (unless (eq (call-next-method) ',dont-remove-key)
	     (remove-from-inventory item)))))))

(defmacro defaction ((&rest keys) time description &body body)
  (let ((key (gensym))
	(key-list (gensym)))
    `(let ((,key-list (ensure-list ',keys)))
       (loop for ,key in ,key-list
	     when (gethash ,key *action-descriptions*)
	       do (print-to-log "You're declaring the ~a action twice!" ,key)
	     do (setf (gethash ,key *actions*)
		      (lambda ()
			(let ((.time. ,time))
			  ,@body
			  .time.))))
       (setf (gethash (format nil "~{~c~#[~; or ~;, ~]~}" ,key-list)
		      *action-descriptions*)
	     ,description))))
