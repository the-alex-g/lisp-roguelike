(defparameter *colors* '((black 30)
			 (red 31)
			 (green 32)
			 (orange 33)
			 (blue 34)
			 (purple 35)
			 (teal 36)
			 (grey 90)
			 (dark-red 91)
			 (light-green 92)
			 (yellow 93)
			 (sky-blue 94)
			 (dark-purple 95)
			 (dark-teal 96)))

(defgeneric kill (obj)
  (:method (obj)))
(defgeneric drop-corpse (obj))

(defgeneric wallp (obj)
  (:method (obj) nil)
  (:method ((obj list)) (wallp (solid obj)))
  (:method ((obj symbol)) (eq obj 'wall))
  (:method ((obj character)) t))

(defgeneric playerp (obj)
  (:method ((obj player)) t)
  (:method (obj) nil))

(defgeneric hostilep (obj)
  (:method (obj) nil)
  (:method ((obj enemy)) t))

(defmethod name ((obj character)) "wall")
(defmethod name ((obj symbol)) obj)

(defgeneric color (obj)
  (:method ((obj number))
    (if (or (<= 30 obj 36)
	    (<= 90 obj 96))
	obj
	0))
  (:method ((obj symbol))
    (let ((color-set (assoc obj *colors*)))
      (if color-set
	  (cadr color-set)
	  0)))
  (:method ((obj actor))
    (color (slot-value obj 'color))))

(defgeneric display-char (obj)
  (:method ((obj actor))
    (if (eq (color obj) 30)
	(slot-value obj 'display-char)
	(apply-colors (slot-value obj 'display-char) (color obj))))
  (:method ((pos list))
    (if (and (wallp (vec+ pos +up+))
	     (wallp (vec+ pos +down+)))
	(setf (solid pos) #\|)
	(setf (solid pos) #\-)))
  (:method ((obj character)) obj))

(defmethod evasion ((obj creature))
  (max 1 (+ 5 (dex obj) (slot-value obj 'evasion))))

(defgeneric deadp (obj)
  (:method ((obj creature))
    (= (health obj) 0)))

(defmethod (setf health) (value (obj creature))
  (if (> value 0)
      (if (slot-boundp obj 'max-health)
	  (setf (slot-value obj 'health) (min value (max-health obj)))
	  (progn (setf (slot-value obj 'max-health) (health obj))
		 (setf (slot-value obj 'health) value)))
      (progn (setf (slot-value obj 'health) 0)
	     (kill obj))))

(defmethod (setf con) (value (obj creature))
  (let ((dhealth (- value (con obj))))
    (incf (slot-value obj 'max-health)
	  dhealth)
    (incf (health obj) dhealth)))

(defmethod (setf hunger) (value (obj player))
  (cond ((> value (max-hunger obj))
	 (setf (slot-value obj 'hunger) (max-hunger obj)))
	((< value 0)
	 (setf (slot-value obj 'hunger) 20)
	 (decf (health obj)))
	(t
	 (setf (slot-value obj 'hunger) value))))

(defmethod (setf illumination) (value (obj actor))
  (let ((current-value (illumination obj)))
    (when (and (<= current-value 0)
	       (> value 0))
      (add-glowing obj))
    (when (and (> current-value 0)
	       (<= value 0))
      (remove-glowing obj)))
  (setf (slot-value obj 'illumination) value))
  
(defgeneric death (obj)
  (:method ((obj creature))
    "killing it"))

(defgeneric resistp (obj type)
  (:method ((obj creature) type)
    (member type (resistances obj))))

(defgeneric immunep (obj type)
  (:method ((obj creature) type)
    (member type (immunities obj))))

(defgeneric vulnerablep (obj type)
  (:method ((obj creature) type)
    (member type (vulnerabilities obj))))

(defgeneric absorbp (obj type)
  (:method ((obj creature) type)
    (member type (absorbances obj))))

(defgeneric weapons (obj)
  (:method ((obj creature))
    (let ((held-items (gethash 'hand (equipment obj))))
      (cond ((= (length held-items) 1)
	     held-items)
	    ((= (length held-items) 0)
	     (list (make-fist)))
	    (t
	     (loop for equipment in held-items
		   when (weaponp equipment)
		     collect equipment))))))

(defgeneric description (obj)
  (:method ((obj container))
    (description (contents obj)))
  (:method ((obj equipment))
    (log-to-string "takes ~d ~a slots~%deals ~a as a weapon~:[~;~%~:*~a~]"
		   (size obj)
		   (equip-slot obj)
		   (damage-string (atk obj))
		   (slot-value obj 'description))))


(macrolet ((container-method (method)
	     `(defmethod ,method ((obj container))
		(,method (contents obj)))))
  (container-method price)
  (container-method atk)
  (container-method range)
  (container-method size)
  (container-method weaponp)
  (container-method equip-slot))

(defun contain (obj)
  (make-instance 'container :contents obj))
