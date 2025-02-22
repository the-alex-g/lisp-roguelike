;; THESE COULD BE CONSTANTS
(defparameter +directions+ '((0 . 1) (1 . 0) (-1 . 0) (0 . -1)))
(defparameter +zero+ '(0 . 0))
(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))

(defstruct attack dmg to-hit source types)
(defstruct weapon dmg (to-hit 0) damage-types)
(defclass actor ()
  ((display-char :initform #\? :initarg :display-char :writer (setf display-char))
   (pos :initform +zero+ :initarg :pos :accessor pos)
   (name :initform "" :initarg :name :accessor name)
   (color :initform 30 :initarg :color :accessor color)
   (hiddenp :initform nil :initarg :hiddenp :accessor hiddenp)
   (solidp :initform nil :accessor solidp :initarg :solidp)
   (opaquep :initform nil :accessor opaquep :initarg :opaquep)
   (persistently-visiblep :initform nil :initarg :pvisiblep :accessor persistently-visiblep)))
(defclass creature (actor)
  ((dex :initform 0 :initarg :dex :accessor dex)
   (str :initform 0 :initarg :str :accessor str)
   (health :initform 1 :initarg :health :reader health)
   (armor :initform 0 :initarg :armor :accessor armor)
   (evasion :initform 0 :initarg :evd :writer (setf evasion))
   max-health
   (resistances :initform '() :initarg :resist :accessor resistances)
   (immunities :initform '() :initarg :immune :accessor immunities)
   (vulnerablities :initform '() :initarg :vulnerable :accessor vulnerabilities)
   (absorbances :initform '() :initarg :absorb :accessor absorbances)))
(defclass wall (actor)
  ((solidp :initform t :allocation :class)
   (opaquep :initform t :allocation :class)
   (persistently-visiblep :initform 0)))

(defparameter *board* (make-hash-table :test #'equal))
(defparameter *board-size* '(10 . 10))
(defparameter *player* (make-instance 'creature :health 10 :name "player" :pos '(5 . 5) :color 31))
(defparameter *sight-distance* 10)

(defun apply-colors (char colors)
  (format nil "~c[~{~d~^;~}m~c~0@*~c[0;30m" #\esc (if (listp colors)
						      colors
						      (list colors))
	  char))

(defgeneric display-char (obj)
  (:method ((obj actor))
    (if (eq (color obj) 30)
	(slot-value obj 'display-char)
	(apply-colors (slot-value obj 'display-char) (color obj))))
  (:method ((obj wall))
    (when (eq (slot-value obj 'display-char) #\?)
      (if (or (wallp (gethash (vec+ (pos obj) +left+) *board*))
	      (wallp (gethash (vec+ (pos obj) +right+) *board*)))
	  (setf (display-char obj) #\-)
	  (setf (display-char obj) #\|)))
    (call-next-method)))

(defmethod (setf health) (value (obj creature))
  (if (> value 0)
      (if (slot-boundp obj 'max-health)
	  (setf (slot-value obj 'health) (min value (slot-value obj 'max-health)))
	  (progn (setf (slot-value obj 'max-health) (health obj))
		 (setf (slot-value obj 'health) value)))
      (setf (slot-value obj 'health) 0)))

(defmethod evasion ((obj creature))
  (+ 10 (dex obj) (slot-value obj 'evasion)))

(defgeneric deadp (obj)
  (:method ((obj creature))
    (= (health obj) 0)))

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

(defun print-to-log (string &rest args)
  (format t "~?~%" string args))

(defun roll (num die &rest modifiers)
  (+ (loop repeat num
	   sum (1+ (random die)))
     (loop for m in modifiers
	   sum m)))

(defun get-attack (attacker weapon)
  (make-attack :dmg (+ (apply #'roll (weapon-dmg weapon)) (str attacker) )
	       :to-hit (+ (random 20) 1 (weapon-to-hit weapon) (dex attacker))
	       :source (name attacker)
	       :types (weapon-damage-types weapon)))

(defun damage-modifier (defender damage-types)
  (labels ((calculate-damage-modifier (mod types)
	     (if types
		 (calculate-damage-modifier
		  (* mod (cond ((absorbp defender (car types)) -1)
			       ((immunep defender (car types)) 0)
			       ((resistp defender (car types)) 0.5)
			       ((vulnerablep defender (car types)) 2)
			       (t 1)))
		  (cdr types))
		 mod)))
    (calculate-damage-modifier 1 damage-types)))

(defun attack (attack defender)
  (if (>= (attack-to-hit attack) (evasion defender))
      (damage defender attack)
      (print-to-log "~a missed ~a" (attack-source attack) (name defender))))

(defun damage (defender attack)
  (let* ((base-damage (max 1 (- (attack-dmg attack) (armor defender))))
	 (mod-damage (round (* base-damage (damage-modifier defender (attack-types attack)))))
	 (real-damage (cond ((= mod-damage 0) 0)
			    ((< mod-damage 0) mod-damage)
			    (t (max 1 mod-damage)))))
    (decf (health defender) real-damage)
    (print-to-log "~a hit ~a for ~d damage~:[~;, ~a~]"
		  (attack-source attack)
		  (name defender)
		  (max 0 real-damage)
		  (deadp defender)
		  (death defender))))

(defun test-combat ()
  (let ((enemy (make-instance 'creature :name "enemy" :health 10))
	(attack1 (make-attack :dmg 5 :to-hit 10 :source "player" :types '(slashing)))
	(attack2 (make-attack :dmg 5 :to-hit 5 :source "player" :types '(slashing)))
	(attack3 (make-attack :dmg 10 :to-hit 10 :source "player" :types '(bludgeoning))))
    (flet ((my-attack (text atk val)
	     (print-to-log "~:@(~a~)" text)
	     (attack atk enemy)
	     (print-to-log "health remaining: ~d    ~:[FAIL~;PASS~]" (health enemy)
			   (= (health enemy) val))))
      (my-attack "basic attack" attack1 5)
      (setf (resistances enemy) '(slashing))
      (my-attack "resistances" attack1 3)
      (setf (absorbances enemy) '(slashing))
      (my-attack "absorbance" attack1 8)
      (my-attack "health overflow" attack1 10)
      (my-attack "missing" attack2 10)
      (my-attack "killing" attack3 0))))

(defun has-los (to from distance)
  (let ((dx (- (car to) (car from)))
	(dy (- (cdr to) (cdr from))))
    (labels ((get-pos-on-line (m)
	       (cons (round (+ (car from) (* m dx)))
		     (round (+ (cdr from) (* m dy)))))
	     (pos-opaquep (m)
	       (let* ((pos (get-pos-on-line m))
		      (actor (gethash pos *board*)))
		 (if actor
		     (opaquep actor)
		     nil))))
      (and (or (< distance 0)
	       (>= distance (+ (abs dx) (abs dy))))
	   (loop for x below (abs dx)
		 never (pos-opaquep (/ x (abs dx))))
	   (loop for y below (abs dy)
		 never (pos-opaquep (/ y (abs dy))))))))
			    

(defgeneric visiblep (obj)
  (:method ((pos list))
    (has-los pos (pos *player*) *sight-distance*))
  (:method ((obj actor))
    (cond ((hiddenp obj) nil)
	  ((persistently-visiblep obj)
	   (if (= (persistently-visiblep obj) 1)
	       t
	       (let ((result (visiblep (pos obj))))
		 (if result (setf (persistently-visiblep obj) 1))
		 result)))
	  (t (visiblep (pos obj))))))

(defgeneric wallp (obj)
  (:method (obj) nil)
  (:method ((obj wall)) t))

(defun vec+ (&rest vectors)
  (loop for v in vectors
	sum (car v) into x
	sum (cdr v) into y
	finally (return (cons x y))))

(defun vec- (vector &rest vectors)
  (if vectors
      (vec+ vector (loop for v in vectors
			 sum (- (car v)) into x
			 sum (- (cdr v)) into y
			 finally (return (cons x y))))
      (cons (- (car vector)) (- (cdr vector)))))

(defun print-board ()
  (loop for y below (cdr *board-size*)
	do (loop for x below (car *board-size*)
		 do (let* ((pos (cons x y))
			   (actor (gethash pos *board*)))
		      (format t "~a" (cond ((and actor (visiblep actor))
					    (display-char actor))
					   ((visiblep pos) #\.)
					   (t #\space)))))
	do (terpri)))

(loop for x below 10
      do (setf (gethash (cons x 0) *board*) (make-instance 'wall :pos (cons x 0)))
      do (setf (gethash (cons x 9) *board*) (make-instance 'wall :pos (cons x 9))))
(loop for y below 10
      do (setf (gethash (cons 0 y) *board*) (make-instance 'wall :pos (cons 0 y)))
      do (setf (gethash (cons 9 y) *board*) (make-instance 'wall :pos (cons 9 y))))

(setf (gethash (pos *player*) *board*) *player*)
