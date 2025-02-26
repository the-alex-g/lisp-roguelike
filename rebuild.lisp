(load "~/quicklisp/setup.lisp")
(ql:quickload :trivial-raw-io)

(load "utils.lisp")

(defstruct attack dmg to-hit source types)
(defstruct weapon dmg (to-hit 0) damage-types)
(defclass actor ()
  ((display-char :initform #\? :initarg :display-char :writer (setf display-char))
   (pos :initform +zero+ :initarg :pos :accessor pos)
   (name :initform "" :initarg :name :accessor name)
   (color :initform 30 :initarg :color :accessor color)
   (hiddenp :initform nil :initarg :hiddenp :accessor hiddenp)))
(defclass creature (actor)
  ((dex :initform 0 :initarg :dex :accessor dex)
   (str :initform 0 :initarg :str :accessor str)
   (spd :initform 1 :initarg :spd :accessor spd)
   (health :initform 1 :initarg :health :reader health)
   (armor :initform 0 :initarg :armor :accessor armor)
   (evasion :initform 0 :initarg :evd :writer (setf evasion))
   max-health
   (weapon :initform nil :initarg :weapon :accessor weapon)
   (resistances :initform '() :initarg :resist :accessor resistances)
   (immunities :initform '() :initarg :immune :accessor immunities)
   (vulnerablities :initform '() :initarg :vulnerable :accessor vulnerabilities)
   (absorbances :initform '() :initarg :absorb :accessor absorbances)))
(defclass enemy (creature)
  ((energy :initform 0 :accessor energy)))
(defclass equipment (actor) ())

(load "terminal.lisp")
(load "inventory.lisp")

(defparameter *solid-actors* (make-hash-table :test #'equal))
(defparameter *non-solid-actors* (make-hash-table :test #'equal))
(defparameter *board-size* '(10 . 10))
(defparameter *player* (make-instance 'creature :health 10 :name "player" :pos '(5 . 5) :color 31))
(defparameter *sight-distance* 10)
(defparameter *actions* (make-hash-table))
(defparameter *action-descriptions* (make-hash-table))
(defparameter *print-surroundings-mode* 'all)

(defmacro defaction (key description &body body)
  `(progn (setf (gethash ,key *action-descriptions*) ,description)
	  (setf (gethash ,key *actions*) (lambda ()
					   ,@body))))
(defun solid (pos)
  (gethash pos *solid-actors*))

(defun (setf solid) (value pos)
  (setf (gethash pos *solid-actors*) value))

(defun remove-solid (pos)
  (remhash pos *solid-actors*))

(defun non-solid (pos)
  (gethash pos *non-solid-actors*))

(defun (setf non-solid) (value pos)
  (setf (gethash pos *non-solid-actors*) value))

(defun remove-non-solid (pos)
  (remhash pos *non-solid-actors*))

(defun contents (pos)
  (or (solid pos) (non-solid pos)))

(defun emptyp (pos)
  (not (contents pos)))

(defun remove-all (pos)
  (remove-non-solid pos)
  (remove-solid pos))

(defgeneric wallp (obj)
  (:method (obj) nil)
  (:method ((obj symbol)) (eq obj 'wall))
  (:method ((obj character)) t))

(defmethod name ((obj character)) "wall")

(defun apply-default-colors ()
  (format t "~c[40;37m" #\esc))

(defun apply-colors (char colors)
  (format nil "~c[~{~d~^;~}m~c~0@*~c[40;37m"
	  #\esc (if (listp colors)
		    colors
		    (list colors))
	  char))

(defgeneric display-char (obj)
  (:method ((obj actor))
    (if (eq (color obj) 30)
	(slot-value obj 'display-char)
	(apply-colors (slot-value obj 'display-char) (color obj))))
  (:method ((pos list))
    (if (or (wallp (solid (vec+ pos +left+)))
	    (wallp (solid (vec+ pos +right+))))
	(setf (solid pos) #\-)
	(setf (solid pos) #\|)))
  (:method ((obj character)) obj))

(defgeneric corpse (obj)
  (:method ((obj creature))
    (make-instance 'actor :name (log-to-string "~a corpse" (name obj)) :display-char #\c)))

(defgeneric kill (obj)
  (:method ((obj enemy))
    (remove-solid (pos obj))
    (place (corpse obj) (pos obj) :solid nil)))

(defmethod (setf health) (value (obj creature))
  (if (> value 0)
      (if (slot-boundp obj 'max-health)
	  (setf (slot-value obj 'health) (min value (slot-value obj 'max-health)))
	  (progn (setf (slot-value obj 'max-health) (health obj))
		 (setf (slot-value obj 'health) value)))
      (progn (setf (slot-value obj 'health) 0)
	     (kill obj))))

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

(defun attack (attack defender)
  (if (>= (attack-to-hit attack) (evasion defender))
      (damage defender attack)
      (print-to-log "~a missed ~a" (attack-source attack) (name defender))))

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

(defun test-vector-math ()
  (flet ((test (test value function &rest args)
	   (let ((result (apply function args)))
	     (format t "(~a ~{~a~^ ~}) = ~a >>> ~:[FAIL~;PASS~]~%"
		     function args result (funcall test result value)))))
    (test #'equal '(1 . 1) 'vec+ '(1 . 0) '(0 . 1))
    (test #'equal '(0 . 1) 'vec- '(1 . 1) '(1 . 0))
    (test #'equal '(-1 . -1) 'vec- '(1 . 1))
    (test #'= 5.0 'vec-length '(3 . 4))))

(defun show-colors ()
  (loop for code below 111
	do (format t "~c[~dmCODE ~:*~d~0@*~c[0m~%" #\esc code)))

(let ((memos (make-hash-table :test #'equal)))
  (labels ((calculate-los (to from)
	     (let ((dx (- (car to) (car from)))
		   (dy (- (cdr to) (cdr from))))
	       (labels ((get-pos-on-line (m)
			  (cons (round (+ (car from) (* m dx)))
				(round (+ (cdr from) (* m dy)))))
			(pos-opaquep (m)
			  (let* ((pos (get-pos-on-line m))
				 (actor (solid pos)))
			    (if actor
				(wallp actor)
				nil))))
		 (and (loop for x below (abs dx)
			    never (pos-opaquep (/ x (abs dx))))
		      (loop for y below (abs dy)
			    never (pos-opaquep (/ y (abs dy)))))))))
    (defun has-los (to from distance)
      (let ((key (list to from)))
	(multiple-value-bind (memo memo-existsp) (gethash key memos)
	     (if (if memo-existsp
		     memo
		     (let ((new-val (calculate-los to from)))
		       (setf (gethash key memos) new-val)
		       new-val))
		 ;; check if point is within distance
		 (or (< distance 0)
		     (>= distance (vec-length (vec- to from))))
		 nil))))))

(defgeneric pickup (item)
  (:method (item))
  (:method :after ((item equipment))
    (remove-non-solid (pos item))
    (add-to-inventory item)
    (print-to-log "you picked up a ~a" (name item))))

(defgeneric interact (object actor)
  (:method (object actor))
  (:method ((item equipment) (actor (eql *player*)))
    (pickup item)))

(defgeneric move-into (passive active)
  (:method ((passive creature) (active creature))
    (if (weapon active)
	(attack (get-attack active (weapon active)) passive)))
  (:method (passive active))) ; default case: do nothing

(defun move-into-pos (pos obj)
  (move-into (solid pos) obj)
  (move-into (non-solid pos) obj))

(defmacro flood-fill (start (value-to-store exit-condition
			     &key (solid t) (stop-for-occupied t) (go-until nil))
		      &body body)
  `(let ((cells (make-hash-table :test #'equal)))
     (setf (gethash ,start cells) t)
     (labels ((occupiedp (pos)
		(if ,solid
		    (solid pos)
		    (or (non-solid pos) (wallp (solid pos)))))
	      (neighbors (pos)
		(loop for direction in +directions+
		      unless (let ((cell-pos (vec+ pos direction)))
			       (or (gethash cell-pos cells)
				   (wallp (solid cell-pos))
				   (and (occupiedp cell-pos)
					(not (equal cell-pos ,go-until))
					,stop-for-occupied)))
			collect (vec+ pos direction)))
	      (iterate (frontier)
		(when (car frontier)
		  (let* ((current (car frontier))
			 (neighbors (neighbors current))
			 (exit-condition ,exit-condition))
		    (if exit-condition
			exit-condition
			(progn
			  (mapc (lambda (n) (setf (gethash n cells) ,value-to-store)) neighbors)
			  (iterate (append (cdr frontier) neighbors))))))))
       (let ((result (iterate (list ,start))))
	 ,@body))))

(defun place (obj pos &key (solid t) (interact t))
  (flood-fill pos (t (unless (occupiedp current) current) 
		     :stop-for-occupied nil
		     :solid solid)
	      (if solid
		  (setf (solid result) obj)
		  (setf (non-solid result) obj))
	      (setf (pos obj) result)))

(defun find-path (from to)
  (flood-fill from (current (if (equal current to) t) :go-until to)
	      (if result
		  (labels ((build-path (pos &optional (path nil))
			     (if (equal pos from)
				 path
				 (build-path (gethash pos cells) (cons pos path)))))
		    (build-path to))
		  (list from))))

(defun reposition (obj new-pos)
  (let ((collider (solid new-pos)))
    (if collider
	(move-into collider obj)
	(progn (remove-solid (pos obj))
	       (setf (solid new-pos) obj)
	       (setf (pos obj) new-pos)
	       (move-into (non-solid new-pos) obj)))))

(defun move (obj direction)
  (reposition obj (vec+ (pos obj) direction)))

(defun step-towards (target obj)
  (reposition obj (car (find-path (pos obj) (pos target)))))

(defgeneric act (obj)
  (:method ((obj enemy))
    (when (>= (energy obj) 1)
      (step-towards *player* obj)
      (decf (energy obj))
      (act obj))))

(defgeneric update (obj)
  (:method (obj))
  (:method ((obj enemy))
    (incf (energy obj) (/ (spd obj) (spd *player*)))
    (act obj)))

(defgeneric visiblep (obj)
  (:method ((pos list))
    (has-los pos (pos *player*) *sight-distance*))
  (:method ((n symbol)) nil)
  (:method ((c character)) t)
  (:method ((obj actor))
    (if (hiddenp obj)
	nil
	(visiblep (pos obj)))))

(defun print-board ()
  (apply-default-colors)
  (loop for y below (cdr *board-size*)
	do (format t "~{~a~}~%"
		   (loop for x below (car *board-size*)
			 collect (let* ((pos (cons x y))
					(actor (contents pos)))
				   (cond ((and (wallp actor)
					       (or (visiblep pos)
						   (visiblep actor)))
					  (display-char pos))
					 ((and actor (visiblep actor))
					  (display-char actor))
					 ((visiblep pos) #\.)
					 (t #\space)))))))

(loop for i below 10
      do (setf (solid (cons i 0)) 'wall)
      do (setf (solid (cons i 9)) 'wall)
      do (setf (solid (cons 0 i)) 'wall)
      do (setf (solid (cons 9 i)) 'wall))

(place *player* '(5 . 5))
(setf (weapon *player*) (make-weapon :dmg '(1 6) :damage-types '(slashing)))
(place (make-instance 'enemy :display-char #\g :color 32 :name "goblin" :weapon (make-weapon :dmg '(1 4) :damage-types '(slashing))) '(2 . 2))
(place (make-instance 'equipment :name "cheese") '(4 . 6) :solid nil)

(defun print-surroundings ()
  (flet ((printp (obj)
	   (not (or (eq *print-surroundings-mode* 'none)
		    (not obj)
		    ;; things that are walls but not characters (secret doors) should be printed
		    (and (characterp obj) (eq *print-surroundings-mode* 'non-walls))))))
    (print-to-screen "~:[~;you see ~]~:*~{~:[~;a ~:*~a ~a~#[~;~; and ~:;, ~]~]~}~%"
		     (append
		      (loop for direction in +directions+
			    when (printp (contents (vec+ (pos *player*) direction)))
		    	      collect (name (contents (vec+ (pos *player*) direction)))
			      and collect (concatenate 'string "to the "
						       (gethash direction +direction-names+)))
		      (let ((obj (non-solid (pos *player*))))
			(when (printp obj)
			  (list (name obj) "in your space")))))))

(defun print-game ()
  (clear-screen)
  (print-board)
  (print-surroundings)
  (print-log))

(defun start ()
  (labels ((process-round (input)
	     (unless (eq input #\q)
	       (let ((action (gethash input *actions*)))
		 (when action
		   (funcall action)
		   (loop for actor being the hash-values of *solid-actors*
			 do (update actor))
		   (loop for actor being the hash-values of *non-solid-actors*
			 do (update actor))))
	       (print-game)
	       (process-round (custom-read-char)))))
    (process-round #\space))
  (format t "~c[0m" #\esc))

(defaction #\a "move left" (move *player* +left+))
(defaction #\d "move right" (move *player* +right+))
(defaction #\w "move up" (move *player* +up+))
(defaction #\s "move down" (move *player* +down+))
(defaction #\p "wait" t)
(defaction #\i "interact"
  (interact (non-solid (pos *player*)) *player*))
(defaction #\v "print inventory"
  (print-inventory))
(defaction #\D "drop an item"
  (with-item-from-inventory
      (remove-from-inventory item)
    (place item (pos *player*) :solid nil)))
(defaction #\# "open a REPL"
  (labels ((my-repl ()
	     (fresh-line)
	     (princ ">>> ")
	     (force-output)
	     (let ((input (read-from-string (read-line))))
	       (unless (eq input 'q)
		 (print (eval input))
		 (my-repl)))))
    (my-repl)))

(start)
