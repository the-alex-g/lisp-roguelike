(load "~/quicklisp/setup.lisp")
(ql:quickload :trivial-raw-io)

(load "masking.lisp")
(load "generic-functions.lisp")
(load "utils.lisp")
(load "log.lisp")
(load "colors.lisp")
(load "class-definitions.lisp")

(defparameter *player*
  (make-instance 'player :health 20 :name "player" :color 'red :illumination 5 :char #\@))

(mapc #'load
      (list "bsp-dungeon.lisp"
	    "dungeon.lisp"
	    "terminal.lisp"
	    "inventory.lisp"
	    "definition-macros.lisp"
	    "shops.lisp"
	    "idle-behaviors.lisp"
	    "codex.lisp"
	    "alignment.lisp"
	    "leveling.lisp"))

(define-mask-set '(slashing piercing bludgeoning acid lightning cold fire holy unholy necrotic))

(defparameter *game-over-p* nil)
(defparameter *print-surroundings-mode* 'my-space)

(setf (slot-value *player* 'max-health) (health *player*))

(defun generate-attack (attacker num die &optional dmg-bonus to-hit types statuses)
  (make-attack :dmg (roll num die (str attacker) dmg-bonus)
	       :to-hit (roll 1 20 to-hit (dex attacker))
	       :source (name attacker)
	       :types (make-mask (ensure-list types))
	       :statuses (ensure-list statuses)))

(defun get-loot (obj)
  (let ((loot '()))
    (when (meat obj)
      (push (meat obj) loot))
    (loop for item in (loot obj)
	  if (and item (atom item))
	    do (push item loot)
	  else
	    do (let ((bit (eval-weighted-list item)))
		 (when (car bit)
		   (mapc (lambda (b) (push b loot)) bit))))
    (loop for item-list being the hash-values of (equipment obj)
	  do (loop for item in item-list
		   unless (breaksp item 50)
		     do (push item loot)))
    loot))

(defun find-path (from to)
  (a-star from to #'movement-cost))

(defun damage-modifier (defender damage-types)
  (unless (numberp damage-types)
    (setf damage-types (make-mask damage-types)))
  (let* ((traits (list (absorbances defender)
		       (resistances defender)
		       (vulnerabilities defender)
		       (immunities defender)))
	 (others (lognot (apply #'logior traits))))
    (/ (loop for trait in (append traits (list others))
	     for modifier in '(-1 1/2 2 0 1)
	     sum (* (mask trait damage-types) modifier))
       (logcount damage-types))))

;;; memoized has-los function
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
    (defun has-los (to from)
      (let ((key (list to from)))
	(multiple-value-bind (memo memo-existsp) (gethash key memos)
	  (if memo-existsp
	      memo
	      (let ((new-val (calculate-los to from)))
		(setf (gethash key memos) new-val)
		new-val)))))))

(defparameter *lightmap* (make-hash-table :test #'equal))

(defun generate-lightmap (glowing-actors)
  (flet ((light-strength (distance light)
	   (- 1 (square (/ distance light)))))
    (let ((lightmap (make-hash-table :test #'equal :size (* 100 (length glowing-actors)))))
      (loop for light-source in glowing-actors
	    do (let ((light (illumination light-source)))
		 (loop for x from (- light) to light
		       do (loop for y from (- light) to light
				when (and (<= (vec-length (cons x y)) light)
					  (has-los (vec+ (cons x y) (pos light-source))
						   (pos light-source)))
				  do (let ((pos (vec+ (cons x y) (pos light-source))))
				       (setf (gethash pos lightmap)
					     (min 1 (+ (gethash pos lightmap 0)
						       (light-strength (vec-length (cons x y))
								       light)))))))))
      lightmap)))

(defun illuminatedp (pos)
  (> (illumination pos) 0))

(defconsume quaff potion)
(defconsume eat)

(defun step-on-path (path obj)
  (reposition obj (car path)))

(defun flee-direction (obj path)
  (vec- (pos obj) (car path)))

(defun flee (obj path)
  (move obj (flee-direction obj path)))

(defun can-flee (obj path)
  (not (solid (vec+ (pos obj) (flee-direction obj path)))))

(defun target-too-close-p (obj range)
  (<= (distance (pos obj) (target-pos obj)) (/ range 2)))

(defun choose-target (initial-mode range &key (can-switch-p t))
  (let ((target-position (pos *player*))
	(target-list
	  (apply #'append
		 (loop for y from (- range) to range
		       collect (loop for x from (- range) to range
				     with pos = +zero+
				     do (setf pos (vec+ (cons x y)
							(pos *player*)))
				     when (and (visiblep pos (pos *player*))
					       (<= (distance (pos *player*) pos)
						   range)
					       (contents pos)
					       (not (wallp (contents pos)))
					       (not (equal (contents pos) *player*)))
				       collect (contents pos))))))
    (if (or target-list
	    (eq initial-mode 'free-form))
	(labels ((two-key-targeting ()
		   (let ((i 0))
		     (if (and target-list
			      (<= (length target-list) 10))
			 (progn (with-cursor-saved
				    (mapc (lambda (obj)
					    (position-cursor-list (pos obj))
					    (apply-color (format nil "~d" i) (color obj)
							 :function #'print-to-screen)
					    (incf i))
					  target-list))
				(labels ((undraw-numbers ()
					   (with-cursor-saved
					       (mapc (lambda (obj)
						       (position-cursor-list (pos obj))
						       (print-to-screen (display-char obj)))
						     target-list)))
					 (get-target ()
					   (let* ((input (custom-read-char))
						  (value (digit-char-p input)))
					     (cond (value
						    (if (< value i)
							(progn (undraw-numbers)
							       (nth value target-list))
							(get-target)))
						   ((and (eq input #\S)
							 can-switch-p)
						    (undraw-numbers)
						    (free-form-targeting))
						   ((eq input #\q)
						    (undraw-numbers)
						    nil)
						   (t
						    (get-target))))))
				  (get-target)))
			 (free-form-targeting))))
		 (free-form-targeting ()
		   (labels ((draw-cursor ()
			      (with-cursor-saved
				  (position-cursor-list target-position)
				(with-color (31 5)
				  (print-to-screen "X"))))
			    (undraw-cursor ()
			      (with-cursor-saved
				  (position-cursor-list target-position)
				(let ((contents (contents target-position)))
				  (if (and contents (not (hiddenp contents)))
				      (print-to-screen "~a" (display-char contents))
				      (print-to-screen ".")))))
			    (move-cursor (direction)
			      (when (<= (distance (vec+ target-position direction) (pos *player*))
					range)
				(undraw-cursor)
				(setf target-position (move target-position direction))))
			    (input-loop ()
			      (draw-cursor)
			      (let ((input (custom-read-char)))
				(cond ((and (eq input #\s)
					    can-switch-p)
				       (undraw-cursor)
				       (two-key-targeting))
				      ((eq input #\newline)
				       (undraw-cursor)
				       (or (contents target-position)
					   target-position))
				      ((eq input #\q)
				       (undraw-cursor)
				       nil)
				      (t
				       (move-cursor (get-direction input))
				       (input-loop))))))
		     (input-loop))))
	  (if (eq initial-mode 'two-key)
	      (two-key-targeting)
	      (free-form-targeting)))
	(progn (print-to-log "there are no targets in range")
	       nil))))

(defun get-player-lines ()
  (flatten
   (list (log-to-string "STR ~@d  DEX ~@d  CON ~@d"
			(str *player*) (dex *player*) (con *player*))
	 (log-to-string "INT ~@d  PER ~@d  CHA ~@d"
			(intl *player*) (per *player*) (cha *player*))
	 (mapcar (lambda (weapon)
		   (log-to-string "~@:(~a~): ~a"
				  (name weapon)
				  (damage-string (atk weapon))))
		 (weapons *player*))
	 (log-to-string "HEALTH ~d/~d"
			(health *player*)
			(max-health *player*))
	 (log-to-string "EXPERIENCE ~d/~d"
			*experience*
			(xp-for-next-level))
	 (log-to-string "FOOD ~d/~d"
			(hunger *player*)
			(max-hunger *player*))
	 (log-to-string "GOLD ~d" *gold*))))

(defun print-surroundings ()
  (if (eq *print-surroundings-mode* 'my-space)
      (let ((obj (non-solid (pos *player*))))
	(when (and obj (not (hiddenp obj)))
	  (print-to-screen "you are standing over a ~a~%" (surrounding-name obj))))
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
			      (list (name obj) "in your space"))))))))

(defun darkness (pos)
  (- 1 (illumination pos)))

(defun print-board ()
  (apply-default-colors)
  (let ((player-lines (get-player-lines)))
    (loop for y from -1 to (1+ (cdr *board-size*))
	  do (format t "~{~a~}~a~%"
		     (loop for x from -1 to (1+ (car *board-size*))
			   collect (let* ((pos (cons x y))
					  (actor (contents pos))
					  (has-los-p (visiblep pos (pos *player*))))
				     (cond ((characterp actor)
					    (apply-color actor
							 (if has-los-p
							     (darken 255 (darkness pos) 240)
							     240)))
					   ((and actor (not (hiddenp actor)) has-los-p)
					    (display-char actor :darken (darkness pos)))
					   (has-los-p
					    (display-char pos :has-los-p has-los-p))
					   (t #\space))))
		     (or (nth (1+ y) player-lines) "")))))

(defun print-game ()
  (clear-screen)
  (setf *lightmap* (generate-lightmap (glowing-actors)))
  (print-board)
  (print-surroundings)
  (print-log))

(defun update-actors ()
  (loop for actor being the hash-values of (solid-actors)
	do (update actor))
  (loop for actor being the hash-values of (non-solid-actors)
	do (update actor)))

(defun game-over-p ()
  (or (deadp *player*)
      *game-over-p*))

(defun resolve-action (input)
  (let ((action (gethash input *actions*)))
    (if action
	(funcall action)
	0)))

(defun start ()
  (print-game)
  (labels ((process-round (input)
	     (loop repeat (resolve-action input)
		   do (update-actors))
	     (print-game)))
    (loop until (game-over-p)
	  do (process-round (custom-read-char))))
  (when (deadp *player*)
    (format t "~a has died.~c[0m~%~%" (name *player*) #\esc)))

(load "action-definitions.lisp")
(load "methods.lisp")

(add-layer '((75 ((75 ((75 ((75 make-goblin)
			    (25 make-goblin-archer)))
		       (25 make-kobold)))
		  (25 make-troll)))
	     (25 make-pit-trap))
	   '((50 make-brazier)
	     (50 make-wall)))

(let ((cells (add-layer '((75 ((75 ((25 make-goblin-archer)
				    (75 make-goblin)))
			       (25 make-kobold)))
			  (25 make-pit-trap))
			'((50 make-wall)
			  (50 make-brazier)))))
  (setf *current-layer* (car *layers*))
  (place *player* (pos (make-shopkeeper (get-spawn-position cells 8)))))
(equip (make-sword) *player*)

(start)
