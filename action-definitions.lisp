(defparameter *actions* (make-hash-table))
(defparameter *action-descriptions* (make-hash-table))
(defparameter *game-over-p* nil)

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

(defmacro on-new-screen (&body body)
  `(progn (clear-screen)
	  ,@body
	  (print-to-screen "~2%press any key to return to game")
	  (custom-read-char)))

(defun resolve-action (input)
  (let ((action (gethash input *actions*)))
    (if action
	(funcall action)
	0)))

(defmacro with-time-safe-item-from-inventory (&body body)
  `(with-item-from-inventory-if (setf .time. 0)
     ,@body))

(defmacro with-time-safe-owned-item (&body body)
  `(with-owned-item-if (setf .time. 0)
     ,@body))

(defaction (#\4 #\h) 1 "move left" (move *player* +left+))

(defaction (#\6 #\l) 1 "move right" (move *player* +right+))

(defaction (#\8 #\k) 1 "move up" (move *player* +up+))

(defaction (#\2 #\j) 1 "move down" (move *player* +down+))

(defaction (#\9 #\u) 1 "move up-right" (move *player* '(1 . -1)))

(defaction (#\7 #\y) 1 "move up-left" (move *player* '(-1 . -1)))

(defaction (#\3 #\n) 1 "move down-right" (move *player* '(1 . 1)))

(defaction (#\1 #\b) 1 "move down-left" (move *player* '(-1 . 1)))

(defaction #\P 1 "wait" t)

(defaction (#\i #\,) 1 "interact"
  (let ((item (non-solid (pos *player*))))
    (if (and item (not (hiddenp item)))
	(interact item *player*)
	(print-to-log "there is nothing here"))))

(defaction #\v 0 "print inventory"
  (print-inventory))

(defaction #\D 1 "drop an item"
  (with-time-safe-item-from-inventory
      (remove-from-inventory item)
    (place item (pos *player*) :solid nil)))

(defaction #\e 1 "equip an item"
  (with-time-safe-item-from-inventory
      (let ((result (equip item *player*)))
	(when result
	  (if (listp result)
	      (print-to-log "you equipped ~a instead of ~{~a~#[~; and ~:;, ~]~}"
			    (name item) (mapcar #'name result))
	      (print-to-log "you equipped ~a" (name item)))))))

(defaction #\U 1 "unequip an item"
  (let ((item-list (apply #'append (loop for i-list being the hash-values of (equipment *player*)
					 collect i-list))))
    (if item-list
	(let ((item (get-item-from-list
		     item-list
		     :naming-function (lambda (i) (log-to-string "~a (~a)" (name i) (equip-slot i)))
		     :what "item to unequip")))
	  (unequip item *player*)
	  (print-to-log "you have unequipped ~a" (name item)))
	(print-to-log "you have nothing equipped"))))

(defaction #\E 1 "eat"
  (with-time-safe-item-from-inventory
    (eat item *player*)))

(defaction #\t 1 "throw an item"
  (with-time-safe-owned-item
      (let ((target (choose-target 'free-form
				   (if (= (size item) 1)
				       3
				       1))))
	(when target
	  (throw-at target item *player*)))))

(defaction #\A 1 "attack"
  (let ((target (choose-target 'two-key (range (car (weapons *player*))))))
    (when (and target (not (listp target)))
      (attack target *player*))))

(defaction #\x 1 "examine"
  (with-time-safe-owned-item
      (print-to-log (description item))))

(defaction #\q 1 "quaff"
  (with-time-safe-owned-item
      (quaff item *player*)))

(defaction #\R 0 "rest"
  (flet ((cannot-rest ()
	   (cond ((loop for actor-pos being the hash-keys of (solid-actors)
			thereis (and (visiblep actor-pos (pos *player*))
				     (hostilep (solid actor-pos))))
		  "there are enemies nearby")
		 ((= (health *player*) (max-health *player*))
		  "you are fully rested")
		 ((<= (hunger *player*) 20)
		  "you need food")
		 (t
		  nil))))
    (let ((resting-status (make-resting-status))
	  (initial-resting-status (cannot-rest)))
      (if initial-resting-status
	  (print-to-log "you cannot rest because ~a" initial-resting-status)
	  (loop until (cannot-rest)
		  initially (apply-to *player* resting-status)
		do (update-actors)
		finally (progn (remove-status resting-status)
			       (print-to-log "you wake up because ~a"
					     (cannot-rest))))))))

(defaction #\/ 1 "look"
  (with-direction (setf .time. 0)
    (look-at (vec+ (pos *player*) direction))))

(defaction #\# 0 "open a REPL"
  (labels ((read-and-eval (previous-input)
	     (let ((input (if previous-input
			      (concatenate 'string
					   previous-input
					   " "
					   (read-line))
			      (read-line))))
	       (if (string= "q" input)
		   'exit-repl
		   (handler-case (eval (read-from-string input))
		     (end-of-file () (read-and-eval input))))))
	   (my-repl ()
	     (format t "~&>>> ")
	     (force-output)
	     (let ((result (read-and-eval nil)))
	       (unless (eq result 'exit-repl)
		 (print result)
		 (my-repl)))))
    (my-repl)))

(defaction #\? 0 "help"
  (on-new-screen
   (column-print (loop for action being the hash-keys of *action-descriptions*
		       collect (log-to-string "~a: ~a"
					      action
					      (gethash action *action-descriptions*)))
		 :fit-screen t)))

(defaction #\Q 0 "quit"
  (setf *game-over-p* t))
