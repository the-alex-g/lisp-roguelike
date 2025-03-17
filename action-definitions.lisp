(defaction (#\4 #\h) "move left" (move *player* +left+))

(defaction (#\6 #\l) "move right" (move *player* +right+))

(defaction (#\8 #\k) "move up" (move *player* +up+))

(defaction (#\2 #\j) "move down" (move *player* +down+))

(defaction (#\9 #\u) "move up-right" (move *player* '(1 . -1)))

(defaction (#\7 #\y) "move up-left" (move *player* '(-1 . -1)))

(defaction (#\3 #\n) "move down-right" (move *player* '(1 . 1)))

(defaction (#\1 #\b) "move down-left" (move *player* '(-1 . 1)))

(defaction #\P "wait" t)

(defaction #\i "interact"
  (interact (non-solid (pos *player*)) *player*))

(defaction #\v "print inventory"
  (print-inventory))

(defaction #\D "drop an item"
  (with-item-from-inventory
      (remove-from-inventory item)
    (place item (pos *player*) :solid nil)))

(defaction #\e "equip an item"
  (with-item-from-inventory
    (let ((result (equip item *player*)))
      (cond ((listp result)
	     (print-to-log "you equipped ~a instead of ~{~a~#[~; and ~:;, ~]~}"
			   (name item) (mapcar #'name result)))
	    (result
	     (print-to-log "you equipped ~a" (name item)))))))

(defaction #\U "unequip an item"
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

(defaction #\E "eat"
  (with-item-from-inventory
      (eat item *player*)))

(defaction #\t "throw an item"
  (with-item-from-inventory
    (let ((target (choose-target 'free-form
				 (if (= (size item) 1)
				     3
				     1))))
      (when target
	(throw-at target item *player*)))))

(defaction #\A "attack"
  (let ((target (choose-target 'two-key (range (car (weapons *player*))))))
    (when (and target (not (listp target)))
      (attack target *player*))))

(defaction #\x "examine"
  (with-item-from-inventory
      (print-to-log (description item))))

(defaction #\R "rest"
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

(defaction #\# "open a REPL"
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

(defaction #\? "help"
  (loop for action being the hash-keys of *action-descriptions*
	do (print-to-log "~a: ~a" action (gethash action *action-descriptions*)))
  (print-to-log "q: quit"))
