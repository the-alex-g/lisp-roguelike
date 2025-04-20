(defparameter *experience* 0)
(defparameter *level* 1)

(defun increase-health ()
  (let ((health-increase (max 1 (roll 1 10 (con *player*)))))
    (incf (slot-value *player* 'max-health) health-increase)
    (incf (health *player*) health-increase)))

(defun increase-stat ()
  (let ((ability (get-item-from-list '(str con dex spd int per cha det)
				     :what "ability to increase"
				     :exit-option nil)))
    (eval `(incf (,ability *player*)))))

;; triangular numbers times 10
(defun xp-for-next-level ()
  (* *level* (1+ *level*) 5))

(defun level-up ()
  (decf *experience* (xp-for-next-level))
  (incf *level*)
  (increase-health)
  (increase-stat)
  (print-to-log "you leveled up to level ~d" *level*)
  (if (>= *experience* (xp-for-next-level))
      (level-up)))

(defun gain-experience (amount)
  (incf *experience* amount)
  (if (>= *experience* (xp-for-next-level))
      (level-up)))
