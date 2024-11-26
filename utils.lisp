(defparameter *log* '())
(setf *random-state* (make-random-state t))

(defun log-to-string (control-string &rest args)
  (labels ((convert-to-string (item)
	     (cond
	       ((listp item)
		(mapcar #'convert-to-string item))
	       ((symbolp item)
		(coerce (loop for c in (coerce (prin1-to-string item) 'list)
			      collect (if (eql c #\-)
					  #\space
					  (char-downcase c)))
			'string))
	       (t item))))
    (apply #'format nil control-string (mapcar #'convert-to-string args))))

(defun print-to-log (control-string &rest args)
  (setf *log* (append
	       *log*
	       (list (apply #'log-to-string control-string args)))))

(defun print-to-screen (control-string &rest args)
  (princ (apply #'log-to-string control-string args)))

(defun square (number)
  (* number number))

(defun add-pos (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defun sub-pos (a b)
  (cons (- (car a) (car b)) (- (cdr a) (cdr b))))

(defun mul-pos (a scalar)
  (cons (* (car a) scalar) (* (cdr a) scalar)))

(defun distance (p1 p2)
  (sqrt (+ (square (- (car p2) (car p1)))
	   (square (- (cdr p2) (cdr p1))))))

;; generate a random number between 1 and d
(defun roll (d)
  (1+ (random (max 1 d))))
