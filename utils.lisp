(defun pretty-print (control-string &rest args)
  (labels ((convert-to-string (item)
	     (if (stringp item)
		 item
		 (coerce (loop for c in (coerce (prin1-to-string item) 'list)
			       collect (if (eql c #\-)
					   #\space
					   (char-downcase c)))
		     'string))))
    (apply #'format t control-string (mapcar #'convert-to-string args))))

(defun pretty-print-to-string (control-string &rest args)
  (labels ((convert-to-string (item)
	     (if (stringp item)
		 item
		 (coerce (loop for c in (coerce (prin1-to-string item) 'list)
			       collect (if (eql c #\-)
					   #\space
					   (char-downcase c)))
		     'string))))
    (apply #'format nil control-string (mapcar #'convert-to-string args))))

(defun square (number)
  (* number number))

(defun add-pos (p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(defun sub-pos (a b)
  (cons (- (car a) (car b)) (- (cdr a) (cdr b))))

(defun distance (p1 p2)
  (sqrt (+ (square (- (car p2) (car p1)))
	   (square (- (cdr p2) (cdr p1))))))

;; generate a random number between 1 and d
(defun roll (d)
  (1+ (random (max 1 d))))
