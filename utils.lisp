(defparameter *log* '())
(defparameter *in-terminal* (handler-case (sb-posix:tcgetattr 0)
			      (error () nil)))

(setf *random-state* (make-random-state t))

(defun custom-read-char ()
  (force-output)
  (if *in-terminal*
      (trivial-raw-io:read-char)
      (read-char)))

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
  (princ (apply #'log-to-string control-string args))
  (force-output))

(defun square (number)
  (* number number))

(defun add-pos (&rest points)
  (loop for p in points
	sum (car p) into x
	sum (cdr p) into y
	finally (return (cons x y))))

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

(defun randnth (lst)
  (if (listp lst)
      (nth (random (length lst)) lst)
      lst))

(defun pos-flatten (lst)
  (if lst
      (if (and (numberp (car lst)) (numberp (cdr lst)))
	  (list lst)
	  (append (pos-flatten (car lst)) (if (cdr lst)
					      (pos-flatten (cdr lst)))))))

(defun flatten (lst) ;; https://www.lee-mac.com/flatten.html
  (if (atom lst)
      (list lst)
      (append (flatten (car lst)) (if (cdr lst)
				      (flatten (cdr lst))))))

(defun eval-weighted-list (list)
  (labels ((get-results (lst)
	     (if (numberp (caar lst))
		 (loop for pair in lst
		       with index = (random 100)
		       when (< index (car pair))
			 return (if (atom (cadr pair))
				    (cadr pair)
				    (get-results (cdr pair)))
		       do (decf index (car pair)))
		 (mapcar #'get-results lst))))
    (flatten (get-results list))))

(defun get-closest-point-to (point region)
  (let* ((x (loop for p in region
		  with minx = 1000
		  when (< (abs (- (car point) (car p))) minx)
		    do (setf minx (car p))
		  finally (return minx)))
	 (y (loop for p in region
		  with miny = 1000
		  when (and (eq (car p) x)
			    (< (abs (- (cdr point) (cdr p))) miny))
		    do (setf miny (cdr p))
		  finally (return miny))))
    (cons x y)))
