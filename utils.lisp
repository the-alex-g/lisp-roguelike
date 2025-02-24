(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))
(defparameter +zero+ '(0 . 0))
(defparameter +directions+ (list +down+ +right+ +up+ +left+))
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

(defun print-log ()
  (loop for item in *log*
	do (format t "~a~%" item))
  (setf *log* '()))

(defun print-to-screen (control-string &rest args)
  (princ (apply #'log-to-string control-string args))
  (force-output))

(defun square (number)
  (expt number 2))

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

(defun vec-length (vector)
  (sqrt (+ (square (car vector))
	   (square (cdr vector)))))

(defun distance (a b)
  (vec-length (vec- a b)))

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
  (loop for p in region
	with best-point = '(0 . 0)
	when (< (distance p point) (distance point best-point))
	  do (setf best-point p)
	finally (return best-point)))
