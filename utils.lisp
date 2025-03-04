(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))
(defparameter +zero+ '(0 . 0))
(defparameter +directions+ (list +up+ +right+ +down+ +left+
				 '(1 . 1) '(1 . -1) '(-1 . 1) '(-1 . -1)))
(defparameter +direction-names+ (make-hash-table :test #'equal))
(defparameter *log* '())
(defparameter *in-terminal* (handler-case (sb-posix:tcgetattr 0)
			      (error () nil)))
(defparameter *fake-input* nil)

(setf (gethash +left+ +direction-names+) "west")
(setf (gethash +right+ +direction-names+) "east")
(setf (gethash +down+ +direction-names+) "south")
(setf (gethash +up+ +direction-names+) "north")

(setf *random-state* (make-random-state t))

(defun ensure-list (arg)
  (if (listp arg)
      arg
      (list arg)))

(defun custom-read-char ()
  (force-output)
  (if *fake-input*
      *fake-input*
      (if *in-terminal*
	  (trivial-raw-io:read-char)
	  (read-char))))

(defmacro with-fake-input (input &body body)
  `(let ((*fake-input* ,input))
     ,@body))

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

(defun vec* (vector scalar)
  (cons (* (car vector) scalar) (* (cdr vector) scalar)))

(defun vec-length (vector)
  (sqrt (+ (square (car vector))
	   (square (cdr vector)))))

(defun distance (a b)
  (floor (vec-length (vec- a b))))

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
	with best-point = nil
	when (or (not best-point)
		 (< (distance p point) (distance point best-point)))
	  do (setf best-point p)
	finally (return best-point)))

;; Return an item, chosen by the player, from the given list
;; If the list items are not printable, pass a naming-function that gets a
;; printable name from the list item.
(defun get-item-from-list (lst &key
				 (naming-function (lambda (x) (log-to-string "~a" x)))
				 (ignoring nil)
				 (test #'eq)
				 (exit-option t)
				 (what "object"))
  (let* ((temp (loop for x in lst
		     when (funcall naming-function x)
		       unless (member x ignoring :test test)
			 collect (log-to-string "~a" (funcall naming-function x)) into a
			 and collect x into b
		     finally (return (cons a b))))
	 (name-list (car temp))
	 (item-list (cdr temp))
	 (tab-length (+ 7 (loop for item in name-list maximize (length item)))))
    (labels ((print-list (from)
	       (print-to-screen "~{~@?~}"
				(loop for n in from
				      with i = 0
				      if (= 0 (mod i 2))
					collect "~%~2t~d) ~a"
					and collect i
					and collect n
					and do (incf i)
				      else
					collect "~vt~d) ~a"
					and collect tab-length
					and collect i
					and collect n
					and do (incf i))))
	     (pick-item (from)
	       (fresh-line)
	       (print-to-screen "Choose an ~a: " what)
	       (let* ((raw (if (<= (length lst) 10)
			       (custom-read-char)
			       (read-line)))
		      (index (if (<= (length lst) 10)
				 (digit-char-p raw)
				 (parse-integer raw))))
		 (cond ((or (eq raw #\q) (string= raw "q"))
			nil)
		       ((and index (< index (length from)))
			(nth index from))
		       (t
			(print-to-screen "~%That was an invalid choice")
			(pick-item from))))))
      (print-list (if exit-option
		      (append name-list '(cancel))
		      name-list))
      (pick-item (if exit-option
		     (append item-list '(nil))
		     item-list)))))
