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

(defun constructor (&rest items)
  (read-from-string (format nil "make~{-~a~}" (mapcar #'symbol-name items))))

(defun make-keyword (name)
  (intern (symbol-name name) "KEYWORD"))

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

(defun distance (a b &key (exactp nil))
  (if exactp
      (vec-length (vec- a b))
      (floor (vec-length (vec- a b)))))

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

(defun weighted-list-p (list)
  (and (listp list)
       (listp (car list))
       (numberp (caar list))))

(defun eval-weighted-list (list &optional (weight 100))
  (labels ((get-results (lst)
	     (if (numberp (caar lst))
		 (loop for pair in lst
		       with index = (random weight)
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

(defun column-print (list-of-items
		     &key
		       (indexp nil) (columns 2) (fit-screen t)
		       (print-function #'print-to-screen))
  (let ((tab-length (+ (loop for item in list-of-items maximizing (length item))
		       (if indexp 5 2))))
    (if (> tab-length 40)
	(setf columns 1)
	(when fit-screen
	  (setf columns
		(floor (/ (if indexp 78 80) tab-length)))))
    (funcall print-function
	      "~{~@?~}"
	      (do ((remaining-items list-of-items (cdr remaining-items))
		   (i 0 (1+ i))
		   (collected-items
		    nil
		    (append
		     collected-items
		     (if (= 0 (mod i columns))
			 (if indexp
			     (list "~%~2t~d) ~a" i (car remaining-items))
			     (list "~%~a" (car remaining-items)))
			 (if indexp
			     (list "~vt~d) ~a" (+ 2 (* tab-length (mod i columns)))
				   i (car remaining-items))
			     (list "~vt~a" (* tab-length (mod i columns))
				   (car remaining-items)))))))
		  ((not remaining-items) collected-items)))))

;; Return an item, chosen by the player, from the given list
;; If the list items are not printable, pass a naming-function that gets a
;; printable name from the list item.
(defun get-item-from-list (lst &key
				 (naming-function (lambda (x) x))
				 (ignoring nil)
				 (test #'eq)
				 (exit-option t)
				 (anp t)
				 (what "object"))
  (let* ((temp (loop for x in lst
		     when (funcall naming-function x)
		       unless (member x ignoring :test test)
			 collect (log-to-string "~a" (funcall naming-function x)) into a
			 and collect x into b
		     finally (return (cons a b))))
	 (name-list (car temp))
	 (item-list (cdr temp)))
    (labels ((pick-item (from)
	       (fresh-line)
	       (print-to-screen "Choose a~:[~;n~] ~a: " anp what)
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
      (column-print (if exit-option
			(append name-list (list "cancel"))
			name-list)
		    :indexp t)
      (pick-item (if exit-option
		     (append item-list '(nil))
		     item-list)))))

(defun confirm-action (message)
  (get-item-from-list '(t) :naming-function (lambda (x)
					      (declare (ignore x))
					      message)
		      :what 'option))

(defun roll (num die &rest modifiers)
  (+ (loop repeat num
	   sum (1+ (random die)))
     (loop for m in modifiers
	   sum m)))

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

(defun apply-colors (arg colors)
  (format nil "~c[~{~d~^;~}m~a~0@*~c[40;37m"
	  #\esc (ensure-list colors)
	  arg))

(defun damage-string (atk)
  (setf (nth 4 atk) (ensure-list (nth 4 atk)))
  (apply #'log-to-string
	 "~dd~d~[~:;~:*~@d~]~4@*~{ ~a~} damage~3@*~[~:;~:*, ~@d to hit~]"
	 atk))

(defun get-direction (&optional input)
  (unless input
    (setf input (custom-read-char)))
  (let ((direction (assoc input `((#\h ,+left+)
				  (#\l ,+right+)
				  (#\k ,+up+)
				  (#\j ,+down+)
				  (#\y (-1 . -1))
				  (#\u (1 . -1))
				  (#\b (-1 . 1))
				  (#\n (1 . 1))))))
    (when direction
      (cadr direction))))
      
(defmacro with-direction (no &body body)
  `(progn
     (print-to-screen "enter a direction: ")
     (let ((direction (get-direction)))
       (if direction
	   (progn ,@body)
	   ,no))))
