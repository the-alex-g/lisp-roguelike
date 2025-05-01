(defparameter +left+ '(-1 . 0))
(defparameter +right+ '(1 . 0))
(defparameter +up+ '(0 . -1))
(defparameter +down+ '(0 . 1))
(defparameter +zero+ '(0 . 0))
(defparameter +directions+ (list +up+ +right+ +down+ +left+
				 '(1 . 1) '(1 . -1) '(-1 . 1) '(-1 . -1)))
(defparameter +direction-names+ (make-hash-table :test #'equal))
(defparameter *log* '(nil "welcome to this game!"))
(defparameter *max-log-size* 100)
(defparameter *in-terminal* (handler-case (sb-posix:tcgetattr 0)
			      (error () nil)))
(defparameter *fake-input* nil)
(defparameter *terminal-size* '(80 . 31))

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
  (setf (cdr *log*)
	(append (cdr *log*)
		(list (apply #'log-to-string control-string args)))))

(defun print-log ()
  (loop for item in (cdr *log*)
	do (format t "~a~%" item))
  (setf (car *log*)
	(do ((item-list (reverse (append (car *log*) (cdr *log*))) (cdr item-list))
	     (new-list nil (cons (car item-list) new-list))
	     (index 0 (1+ index)))
	    ((or (not item-list)
		 (>= index *max-log-size*))
	     new-list)))
  (setf (cdr *log*) '()))

(defun print-to-screen (control-string &rest args)
  (princ (apply #'log-to-string control-string args))
  (force-output))

(defun print-history ()
  (loop for item in (car *log*)
	do (print-to-screen "~a~%" item)))

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

(defun manhattan (a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))

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

(defun string-length (string)
  (do ((chars (coerce string 'list) (cdr chars))
       (collecting t (let ((new-val (cond ((eq (car chars) #\esc)
					   nil)
					  ((eq (car chars) #\m)
					   t)
					  (t
					   collecting))))
		       (unless (eq new-val collecting)
			 (decf size)
			 (incf trimmed-chars))
		       new-val))
       (trimmed-chars 0 (if collecting trimmed-chars (1+ trimmed-chars)))
       (size 0 (if collecting (1+ size) size)))
      ((not chars) (values size trimmed-chars))))

(defun max-dimensions (string-list)
  (do ((max-width 0)
       (max-size 0)
       (items string-list (cdr items)))
      ((not items) (values max-size max-width))
    (multiple-value-bind (size trim-size) (string-length (car items))
      (setf max-width (max max-width (+ size trim-size)))
      (setf max-size (max max-size size)))))

(defun column-print (list-of-items
		     &key
		       (indexp nil) (columns 2) (fit-screen t)
		       (print-function #'print-to-screen))
  (multiple-value-bind (item-width tab-length) (max-dimensions list-of-items)
    (if (> item-width (/ (car *terminal-size*) 2))
	(setf columns 1)
	(when fit-screen
	  (setf columns
		(floor (/ (- (car *terminal-size*) (if indexp 2 0))
			  item-width)))))
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

(defun confirmp (&rest args)
  (print-to-screen "~:[~;~:*~? ~]" (car args) (cdr args))
  (labels ((get-answer ()
	     (print-to-screen "(y or n)~%")
	     (let ((answer (char-downcase (custom-read-char))))
	       (cond ((eq answer #\y)
		      t)
		     ((eq answer #\n)
		      nil)
		     (t
		      (get-answer))))))
    (get-answer)))

(defun get-number-input (&key (min 0 minp) (max 0 maxp))
  (print-to-screen "please enter a number~
                    ~:[~2*~; between ~d and ~d~]~:[~*~; greater than ~d~]~:[~*~; less than ~d~] "
		   (and minp maxp)
		   min max
		   (and minp (not maxp))
		   (1- min)
		   (and maxp (not minp))
		   (1+ max))
  (labels ((get-answer ()
	     (let* ((raw (read-line))
		    (number (parse-integer raw :junk-allowed t)))
	       (if number
		   number
		   (progn (print-to-screen "~%please enter a number ")
			  (get-answer))))))
    (cond ((and maxp minp)
	   (max min (min max (get-answer))))
	  (maxp
	   (min max (get-answer)))
	  (minp
	   (max min (get-answer)))
	  (t
	   (get-answer)))))

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

(defun roll (num die &rest modifiers)
  (+ (loop repeat num
	   sum (1+ (random die)))
     (loop for m in modifiers
	   sum m)))

(defun priority-add (list new-item &optional (priority 0 priorityp))
  (unless priorityp
    (setf priority (car new-item))
    (setf new-item (cdr new-item)))
  (if list
      (do ((old-items list (cdr old-items))
	   (collectedp nil)
	   (new-items nil (cons (car old-items)
				(if (and (not collectedp)
					 (< priority (caar old-items)))
				    (progn (setf collectedp t)
					   (cons (cons priority new-item) new-items))
				    new-items))))
	  ((not old-items) (reverse (if collectedp
					new-items
					(cons (cons priority new-item) new-items)))))
      (list (cons priority new-item))))

(defun priority-append (list1 list2)
  (if (car list2)
      (priority-append (priority-add list1 (car list2)) (cdr list2))
      list1))

(defmacro flood-fill (start (value-to-store exit-condition
			     &key (solid t) (stop-for-occupied t) (go-until nil))
		      &body body)
  `(let ((cells (make-hash-table :test #'equal)))
     (setf (gethash ,start cells) t)
     (labels ((occupiedp (pos)
		(,(if solid 'solid 'non-solid) pos))
	      (valid-neighbor-p (pos)
		(and pos
		     (not (gethash pos cells))
		     (not (wallp (solid pos)))
		     ,(if stop-for-occupied
			  `(or (not (occupiedp pos)) (equal pos ,go-until))
			  t)))
	      (neighbors (pos)
		(loop for direction in +directions+
		      with neighbors = nil
		      do (let* ((cell-pos (vec+ pos direction)))
			   (when (valid-neighbor-p cell-pos)
			     (push cell-pos neighbors)))
		      finally (return neighbors)))
	      (iterate (frontier)
		(when (car frontier)
		  (let* ((current (car frontier))
			 (neighbors (neighbors current)))
		    (or ,exit-condition
			(progn
			  (mapc (lambda (n) (setf (gethash n cells) ,value-to-store)) neighbors)
			  (iterate (append (cdr frontier) neighbors))))))))
       (let ((result (iterate (list ,start))))
	 ,@body))))

(defgeneric stationaryp (obj)
  (:method (obj) obj))

(defun a-star (start end heuristic)
  (let ((cells (make-hash-table :test #'equal))
	(costs (make-hash-table :test #'equal)))
    (setf (gethash start cells) t)
    (setf (gethash start costs) 0)
    (labels ((valid-neighbor-p (cost pos)
	       (or (equal pos end)
		   (and pos
			(or (not (gethash pos costs))
			    (< cost (gethash pos costs)))
			(not (wallp (solid pos)))
			(not (stationaryp (solid pos))))))
	     (neighbors (of)
	       (loop for direction in +directions+
		     with neighbors = nil
		     do (let* ((cell-pos (vec+ (cdr of) direction))
			       (cell-cost (+ (gethash (cdr of) costs)
					     (funcall heuristic cell-pos))))
			  (when (valid-neighbor-p cell-cost cell-pos)
			    (push (cons cell-cost cell-pos) neighbors)))
		     finally (return neighbors)))
	      (iterate (frontier)
		(when (cdar frontier)
		  (let* ((current (cdar frontier))
			 (neighbors (neighbors (car frontier))))
		    (or (equal current end)
			(iterate (priority-append (cdr frontier)
						  (mapcar (lambda (n)
							    (setf (gethash (cdr n) cells) current)
							    (setf (gethash (cdr n) costs) (car n))
							    (cons (+ (car n) (manhattan (cdr n) end))
								  (cdr n)))
							  neighbors))))))))
      (values (if (iterate (list (cons 0 start)))
		  (do ((pos end (gethash pos cells))
		       (path nil (cons pos path)))
		      ((equal pos start) path))
		  (list start))
	      cells))))

(defun apply-color (arg color &key (bg nil)
				(function (lambda (&rest args) (apply #'format nil args))))
  (funcall function "~c[~d;5;~dm~a~0@*~c[40;37m" #\esc
	   (if bg 48 38) (if (numberp color) color (color color)) arg))

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

(defun random-direction (&key (zero t))
  (nth (random (if zero 9 8))
       (if zero
	   (cons +zero+ +directions+)
	   +directions+)))
