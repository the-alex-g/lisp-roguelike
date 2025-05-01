(defparameter *cursor-offset* '(2 . 2))

(defun escape-code (code &rest args)
  (format t "~c[~{~d~^;~}~c" #\esc args code))

(defun move-cursor-vertical (lines)
  (if (> lines 0)
      (escape-code #\B lines)
      (escape-code #\A (abs lines))))

(defun move-cursor-horizontal (columns)
  (if (> columns 0)
      (escape-code #\C columns)
      (escape-code #\D columns)))

(defun position-cursor (x y)
  (escape-code #\H y x))

(defun position-cursor-list (pos)
  (setf pos (vec+ pos *cursor-offset*))
  (position-cursor (car pos) (cdr pos)))

(defun clear-screen ()
  (escape-code #\J 2)
  (position-cursor 0 0))

(defmacro with-cursor-saved (&body body)
  `(progn (escape-code #\s)
	  (unwind-protect
	       (progn ,@body)
	    (escape-code #\u))))

(defmacro with-color ((&rest codes) &body body)
  `(progn
     ,(if (listp codes)
	  `(apply #'escape-code #\m ',codes)
	  `(escape-code #\m ,codes))
     (unwind-protect
	  (progn ,@body)
       (escape-code #\m 0))))

(defun apply-default-colors ()
  (format t "~c[40;37m" #\esc))
