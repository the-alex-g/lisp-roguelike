(defun escape-code (code &rest args)
  (format t "~c[~{~d~^;~}~c" #\esc args code))

(defun position-cursor (x y)
  (escape-code #\H y x))

(defun position-cursor-list (pos)
  (position-cursor (1+ (car pos)) (1+ (cdr pos))))

(defun move-cursor-vertical (lines)
  (if (> lines 0)
      (escape-code #\B lines)
      (escape-code #\A (abs lines))))

(defun move-cursor-horizontal (columns)
  (if (> columns 0)
      (escape-code #\C columns)
      (escape-code #\D columns)))

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
