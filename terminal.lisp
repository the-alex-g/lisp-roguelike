(defparameter *cursor-offset* '(2 . 2))
(defparameter *colors* '((black 0)
			 (red 1)
			 (green 2)
			 (orange 3)
			 (blue 4)
			 (purple 5)
			 (teal 6)
			 (white 7)
			 (grey 8)
			 (dark-red 9)
			 (light-green 10)
			 (yellow 11)
			 (sky-blue 12)
			 (dark-purple 13)
			 (dark-teal 14)))

(defun calculate-color (r g b)
  (+ 16 (* r 36) (* g 6) b))

(labels ((color-name (color number)
	   (read-from-string (format nil "~a-~d" color number)))
	 (add-color (name number)
	   (push (list name number) *colors*)))
  (loop for x from 1 to 5
	do (add-color (color-name 'red (1- x))
		      (calculate-color x 0 0))
	do (add-color (color-name 'blue (1- x))
		      (calculate-color 0 0 x))
	do (add-color (color-name 'green (1- x))
		      (calculate-color 0 x 0))
	do (add-color (color-name 'purple (1- x))
		      (calculate-color x 0 x))
	do (add-color (color-name 'yellow (1- x))
		      (calculate-color x x 0))
	do (add-color (color-name 'cyan (1- x))
		      (calculate-color 0 x x)))
  (loop for x from 232 to 255
	unless (= x 235) ;; same color as terminal background and color code 0
	  do (add-color (color-name 'grey (- x 232)) x)))

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
