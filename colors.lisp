(load "./utils.lisp")

(defparameter *styles* (make-hash-table))
(defparameter *color-list* nil)

(loop for pair in '((white 0) (red 31) (green 32) (orange 33) (blue 34) (purple 35)
		    (teal 36) (grey 90) (dark-red 91) (pale-green 92) (light-orange 93)
		    (sky-blue 94) (dark-purple 95) (dark-teal 96))
      do (progn
	   (setf (gethash (car pair) *styles*) (cadr pair))
	   (push (car pair) *color-list*)))
(loop for pair in '((bold 1) (italic 3) (uline 4) (blink 5) (black-on-white 7)
		    (striken 9) (2uline 21) (oline 53))
      do (setf (gethash (car pair) *styles*) (cadr pair)))

(defun apply-color (to color &rest colors)
  (format nil "~c[~d~{;~d~}m~a~c[0m"
	  #\esc
	  (gethash color *styles*)
	  (mapcar (lambda (c) (gethash c *styles*)) colors)
	  to
	  #\esc))

(defun apply-background (to color)
  (format nil "~c[~dm~a~c[0m" #\esc (+ (gethash color *styles*) 10) to #\esc))

(defun random-color ()
  (randnth *color-list*))
