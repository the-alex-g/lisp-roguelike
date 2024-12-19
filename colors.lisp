(load "./utils.lisp")

(defparameter *styles* (make-hash-table))
(defparameter *color-list* nil)
(defparameter *default-style* 0)

(mapc (lambda (color-pair)
	(setf (gethash (car color-pair) *styles*) (cadr color-pair))
	(push (car color-pair) *color-list*))
      '((white 97) (red 31) (green 32) (orange 33) (blue 34) (purple 35)
	(teal 36) (grey 90) (dark-red 91) (pale-green 92) (light-orange 93)
	(sky-blue 94) (dark-purple 95) (dark-teal 96)))
(mapc (lambda (color-pair)
	(setf (gethash (car color-pair) *styles*) (cadr color-pair)))
      '((bold 1) (italic 3) (uline 4) (blink 5) (black-on-white 7) (striken 9)
	(2uline 21) (oline 53) (none 0)))

(defun apply-default-style (to-print)
  (let ((result (format to-print "~c[0;~dm" #\esc *default-style*)))
    (if to-print (force-output))
    result))

(defun apply-color (to color &rest colors)
  (format nil "~c[~d~{;~d~}m~a~a"
	  #\esc
	  (gethash color *styles*)
	  (mapcar (lambda (c) (gethash c *styles*)) colors)
	  to
	  (apply-default-style nil)))

(defun apply-background (to color)
  (format nil "~c[~dm~a~a" #\esc (+ (gethash color *styles*) 10)
	  to (apply-default-style nil)))

(defun random-color ()
  (randnth *color-list*))

(defmacro with-color-applied (color &body body)
  `(let (result)
     (let ((*default-style* (gethash ,color *styles*)))
       (apply-default-style t)
       (setf result (progn ,@body)))
     (apply-default-style t)
     result))
