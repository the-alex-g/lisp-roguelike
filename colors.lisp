(load "./utils.lisp")

(defparameter +none+ 0)
(defparameter +bold+ 1)
(defparameter +italic+ 3)
(defparameter +uline+ 4)
(defparameter +blink+ 5)
(defparameter +black-on-white+ 7)
(defparameter +striken+ 9)
(defparameter +2uline+ 21)
(defparameter +black+ 30)
(defparameter +red+ 31)
(defparameter +green+ 32)
(defparameter +orange+ 33)
(defparameter +blue+ 34)
(defparameter +purple+ 35)
(defparameter +teal+ 36)
(defparameter +oline+ 53)
(defparameter +grey+ 90)
(defparameter +dark-red+ 91)
(defparameter +pale-green+ 92)
(defparameter +light-orange+ 93)
(defparameter +sky-blue+ 94)
(defparameter +dark-purple+ 95)
(defparameter +dark-teal+ 36)
(defparameter *color-name-list* '(+red+ +green+ +blue+ +purple+ +teal+ +grey+
				  +dark-red+ +pale-green+ +light-orange+ +orange+
				  +sky-blue+ +dark-purple+ +dark-teal+))

(defun apply-color (to color)
  (format nil "~c[~dm~a~c[0m" #\esc color to #\esc))

(defun apply-background (to color)
  (format nil "~c[~dm~a~c[0m" #\esc (+ color 10) to #\esc))

(defun random-color ()
  (eval (nth (random 13) *color-name-list*)))

(defun color-name (color-var)
  (string-trim "+" (log-to-string "~a" color-var)))

(defun random-color-name ()
  (color-name (nth (random 13) *color-name-list*)))
