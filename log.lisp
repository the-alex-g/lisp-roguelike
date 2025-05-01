(defparameter *log* '(nil "welcome to this game!"))
(defparameter *max-log-size* 100)
(defparameter *has-alert-p* nil)

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
  (setf (cdr *log*) '())
  (when *has-alert-p*
    (custom-read-char)
    (setf *has-alert-p* nil)))

(defun print-to-log (control-string &rest args)
  (setf (cdr *log*)
	(append (cdr *log*)
		(list (apply #'log-to-string control-string args)))))

(defun print-history ()
  (loop for item in (car *log*)
	do (print-to-screen "~a~%" item)))

(defun flag-message (color &key (require-input nil))
  (setf (cdr *log*)
	(do ((messages (cdr *log*) (cdr messages))
	     (new-messages nil (cons (if (cdr messages)
					 (car messages)
					 (apply-color (car messages) color))
				     new-messages)))
	    ((not messages) (reverse new-messages))))
  (when require-input
    (setf *has-alert-p* t)))

(defun flag-alert ()
  (flag-message 'red :require-input t))

(defun flag-warning ()
  (flag-message 'yellow-4))
