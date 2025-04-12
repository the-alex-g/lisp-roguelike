;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; https://roguebasin.com/index.php/Markov_chains_name_generator_in_Python ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *markov-table* (make-hash-table :test 'equal))
(defparameter *places* (list "Adara" "Adena" "Adrianne" "Alarice" "Alvita" "Amara" "Ambika" "Antonia"
 "Aaraceli" "Balandria" "Basha" "Beryl" "Bryn" "Callia" "Caryssa" "Cassandra"
 "Casondrah" "Chatha" "Ciara" "Cynara" "Cytheria" "Dabria" "Darcei" "Deandra"
 "Deirdre" "Delores" "Desdomna" "Devi" "Drucilla" "Duvessa" "Ebony" "Fantine"
 "Fuscienne" "Gabi" "Gallia" "Hanna" "Hedda" "Jerica" "Jetta" "Joby" "Kacila"
 "Kagami" "Kala" "Kallie" "Keelia" "Kerry" "Kerry-Ann" "Kimberly" "Killian"
 "Kory" "Lility" "Lysha" "Mercedes" "Mia" "Maura" "Quella" "Riona" "Safiya"
 "Salina" "Severin" "Sidonia" "Sirena" "Solita" "Tempest" "Thea" "Treva"
 "Trista" "Vala" "Winta"))

(defun initialize-markov-table ()
  (loop for place-name in *places*
	do (do ((char-list (coerce place-name 'list) (cdr char-list))
		(sequence nil
			  (list (if (< (length sequence) 2)
				    sequence
				    (cadr sequence))
				(char-downcase (car char-list)))))
	       ((not char-list) (push nil (gethash sequence *markov-table*)))
	     (push (char-downcase (car char-list)) (gethash sequence *markov-table*)))))

(defun get-markov (a b)
  (gethash (list a b) *markov-table*))

(defun get-next (sequence)
  (let ((options (gethash sequence *markov-table*)))
    (when options
      (nth (random (length options)) options))))

(defun get-markov-name ()
  (let* ((next-char (get-next nil)))
    (do ((sequence (list nil next-char)
		   (list (cadr sequence) next-char))
	 (name (list next-char) (if next-char
				    (append name (list next-char))
				    name)))
	((or (> (length name) 9) (not next-char)) (coerce name 'string))
      (setf next-char (get-next sequence)))))

(defun print-markov-names (count)
  (loop repeat count
	do (format t "~:(~a~)~%" (get-markov-name))))

(initialize-markov-table)
