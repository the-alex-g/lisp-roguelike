(defun shuffle (array)
  (loop for x below 256
	do (let ((index (random (- 256 x)))
		 (value (elt array x)))
	     (setf (elt array x) (elt array index))
	     (setf (elt array index) value)))
  array)

(defun make-permutation ()
  (let ((temp (make-array 256))
	(array (make-array 512)))
    (loop for x below 256
	  do (setf (elt temp x) x))
    (shuffle temp)
    (loop for x below 256
	  do (setf (elt array (+ 256 x)) (elt temp x))
	  do (setf (elt array x) (elt temp x)))
    array))

(defparameter *permutation* (make-permutation))

(defun get-value (x y)
  (elt *permutation* (+ y (elt *permutation* x))))

(defun constant-vector (value)
  (let ((radians (* pi (logand value 3) 0.5)))
    (cons (- (cos radians) (sin radians))
	  (+ (cos radians) (sin radians)))))

(defun vector-from-pos (x y)
  (constant-vector (get-value x y)))

(defun fade (v)
  (* (+ (* (- (* 6 v) 15) v) 10) v v v))

(defun lerp (a b weight)
  (+ a (* weight (- b a))))

(defun calculate-dot (a b vec)
  (+ (* a (car vec))
     (* b (cdr vec))))

(defun noise (x y &key (wavelength 20) (normalize t))
  (setf x (/ x wavelength))
  (setf y (/ y wavelength))
  (let* ((xf (- x (floor x)))
	 (yf (- y (floor y)))
	 (v (fade yf)))
    (setf x (logand (floor x) 255))
    (setf y (logand (floor y) 255))
    (let ((result (lerp (lerp (calculate-dot xf yf (vector-from-pos x y))
			      (calculate-dot xf (1- yf) (vector-from-pos x (1+ y)))
			      v)
			(lerp (calculate-dot (1- xf) yf (vector-from-pos (1+ x) y))
			      (calculate-dot (1- xf) (1- yf) (vector-from-pos (1+ x) (1+ y)))
			      v)
			(fade xf))))
      (if normalize
	  (/ (1+ result) 2)
	  result))))

(defun vec-noise (vec &key (wavelength 20) (normalize t))
  (noise (car vec) (cdr vec) :wavelength wavelength :normalize normalize))

(defun display-noise (&key (size '(200 . 50)) (wavelength 20))
  (loop for y below (cdr size)
	do (format t "~{~a~}~%"
		   (loop for x below (car size)
			 collect (let ((noise (noise x y :wavelength wavelength)))
				   (apply-color #\space
						(round (lerp 232 255 noise))
						:bg t))))))
