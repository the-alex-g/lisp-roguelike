(load "noise.lisp")

(defparameter *board-size* '(60 . 20))
(defparameter *layers* '())
(defparameter *current-layer* (make-layer))
(defparameter *current-depth* 0)
(defparameter *terrain-costs* (make-hash-table))
(defparameter *terrain-colors* (make-hash-table))
(defparameter *terrain-characters* (make-hash-table))
(defparameter *neighbors-required* (make-hash-table))

(defun defterrain (name character &key (cost 1) (color 255))
  (setf (gethash name *terrain-characters*) character)
  (setf (gethash name *terrain-costs*) cost)
  (setf (gethash name *terrain-colors*) (color color))
  name)

(defun solid-actors ()
  (layer-solid-actors *current-layer*))

(defun non-solid-actors ()
  (layer-non-solid-actors *current-layer*))

(defun terrain-table ()
  (layer-terrain *current-layer*))

(defun glowing-actors ()
  (layer-glowing-actors *current-layer*))

(defun (setf solid-actors) (value)
  (setf (layer-solid-actors *current-layer*) value))

(defun (setf non-solid-actors) (value)
  (setf (layer-non-solid-actors *current-layer*) value))

(defun (setf glowing-actors) (value)
  (setf (layer-glowing-actors *current-layer*) value))

(defun add-glowing (actor)
  (push actor (glowing-actors)))

(defun remove-glowing (actor)
  (setf (glowing-actors) (remove actor (glowing-actors))))

(defmacro def-layer-accessors (name source)
  `(progn (defun ,name (pos)
	    (gethash pos (,source)))
	  (defun (setf ,name) (value pos)
	    (setf (gethash pos (,source)) value))
	  (defun ,(read-from-string (format nil "remove-~a" name)) (pos)
	    (remhash pos (,source)))))

(def-layer-accessors terrain terrain-table)
(def-layer-accessors solid solid-actors)
(def-layer-accessors non-solid non-solid-actors)

(defun contents (pos &key (all nil))
  (if all
      (list (solid pos)
	    (non-solid pos)
	    (terrain pos))
      (or (solid pos) (non-solid pos))))

(defun down-ladder-pos ()
  (layer-down-ladder-pos *current-layer*))

(defun up-ladder-pos ()
  (layer-up-ladder-pos *current-layer*))

(defun get-spawn-position (cells restrictions &key start)
  (unless start
    (setf start (randnth cells)))
  (flet ((adjacent-empty-cells (pos)
	   (loop for direction in +directions+
		 when (let ((new-pos (vec+ direction pos)))
			(and (member new-pos cells :test #'equal)
			     (not (solid pos))))
		   sum 1)))
    (flood-fill start (t (when (cond ((numberp restrictions)
				      (>= (adjacent-empty-cells current)
					  restrictions))
				     ((listp restrictions)
				      (member (adjacent-empty-cells current) restrictions)))
			   current))
      (or result
	  start))))

(defun spawn-object (pos spawn-list)
  (funcall (car (eval-weighted-list spawn-list)) pos))

(defun populate-dungeon (region spawn-list sparseness)
  (loop for cell in region
	when (= (random sparseness) 0)
	  do (spawn-object cell spawn-list)))

(defun make-wall (pos)
  (setf (solid pos) 'wall))

(defun place-walls (cells)
  (loop for x from -1 to (1+ (car *board-size*))
	do (loop for y from -1 to (1+ (cdr *board-size*))
		 ;; cell is not on board
		 unless (member (cons x y) cells :test #'equal)
		   ;; cell is next to board
		   when (loop for direction in +directions+
				thereis (member (vec+ (cons x y) direction) cells :test #'equal))
		     ;; put a wall down
		     do (make-wall (cons x y)))))

(defun generate-terrain (cells)
  (loop for cell in cells
	do (let ((noise (vec-noise cell :wavelength 5)))
	     (setf (terrain cell)
		   (cond ((< noise 0.25) 'difficult)
			 (t 'standard))))))

(defun furnish-dungeon (rooms furniture)
  (mapc (lambda (room)
	  (loop repeat (random (max 1 (floor (/ (length room) 8))))
		do (let ((function (car (eval-weighted-list furniture))))
		     (funcall function
			      (get-spawn-position
			       room
			       (gethash function *neighbors-required* 0))))))
	rooms))

(defun initialize-board (hazards furniture)
  (let* ((dungeon (generate-dungeon *board-size* 4))
	 (rooms (car dungeon))
	 (cells (pos-flatten dungeon))
	 (up-ladder-pos (randnth cells))
	 (down-ladder-pos (randnth (loop for cell in cells
					 when (>= (distance cell up-ladder-pos) 5)
					   collect cell))))
    (setf (direction (make-ladder up-ladder-pos)) -1)
    (setf (direction (make-ladder down-ladder-pos)) 1)
    (setf (layer-up-ladder-pos *current-layer*) up-ladder-pos)
    (setf (layer-down-ladder-pos *current-layer*) down-ladder-pos)
    (place-walls cells)
    (furnish-dungeon rooms furniture)
    (populate-dungeon cells hazards 20)
    (generate-terrain cells)
    cells))

(defun add-layer (hazards furniture)
  (let ((*current-layer* (make-layer)))
    (push *current-layer* *layers*)
    (initialize-board hazards furniture)))

(defun place (obj pos &key (solid t))
  (flood-fill pos (t (unless (occupiedp current) current)
		     :stop-for-occupied nil
		     :solid solid)
    (when result
      (if solid
	  (setf (solid result) obj)
	  (setf (non-solid result) obj))
      (setf (pos obj) result)))
  (when (> (illumination obj) 0)
    (add-glowing obj))
  obj)

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

(defun get-best-direction (pos heuristic)
  (do ((directions +directions+ (cdr directions))
       (min-cost 100)
       (direction +zero+))
      ((not directions) (vec+ pos direction))
    (let* ((new-pos (vec+ pos (car directions)))
	   (cost (funcall heuristic new-pos)))
      (when (and (not (solid new-pos))
		 (< (funcall heuristic new-pos) min-cost))
	(setf min-cost cost)
	(setf direction (car directions))))))
