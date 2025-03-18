(defparameter *board-size* '(60 . 20))
(defparameter *layers* '())
(defparameter *current-layer* (make-layer))
(defparameter *current-depth* 0)

(defun solid-actors ()
  (layer-solid-actors *current-layer*))

(defun non-solid-actors ()
  (layer-non-solid-actors *current-layer*))

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

(defun solid (pos)
  (gethash pos (solid-actors)))

(defun (setf solid) (value pos)
  (setf (gethash pos (solid-actors)) value))

(defun remove-solid (pos)
  (remhash pos (solid-actors)))

(defun non-solid (pos)
  (gethash pos (non-solid-actors)))

(defun (setf non-solid) (value pos)
  (setf (gethash pos (non-solid-actors)) value))

(defun remove-non-solid (pos)
  (remhash pos (non-solid-actors)))
    
(defun contents (pos)
  (or (solid pos) (non-solid pos)))

(defun down-ladder-pos ()
  (layer-down-ladder-pos *current-layer*))

(defun up-ladder-pos ()
  (layer-up-ladder-pos *current-layer*))

(defun spawn-object (pos spawn-list)
  (funcall (car (eval-weighted-list spawn-list)) pos))

(defun populate-dungeon (region spawn-list sparseness)
  (loop for cell in region
	when (= (random sparseness) 0)
	  do (spawn-object cell spawn-list)))

(defun place-walls (cells)
  (loop for x from -1 to (1+ (car *board-size*))
	do (loop for y from -1 to (1+ (cdr *board-size*))
		 ;; cell is not on board
		 unless (member (cons x y) cells :test #'equal)
		   ;; cell is next to board
		   when (loop for direction in +directions+
				thereis (member (vec+ (cons x y) direction) cells :test #'equal))
		     ;; put a wall down
		     do (setf (solid (cons x y)) 'wall))))

(defun initialize-board (spawn-list)
  (let* ((dungeon (generate-dungeon *board-size* 4))
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
    (populate-dungeon cells spawn-list 20)
    cells))

(defun add-layer (spawn-list)
  (let ((*current-layer* (make-layer)))
    (push *current-layer* *layers*)
    (initialize-board spawn-list)))

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
