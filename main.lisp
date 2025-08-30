(in-package #:gas)

;;; TODO
;;; Add translational camera
;;; Compute new positions and velocity

;;; ==== UTILS ====
(defun rcos (r theta)
  (* r (cos theta)))

(defun rsin (r theta)
  (* r (sin theta)))

(defmacro bind-v3-coords ((x y z) v &body body)
  `(let* ((,x (vx3 ,v))
	  (,y (vy3 ,v))
	  (,z (vz3 ,v)))
     ,@body))

(defun pow2 (x)
  (expt x 2))


(defun wrap (x min max)
  (let* ((range (- max min))
         (value (mod (- x min) range)))
    (+ value min)))

;;; ==== PHYSICS ====
(defparameter *N* 40) ;; number of particles
(defparameter *box* 40.0) ;; box size 
(defparameter *eps* 5.0) ;; epsilon parameter for LJ
(defparameter *sigma* 1.0) ;; sigma parameter for LJ

(defstruct particle
  (mass 1.0)
  (pos (vec 0.0 0.0 0.0))
  (vel (vec 0.0 0.0 0.0))
  (force-applied (vec 0.0 0.0 0.0)))

(defparameter *particles*
  (make-array *N*
	      :initial-contents (loop repeat *N*
				      collect (make-particle))))


(defun F-1=>2 (p1 p2)
  (let* ((dr (v- (particle-pos p2) (particle-pos p1)))
	 (r (vlength dr)))
    (let ((coef (* 24 *eps* (/ 1 (* r r))
		   (- (* 2 (expt (/ *sigma* r) 12))
		      (*   (expt (/ *sigma* r) 6))))))
      (v* coef dr))))


(defun wrap-pos (p)
  (bind-v3-coords (x y z) (particle-pos p)
	       (let* ((half (/ *box* 2))
		     (-half (* -1 half)))
		 (setf (particle-pos p)
		       (vec (wrap x -half half)
			    (wrap y -half half)
			    (wrap z -half half))))))

(defun init-pos-particles ()
  (let ((half (/ *box* 2)))
    (loop for i below *N*
	  for p = (aref *particles* i)
	  do (setf (particle-pos p)
		   (vec (- (random *box*) half)
			(- (random *box*) half)
			(- (random *box*) half))))))

;;; ==== CAMERA ====
(defparameter *orbit-angle* 1.04)
(defparameter *orbit-radius* 10.0)

(defun project-camera ()
  (vec (rcos *orbit-radius* *orbit-angle*)
       (rsin *orbit-radius* *orbit-angle*)
       0.0))

(defun init-camera ()
  (setf *orbit-angle* 1.04)
  (setf *orbit-radius* 10.0)
  (make-camera3d :position (project-camera)
		 :target (vec 0.0 0.0 0.0)
		 :up (vec 0.0 0.0 1.0)
		 :fovy 60.0
		 :projection :camera-perspective))

(defparameter *camera* (init-camera))


;;; ==== INPUT ====
(defun handle-input ()
  ;; orbit mode
  (when (is-key-down :key-a)
    (decf *orbit-angle* 0.2))
  (when (is-key-down :key-d)
    (incf *orbit-angle* 0.2))
  (when (is-key-down :key-w)
    (decf *orbit-radius* 0.5)
    (when (<= *orbit-radius* 0.0)
      (setf *orbit-radius* 0.0)))
  (when (is-key-down :key-s)
    (incf *orbit-radius* 0.5)))



;;; ==== SCENE ====
(defun draw-axes (&optional (L 10.0))
  (draw-line-3d (vec 0.0 0.0 0.0) (vec L 0.0 0.0) :blue)
  (draw-line-3d (vec 0.0 0.0 0.0) (vec 0.0 L 0.0) :red)
  (draw-line-3d (vec 0.0 0.0 0.0) (vec 0.0 0.0 L) :green))

(defun draw-particles ()
  (loop for i from 0 below *N*
	for pos = (particle-pos (aref *particles* i))
	do (draw-sphere pos 0.5 :gold)
	   (draw-sphere-wires pos 0.5 5 10 :black)))



(defun frame ()
  (with-drawing
    (handle-input)
    (setf (camera3d-position *camera*) (project-camera))
    (clear-background :black)
    (draw-text (format nil "dt = ~5,4f s" (get-frame-time))
	       130 360 20 :blue)
    ;; (draw-fps 20 20)

    ;; moving particles at random along z 
    ;; (let* ((i (random *N*))
    ;; 	   (p (aref *particles* i)))
    ;;   (setf (particle-pos p) (v+ (vec 0.0 0.0 1.0)
    ;; 				  (particle-pos p))))

    ;; reset forces
    (loop for i from 0 below *N*
	  for p = (aref *particles* i)
	  do (setf (particle-force-applied p)
		   (vec 0.0 0.0 0.0)))

    ;; computing forces
    (loop for i from 0 below (1- *N*) do
      (loop for j from (1+ i) below *N* do
	(let* ((p1 (aref *particles* i))
               (p2 (aref *particles* j))
               (fij (F-1=>2 p1 p2)))              
	  (setf (particle-force-applied p1)
		(v+ (particle-force-applied p1) fij))
	  (setf (particle-force-applied p2)
		(v- (particle-force-applied p2) fij)))))

    ;; update pos,vel
    ;; update vel
    (loop for i from 0 below *N*
	  for p = (aref *particles* i)
	  with dt = (get-frame-time)
	  do (setf (particle-vel p)
		   (v+ (particle-vel p)
		       (v* dt
			   (v/ (particle-force-applied p)
			       (particle-mass p))))))

    ;; update pos
    (loop for i from 0 below *N*
	  for p = (aref *particles* i)
	  with dt = (get-frame-time)
	  do (setf (particle-pos p)
		   (v+ (particle-pos p)
		       (v* dt
			   (particle-vel p)))))


    ;; check bounds
    (loop for i below *N*
	  for p = (aref *particles* i)
	  do (wrap-pos p))
    (with-mode-3d (*camera*)
      (draw-particles)
      (draw-axes))))

;;; ==== MAIN ====
(defun main ()
  (let* ((screen-width 400)
         (screen-height 400)
	 (screen-title "Argon gas"))
    ;; initialization
    (init-camera)
    (init-pos-particles)

    (loop for i from 0 below *N*
	  for p = (aref *particles* i)
	  do (setf (particle-vel p)
		   (vec (random 3) (random 3) (random 3))))
    
    (with-window
	(screen-width screen-height screen-title)
      (set-target-fps 60)
      (loop until (window-should-close)
	    do (frame)))))
