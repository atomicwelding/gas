(in-package #:gas)

;;; === UTILS ===
(defun rcos (r theta)
  (* r (cos theta)))

(defun rsin (r theta)
  (* r (sin theta)))

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


(defun draw-axes (&optional (L 10.0))
  (draw-line-3d (vec 0.0 0.0 0.0) (vec L 0.0 0.0) :blue)
  (draw-line-3d (vec 0.0 0.0 0.0) (vec 0.0 L 0.0) :red)
  (draw-line-3d (vec 0.0 0.0 0.0) (vec 0.0 0.0 L) :green))

(defun frame ()
  (with-drawing
    (handle-input)
    (setf (camera3d-position *camera*) (project-camera))
    (clear-background :black)
    (draw-fps 20 20)
    (with-mode-3d (*camera*)
      (draw-sphere (vec 0.0 0.0 0.0) 2.0 :gold)
      (draw-sphere-wires (vec 0.0 0.0 0.0) 2.0 5 10 :black)
      (draw-axes))))


;;; ==== MAIN ====
(defun main ()
  (let* ((screen-width 400)
         (screen-height 400)
	 (screen-title "Argon gas"))
    (with-window
	(screen-width screen-height screen-title)
      (set-target-fps 60)
      (loop until (window-should-close)
	    do (frame)))))
