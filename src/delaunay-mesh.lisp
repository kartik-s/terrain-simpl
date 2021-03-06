;;;; an implementation of an incremental Delaunay triangulation

(in-package :delaunay-mesh)

(defstruct (delaunay-mesh (:constructor %make-delaunay-mesh))
  starting-edge)

(defun make-delaunay-mesh (&key top-left bottom-right)
  (let* ((bottom-left (vec2 (vec2-x top-left) (vec2-y bottom-right)))
	 (top-right (vec2 (vec2-x bottom-right) (vec2-y top-left)))
	 (e1 (make-edge :origin top-left :dest bottom-left))
	 (e2 (make-edge :origin bottom-left :dest bottom-right))
	 (e3 (make-edge :origin bottom-right :dest top-right))
	 (e4 (make-edge :origin top-right :dest top-left)))
    (splice (sym e1) e2)
    (splice (sym e2) e3)
    (splice (sym e3) e4)
    (splice (sym e4) e1)
    (let ((diag (connect (sym e1) (sym e2))))
      (set-lface-data diag `(:edge ,diag))
      (set-lface-data (sym diag) `(:edge ,(sym diag)))
      (%make-delaunay-mesh :starting-edge diag))))

(defun connect (edge-a edge-b)
  (let ((e (make-edge :origin (dest edge-a) :dest (origin edge-b))))
    (splice e (lnext edge-a))
    (splice (sym e) edge-b)
    e))

(defun remove-edge (edge)
  (splice edge (oprev edge))
  (splice (sym edge) (edge-walk (sym oprev) edge)))

(defun swap (edge)
  (let ((a (oprev edge))
	(b (edge-walk (sym oprev) edge))
	(face0-data (lface-data edge))
	(face1-data (lface-data (sym edge))))
    (splice edge a)
    (splice (sym edge) b)
    (splice edge (lnext a))
    (splice (sym edge) (lnext b))
    (setf (origin edge) (dest a))
    (setf (origin (sym edge)) (dest b))
    (set-lface-data edge face0-data)
    (set-lface-data (sym edge) face1-data)))

(defun approx-equal (p0 p1)
  (< (vec2-dist p0 p1) *epsilon*))

;; return a positive value if points a, b, and c
;; are in CCW order, a negative value if in CW order,
;; and zero if they are collinear
(defun orient-2d (point-a point-b point-c)
  (let ((ax (vec2-x point-a))
	(ay (vec2-y point-a))
	(bx (vec2-x point-b))
	(by (vec2-y point-b))
	(cx (vec2-x point-c))
	(cy (vec2-y point-c)))
    (- (* (- ax cx) (- by cy))
       (* (- ay cy) (- bx cx)))))

;; tolerance for numeric comparisons
(defparameter *epsilon* 1e-3)

(defun right-of-p (point edge)
  (> (orient-2d point (dest edge) (origin edge)) *epsilon*))

(defun left-of-p (point edge)
  (< (orient-2d point (dest edge) (origin edge)) (- *epsilon*)))

(defun on-edge-p (point edge)
  (< (abs (- (vec2-dist (origin edge) (dest edge))
	     (+ (vec2-dist (origin edge) point)
		(vec2-dist point (dest edge)))))
     *epsilon*))

(defun in-circle-p (a b c d)
  (let* ((adx (- (vec2-x a) (vec2-x d)))
	 (ady (- (vec2-y a) (vec2-y d)))
	 (bdx (- (vec2-x b) (vec2-x d)))
	 (bdy (- (vec2-y b) (vec2-y d)))
	 (cdx (- (vec2-x c) (vec2-x d)))
	 (cdy (- (vec2-y c) (vec2-y d)))
	 (ab-det (- (* adx bdy) (* bdx ady)))
	 (bc-det (- (* bdx cdy) (* cdx bdy)))
	 (ca-det (- (* cdx ady) (* adx cdy)))
	 (a-lift (+ (* adx adx) (* ady ady)))
	 (b-lift (+ (* bdx bdx) (* bdy bdy)))
	 (c-lift (+ (* cdx cdx) (* cdy cdy))))
    (> (+ (* a-lift bc-det) (* b-lift ca-det) (* c-lift ab-det))
       0)))

(defun locate-point (mesh point)
  (labels ((locate (edge)
	     (cond ((or (approx-equal point (origin edge))
			(approx-equal point (dest edge)))
		    edge)
		   ((right-of-p point edge) (locate (sym edge)))
		   ((not (right-of-p point (onext edge))) (locate (onext edge)))
		   ((not (right-of-p point (dprev edge))) (locate (dprev edge)))
		   (t edge))))
    (locate (delaunay-mesh-starting-edge mesh))))

(defun insert-point (mesh point &optional start-edge)
  (labels ((face-edge ()
	     (let* ((edge (or start-edge (locate-point mesh point)))
		    (on-edge (find-if #'(lambda (e) (on-edge-p point e))
				      (lface-edges edge))))
	       (cond ((null on-edge) edge)
		     ((or (approx-equal point (origin on-edge))
			  (approx-equal point (dest on-edge)))
		      nil)
		     (t (let ((tmp (oprev on-edge)))
			  (remove-edge on-edge)
			  tmp)))))
	   (insert-spoke-edges (first-face-edge)
	     (let* ((first-origin (origin first-face-edge))
		    (first-spoke (sym (make-edge :origin first-origin :dest point))))
	       (splice (sym first-spoke) first-face-edge)
	       (values first-spoke
		       (loop for spoke-sym = (sym first-spoke) then (connect face-edge (sym spoke-sym))
			     for face-edge = first-face-edge then (oprev spoke-sym)
			     until (approx-equal (dest face-edge) first-origin)
			     finally (return face-edge)))))
	   (fix-suspect-edges (first-origin suspect-edge)
	     (let ((tmp (oprev suspect-edge)))
	       (cond ((and (right-of-p (dest tmp) suspect-edge)
			   (in-circle-p (origin suspect-edge) (dest tmp)
					(dest suspect-edge) point))
		      (swap suspect-edge)
		      (fix-suspect-edges first-origin (oprev suspect-edge)))
		     ((approx-equal (origin suspect-edge) first-origin) nil)
		     (t (fix-suspect-edges first-origin
					   (edge-walk (onext lprev) suspect-edge)))))))
    (let ((first-face-edge (face-edge)))
      (cond ((null first-face-edge) (values nil nil))
	    (t (setf (delaunay-mesh-starting-edge mesh) first-face-edge)
	       (multiple-value-bind (first-spoke-edge suspect-edge)
		   (insert-spoke-edges first-face-edge)
		 (fix-suspect-edges (origin first-face-edge) suspect-edge)
		 ;; collect old face data
		 (let ((old-face-data (make-hash-table :test #'eql)))
		   (loop for oedge in (origin-edges first-spoke-edge) do
		     (loop for fedge in (lface-edges oedge) do
		       (when (not (null (lface-data fedge)))
			 (setf (gethash (lface-data fedge) old-face-data) t)))
		     (set-lface-data oedge `(:edge ,oedge)))
		   (values first-spoke-edge old-face-data))))))))

(defun all-faces (mesh)
  (let ((faces (make-array 1 :fill-pointer 0 :adjustable t))
	(visited-edges (make-hash-table :test #'eql))
	(pending-edges (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop for face-edge = (delaunay-mesh-starting-edge mesh) then (vector-pop pending-edges) do
      (cond ((gethash face-edge visited-edges) nil)
	    (t (vector-push-extend face-edge faces)
	       (dolist (edge (lface-edges face-edge))
		 (setf (gethash edge visited-edges) t)
		 (unless (gethash (sym edge) visited-edges)
		   (vector-push-extend (sym edge) pending-edges)))))
      (when (zerop (length pending-edges))
	(return faces)))))
