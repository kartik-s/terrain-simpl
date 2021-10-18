;;;; top-level terrain simplification routines

(in-package :terrain-simpl)

(sb-int:set-floating-point-modes :traps '())

(defstruct (height-field (:constructor %make-height-field))
  height
  width
  max-value
  data)

(defun make-height-field (&key filename)
  (let* ((file (pngload:load-file filename)))
    (%make-height-field :height (pngload:height file)
			:width (pngload:width file)
			:max-value (ash 1 (pngload:bit-depth file))
			:data (pngload:data file))))

(defun height-field-dimensions (height-field)
  (values (height-field-width height-field) (height-field-height height-field)))

(defun height-field-get (height-field point)
  (let ((data (height-field-data height-field)))
    (/ (float (aref data (vec2-y point) (vec2-x point) 0))
       (height-field-max-value height-field))))

(defstruct (terrain (:constructor %make-terrain))
  mesh)

(defun make-terrain (&key height-field goal-p)
  (multiple-value-bind (width height)
      (height-field-dimensions height-field)
    ;; create a new Delaunay triangulation consisting of two right triangles
    ;; with a common diagonal forming a rectangle with the same dimensions
    ;; as the provided height field
    (let ((mesh (make-delaunay-mesh :top-left (vec2 0 0) :bottom-right (vec2 (- width 1) (- height 1)))))
      ;; copy the height field values into the corners of the mesh
      ;; the height at a given point is stored in the 'data' field of
      ;; the edge whose origin is the point
      (dolist (edge (append (lface-edges (delaunay-mesh-starting-edge mesh))
			    (lface-edges (sym (delaunay-mesh-starting-edge mesh)))))
	(setf (odata edge) (height-field-get height-field (origin edge)))
	(setf (odata (sym edge)) (height-field-get height-field (dest edge))))
      ;; greedily insert points into the mesh until the specified goal is met
      (greedily-insert-points mesh height-field goal-p)
      (%make-terrain :mesh mesh))))

(defun greedily-insert-points (mesh height-field goal-p)
  (let ((candidate-points (make-priority-queue :greater-p #'(lambda (c0 c1)
							      (> (getf c0 :err) (getf c1 :err)))
					       :initial-size 20))
	(inserted-points (make-hash-table)))
    (labels ((next-candidate ()
	       (let ((candidate (remove-max candidate-points)))
		 (if (and (getf candidate :valid)
			  (not (gethash (vec2-hash-key (getf candidate :point))
					inserted-points)))
		     candidate
		     (next-candidate))))
	     (insert-candidate (candidate)
	       (multiple-value-bind (first-spoke-edge old-candidates)
		   (insert-point mesh (getf candidate :point) (getf candidate :edge))
		 ;; if the point was already in the mesh, then the insertion will
		 ;; return nil and we continue inserting candidates
		 (when (not (null first-spoke-edge))
		   ;; mark the candidate point as inserted
		   (setf (gethash (vec2-hash-key (getf candidate :point)) inserted-points) t)
		   ;; invalidate any candidate points which belonged to
		   ;; faces that were destroyed by the insertion
		   (loop for old-candidate being the hash-keys in old-candidates do
		     (setf (getf old-candidate :valid) nil))
		   ;; update the spoke edge endpoints, then find and
		   ;; insert the candidate points for the newly created
		   ;; mesh faces

		   (dolist (spoke-edge (origin-edges first-spoke-edge))
		     (setf (odata spoke-edge)
			   (height-field-get height-field (getf candidate :point)))
		     (setf (odata (sym spoke-edge))
			   (odata (edge-walk (sym onext) spoke-edge)))
		     (insert candidate-points
			     (find-face-candidate spoke-edge height-field inserted-points))))))
	     (on-boundary-p (point)
	       (let ((x (vec2-x point))
		     (y (vec2-y point)))
		 (multiple-value-bind (width height)
		     (height-field-dimensions height-field)
		   (or (= x 0) (= y 0) (= x width) (= y height)))))
	     (insert-points ()
	       (let* ((candidate (next-candidate))
		      (point (getf candidate :point)))
		 ;; if the candidate point lies on the boundary of the height field,
		 ;; then mark it as inserted but don't actually insert it
		 ;; TODO: change this behavior?
		 (cond ((on-boundary-p point)
			(setf (gethash (vec2-hash-key point)
				       inserted-points)
			      t)
			(insert-points))
		       ;; otherwise, insert the candidate point into the mesh
		       (t (insert-candidate candidate)
			  (if (funcall goal-p mesh candidate)
			      nil
			      (insert-points)))))))
      ;; find and insert the first two candidate points, one for each initial mesh face
      (insert candidate-points (find-face-candidate (delaunay-mesh-starting-edge mesh)
						    height-field inserted-points))
      (insert candidate-points (find-face-candidate (sym (delaunay-mesh-starting-edge mesh))
						    height-field inserted-points))
      ;; insert the best candidate points into the mesh until the provided goal is met
      (insert-points))))

(defun points-in-triangle (e0)
  (let ((points (make-array 1 :fill-pointer 0 :adjustable t))
	(verts (sort (list (origin e0) (origin (lnext e0)) (origin (lnext (lnext e0))))
		     #'(lambda (p0 p1)
			 (if (= (vec2-x p0) (vec2-x p1))
			     (< (vec2-y p0) (vec2-y p1))
			     (< (vec2-x p0) (vec2-x p1)))))))
    (destructuring-bind (p0 p1 p2) verts
      (let* ((slope-01 (vec2-slope p0 p1))
	     (slope-02 (vec2-slope p0 p2))
	     (slope-12 (vec2-slope p1 p2))
	     (p3 (vec2 (vec2-x p1)
		       (+ (vec2-y p0) (* slope-02 (- (vec2-x p1) (vec2-x p0)))))))
	;; fill left points
	(let ((bottom-slope slope-02)
	      (top-slope slope-01))
	  (when (< (vec2-y p1) (vec2-y p2))
	    (rotatef bottom-slope top-slope))
	  (loop for x from (vec2-x p0) to (vec2-x p1)
		for bottom-y = (vec2-y p0) then (+ bottom-y bottom-slope)
		for top-y = (vec2-y p0) then (+ top-y top-slope) do
		  (loop for y from (ceiling bottom-y) to top-y do
		    (vector-push-extend (vec2 x y) points))))
	;; fill right points
	(let ((bottom-slope slope-02)
	      (top-slope slope-12))
	  (when (< (vec2-y p1) (vec2-y p2))
	    (rotatef bottom-slope top-slope))
	  (loop for x from (vec2-x p2) downto (vec2-x p1)
		for bottom-y = (vec2-y p2) then (- bottom-y bottom-slope)
		for top-y = (vec2-y p2) then (- top-y top-slope) do
		  (loop for y from (ceiling bottom-y) to top-y do
		    (vector-push-extend (vec2 x y) points))))
	points))))

(defparameter *epsilon* 1e-3)

(defun eps-guard (x)
  (if (zerop x) *epsilon* x))

(defun face-normal (e0)
  (let* ((p0 (vec3 (vec2-x (origin e0)) (odata e0) (vec2-y (origin e0))))
	 (p1 (vec3 (vec2-x (dest e0)) (odata (sym e0)) (vec2-y (dest e0))))
	 (p2 (vec3 (vec2-x (dest (lnext e0))) (odata (edge-walk (lnext sym) e0)) (vec2-y (dest (lnext e0))))))
    (vec3-unit (vec3-cross (vec3-sub p1 p0) (vec3-unit (vec3-sub p2 p0))))) )

(defun find-face-candidate (face-edge height-field inserted-points)
  (labels ((height-interpolator (face-normal face-point)
	     (let* ((n-zy (/ (vec3-z face-normal) (eps-guard (vec3-y face-normal))))
		    (n-xy (/ (vec3-x face-normal) (eps-guard (vec3-y face-normal))))
		    (c (+ (* n-xy (vec3-x face-point))
			  (vec3-y face-point)
			  (* n-zy (vec3-z face-point)))))
	       #'(lambda (p)
		   (- c (+ (* n-xy (vec2-x p)) (* n-zy (vec2-y p))))))))
    (let* ((points (remove-if #'(lambda (p)
				  (gethash (vec2-hash-key p) inserted-points))
			      (points-in-triangle face-edge)))
	   (face-point (vec3 (vec2-x (origin face-edge)) (odata face-edge) (vec2-y (origin face-edge))))
	   (interpolate-height (height-interpolator (face-normal face-edge) face-point))
	   (best (reduce #'(lambda (best p)
			     (destructuring-bind (best-point max-err)
				 best
			       (let ((err (abs (- (height-field-get height-field p)
						  (funcall interpolate-height p)))))
				 (if (> err max-err)
				     (list p err)
				     best))))
			 points :initial-value '(nil -1))))
      (destructuring-bind (best-point max-err)
	  best
	(nconc (lface-data face-edge)
	       (list :point best-point :valid t :err max-err))))))

(defun write-stl (terrain filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "solid terrain~%")
    (let ((faces (all-faces (terrain-mesh terrain))))
      (loop for face across faces do
	(when (= 3 (length (lface-edges face)))
	  (let ((norm (face-normal face)))
	    (format out "facet normal ~a ~a ~a~%" (vec3-x norm) (vec3-y norm) (vec3-z norm))
	    (format out "  outer loop~%")
	    (loop for e in (lface-edges face) do
	      (let ((v (origin e))
		    (y (* 100 (odata e))))
		(format out "    vertex ~a ~a ~a~%" (vec2-x v) y (vec2-y v))))
	    (format out "  endloop~%")
	    (format out "endfacet~%")))))))
