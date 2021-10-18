;;;; basic vector math library

(in-package :vec)

(defstruct (vec2 (:constructor vec2 (x y)))
  x y)

(defun vec2-dist (v0 v1)
  (let ((dx (- (vec2-x v0) (vec2-x v1)))
	(dy (- (vec2-y v0) (vec2-y v1))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun vec2-slope (v0 v1)
  (/ (float (- (vec2-y v1) (vec2-y v0)))
     (float (- (vec2-x v1) (vec2-x v0)))))

(defun vec2-hash-key (v)
  (intern (format nil "~a-~a" (vec2-x v) (vec2-y v))))

(defstruct (vec3 (:constructor vec3 (x y z)))
  x y z)

(defun vec3-norm (v)
  (let ((x (vec3-x v))
	(y (vec3-y v))
	(z (vec3-z v)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun vec3-unit (v)
  (let ((norm (vec3-norm  v)))
    (vec3 (/ (vec3-x v) norm)
	  (/ (vec3-y v) norm)
	  (/ (vec3-z v) norm))))

(defun vec3-sub (a b)
  (vec3 (- (vec3-x a) (vec3-x b))
	(- (vec3-y a) (vec3-y b))
	(- (vec3-z a) (vec3-z b))))

(defun vec3-cross (a b)
  (let ((ax (vec3-x a))
	(ay (vec3-y a))
	(az (vec3-z a))
	(bx (vec3-x b))
	(by (vec3-y b))
	(bz (vec3-z b)))
    (vec3 (- (* ay bz) (* az by))
	  (- (* az bx) (* ax bz))
	  (- (* ax by) (* ay bx)))))
