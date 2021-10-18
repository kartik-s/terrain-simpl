;;;; an implementation of the quad-edge data structure for representing embeddings of graphs in two-dimensional manifolds

(in-package :quad-edge)

(defstruct (edge (:constructor %make-edge) :conc-name)
  origin
  onext
  rot
  lface-data
  odata)

(defun make-edge (&key origin dest)
  (let ((e0 (%make-edge :origin origin))
	(e1 (%make-edge))
	(e2 (%make-edge :origin dest))
	(e3 (%make-edge)))
    (setf (onext e0) e0)
    (setf (onext e1) e3)
    (setf (onext e2) e2)
    (setf (onext e3) e1)
    (setf (rot e0) e1)
    (setf (rot e1) e2)
    (setf (rot e2) e3)
    (setf (rot e3) e0)
    e0))

(defmacro edge-walk (ops edge)
  (reduce #'(lambda (curr op) `(,op ,curr)) ops :initial-value edge))

(defun sym (edge)
  (edge-walk (rot rot) edge))

(defun dest (edge)
  (declare (inline sym))
  (origin (sym edge)))

(defun lnext (edge)
  (declare (inline sym))
  (edge-walk (sym rot onext rot) edge))

(defun lprev (edge)
  (declare (inline sym))
  (edge-walk (onext sym) edge))

(defun oprev (edge)
  (edge-walk (rot onext rot) edge))

(defun dprev (edge)
  (declare (inline lnext sym))
  (edge-walk (lnext sym) edge))

(defun splice (edge-a edge-b)
  (let ((alpha (edge-walk (onext rot) edge-a))
	(beta (edge-walk (onext rot) edge-b)))
    (rotatef (onext edge-a) (onext edge-b))
    (rotatef (onext alpha) (onext beta))))

(defun lface-edges (first-edge)
  (cons first-edge
	(loop for e = (lnext first-edge) then (lnext e)
	      until (eql e first-edge)
	      collect e)))

(defun origin-edges (first-edge)
  (cons first-edge
	(loop for e = (onext first-edge) then (onext e)
	      until (eql e first-edge)
	      collect e)))

(defun set-lface-data (face-edge data)
  (dolist (edge (lface-edges face-edge))
    (setf (lface-data edge) data)))
