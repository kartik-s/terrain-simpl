;;;; priority queue implemented as a max-heap

(in-package :priority-queue)

(defstruct (priority-queue (:constructor %make-priority-queue))
  greater-p
  elements)

(defun make-priority-queue (&key greater-p initial-size)
  (let ((elements (make-array initial-size :fill-pointer 0 :adjustable t)))
    (%make-priority-queue :greater-p greater-p :elements elements)))

(defun empty-p (pq)
  (zerop (length (priority-queue-elements pq))))

(defun insert (pq element)
  (let ((elements (priority-queue-elements pq))
	(greater-p (priority-queue-greater-p pq)))
    (vector-push-extend element elements)
    ;; sift up as necessary
    (labels ((sift-up (i)
	       (let ((parent-i (floor (- i 1) 2)))
		 (cond ((zerop i) nil)
		       ((funcall greater-p (elt elements i) (elt elements parent-i))
			(rotatef (elt elements i) (elt elements parent-i))
			(sift-up parent-i))
		       (t nil)))))
      (sift-up (- (length elements) 1)))))

(defun remove-max (pq)
  (let* ((elements (priority-queue-elements pq))
	 (greater-p (priority-queue-greater-p pq))
	 (max-element (elt elements 0)))
    (rotatef (elt elements 0) (elt elements (- (length elements) 1)))
    (vector-pop elements)
    ;; sift down as necessary
    (labels ((sift-down (i)
	       (let ((left-i (+ 1 (* 2 i))))
		 (if (>= left-i (length elements))
		     nil
		     (let* ((right-i (+ 1 left-i))
			    (max-child-i (cond ((>= right-i (length elements)) left-i)
					       ((funcall greater-p (elt elements left-i) (elt elements right-i)) left-i)
					       (t right-i))))
		       (cond ((funcall greater-p (elt elements max-child-i) (elt elements i))
			      (rotatef (elt elements max-child-i) (elt elements i))
			      (sift-down max-child-i))
			     (t nil)))))))
      (sift-down 0)
      max-element)))
