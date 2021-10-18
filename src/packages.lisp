(defpackage :vec
  (:use common-lisp)
  (:export :vec2 :vec2-x :vec2-y
   :vec2-dist :vec2-slope :vec2-hash-key
	   :vec3 :vec3-x :vec3-y :vec3-z
	   :vec3-norm :vec3-unit :vec3-sub :vec3-cross))

(defpackage :quad-edge
  (:use :common-lisp)
  (:export :make-edge :origin :onext :rot :lface-data :odata
	   :edge-walk
   :sym :dest :lnext :lprev :oprev :dprev
   :splice :lface-edges :origin-edges :set-lface-data))

(defpackage :delaunay-mesh
  (:use :common-lisp :quad-edge :vec)
  (:export :make-delaunay-mesh :delaunay-mesh-starting-edge
	   :insert-point :all-faces))

(defpackage :priority-queue
  (:use common-lisp)
  (:export :make-priority-queue :empty-p :insert :remove-max))

(defpackage :terrain-simpl
  (:use :common-lisp :priority-queue :vec :delaunay-mesh :quad-edge)
  (:export :make-terrain :make-height-field :write-stl))
