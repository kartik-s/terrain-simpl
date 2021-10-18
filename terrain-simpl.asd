(asdf:defsystem "terrain-simpl"
  :description "create a polygonal approximation of a terrain or heightfield using a Delaunay triangulation"
  :version "0.0.1"
  :author "Kartik Singh <kartik@freelygenerated.com"
  :license "BSD 2-Clause License"
  :depends-on (#:pngload)
  :components
  ((:module src
    :serial t
    :components
    ((:file "packages")
     (:file "vec")
     (:file "quad-edge")
     (:file "delaunay-mesh")
     (:file "priority-queue")
     (:file "terrain-simpl")))))
