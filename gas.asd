(asdf:defsystem "gas"
  :description "Argon gas simulation"
  :author "weld"
  :license "to-be-defined"
  :version "0.1"
  :depends-on ("cl-raylib" "3d-vectors")
  :components ((:file "package")
	       (:file "main")))
