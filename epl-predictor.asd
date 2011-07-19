;;;; epl-predictor.asd

(asdf:defsystem #:epl-predictor
  :serial t
  :depends-on (#:utils #:matrix #:statistics)
  :components ((:file "package")
               (:file "epl-predictor")
	       (:file "training")
	       (:file "epl")
	       (:file "prediction")
	       (:file "scenario-testing")
	       (:file "load_data")))

