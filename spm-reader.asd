;;;; spm-reader.asd

(asdf:defsystem #:spm-reader
  :serial t
  :description "Experimental package reader"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "AGPL"
  :depends-on (#:com.informatimago.common-lisp.lisp-reader
	       #:split-sequence
	       #:named-readtables)
  :components ((:file "package")
               (:file "spm-reader")))
