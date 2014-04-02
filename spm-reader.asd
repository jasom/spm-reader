;;;; spm-reader.asd

(asdf:defsystem #:spm-reader
  :serial t
  :description "Describe spm-reader here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:com.informatimago.common-lisp.lisp-reader
	       #:split-sequence
	       #:named-readtables)
  :components ((:file "package")
               (:file "spm-reader")))
