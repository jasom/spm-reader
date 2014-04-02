;;;; spm-example.asd

(asdf:defsystem #:spm-example
  :serial t
  :description "Describe spm-example here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:spm-reader
               #:yason
               #:cl-json)
  :components ((:file "package")
               (:file "spm-example")))

