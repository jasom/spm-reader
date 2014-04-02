;;;; package.lisp

(cl:defpackage #:spm-reader
  (:use #:cl #:split-sequence)
   (:export
     #:do-symbols #:do-external-symbols #:in-package
     #:find-symbol #:intern #:import #:shadow #:shadowing-import
     #:unexport #:unintern #:unuse-package #:use-package
     #:find-package #:rename-package #:delete-package #:use-package-as #:install-spm
     #:spm-readtable
     "READTABLE" "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER" "READ"
     "READ-PRESERVING-WHITESPACE" "READ-DELIMITED-LIST" "READ-FROM-STRING"
     "READTABLE-CASE" "READTABLEP" "SET-DISPATCH-MACRO-CHARACTER"
     "GET-DISPATCH-MACRO-CHARACTER" "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
     "SET-SYNTAX-FROM-CHAR" "WITH-STANDARD-IO-SYNTAX" "*READ-BASE*"
     "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*" "*READ-SUPPRESS*"
     "*READTABLE*")
   (:shadow
     #:do-symbols #:do-external-symbols #:in-package
     #:find-symbol #:intern #:import #:shadow #:shadowing-import
     #:unexport #:unintern #:unuse-package #:use-package
     #:find-package #:rename-package #:delete-package)
   (:import-from
     #:com.informatimago.common-lisp.lisp-reader.reader #:defparser
     #:readtable-parse-token #:*input-stream* #:+ct-package-marker+ #:accept
     #:parse-decimal-integer-token #:parse-integer-token #:parse-ratio-token
     #:reject #:token-text #:parse-float-1-token #:parse-float-2-token
     #:token-traits #:traitp
     )
   (:shadowing-import-from
     #:com.informatimago.common-lisp.lisp-reader.reader
     "READTABLE" "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER" "READ"
     "READ-PRESERVING-WHITESPACE" "READ-DELIMITED-LIST" "READ-FROM-STRING"
     "READTABLE-CASE" "READTABLEP" "SET-DISPATCH-MACRO-CHARACTER"
     "GET-DISPATCH-MACRO-CHARACTER" "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
     "SET-SYNTAX-FROM-CHAR" "WITH-STANDARD-IO-SYNTAX" "*READ-BASE*"
     "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*" "*READ-SUPPRESS*"
     "*READTABLE*"))

