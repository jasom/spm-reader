;;;; spm-example.lisp
(defpackage #:spm-example
  (:use #:cl))

(in-package #:spm-example)

(named-readtables:in-readtable spm-reader::spm-readtable)

;; We'll alias YASON as 'json
;; alias CL-JSON ;; (which has the global nickname "JSON")  as 'clj
(eval-when (:compile-toplevel :load-toplevel :execute)
  (:spm-reader:use-package-as :yason 'json)
  (:spm-reader:use-package-as :json 'clj))

;We now can use our aliases to access the packages

(json:encode 
 (list (:alexandria:plist-hash-table
	'("foo" 1 "bar" (7 8 9))
	:test #'equal)
       2 3 4
       '(5 6 7)
       t nil)
 *standard-output*)

(clj:encode-json
 (list (:alexandria:plist-hash-table
	'("foo" 1 "bar" (7 8 9))
	:test #'equal)
       2 3 4
       '(5 6 7)
       t nil)
 *standard-output*)

;;Accessing them by their global name is still possible:

(:yason:encode 
 (list (:alexandria:plist-hash-table
	'("foo" 1 "bar" (7 8 9))
	:test #'equal)
       2 3 4
       '(5 6 7)
       t nil)
 *standard-output*)

(:cl-json:encode-json
 (list (:alexandria:plist-hash-table
	'("foo" 1 "bar" (7 8 9))
	:test #'equal)
       2 3 4
       '(5 6 7)
       t nil)
 *standard-output*)
