;;;; spm-reader.lisp

(cl:in-package #:spm-reader)

;;; "spm-reader" goes here. Hacks and glory await!


(defvar *package-hash* (make-hash-table))
(defvar *package-counter* 0)
(defparameter +package-prefix+ "jasom-spm-internal")
(defparameter +null-package+ "no-such-package")

(defun symbol-to-package (package)
  ;(format t "~S~%" (symbol-package package))
  (if (keywordp package)
      (symbol-name package)
      (gethash package *package-hash* +null-package+)))

(defun fixup-package-designator (package)
  (cond
    ((stringp package)
     (symbol-to-package
      (cl:intern package (cl:find-package :keyword))))
    ((symbolp package)
     (symbol-to-package package))
    (t
     package)))

(defmacro wrap-simple-pfun (fn &optional mangle)
  "Wraps function with lambda list like this (x &optional package-designator)"
  (let ((x (gensym)))
    `(defun ,(cl:intern (symbol-name fn)) (,x &optional (package nil packagep))
       (let ((,x ,(if mangle `(funcall ,mangle ,x) x)))
	 (if packagep
	     (funcall #',fn ,x (fixup-package-designator package))
	     (funcall #',fn ,x))))))

(wrap-simple-pfun cl:find-symbol)
(wrap-simple-pfun cl:intern)

(defun find-package (name)
  (cl:find-package (fixup-package-designator name)))

(wrap-simple-pfun cl:import)

(defun rename-package (package &rest r)
  (apply #'cl:rename-package (fixup-package-designator package) r))

(wrap-simple-pfun cl:shadow)

(wrap-simple-pfun cl:shadowing-import)

(defun delete-package (package)
  (let ((pkg (find-package package)))
    (maphash
     (lambda (k v)
       (when (eql v pkg)
	 (remhash k *package-hash* )))
     *package-hash*)
    (cl:delete-package pkg)))

(wrap-simple-pfun cl:unexport)

(wrap-simple-pfun cl:unintern)

(defmacro in-package (name)
  `(cl:in-package ,(cl:package-name (fixup-package-designator name))))

(wrap-simple-pfun cl:unuse-package (lambda (x) (mapcar #'fixup-package-designator (if (listp x) x (list x)))))

(wrap-simple-pfun cl:use-package
		  (lambda (x)
		    (mapcar #'fixup-package-designator
			    (if (listp x) x (list x)))))

(defmacro do-symbols ((var &optional (package nil packagep) result-form) &body b)
  `(cl:do-symbols (,var ,@(if packagep (list package result-form) nil)) ,@b))

(defmacro do-external-symbols ((var &optional (package nil packagep) result-form) &body b)
  `(cl:do-external-symbols (,var ,@(if packagep (list package result-form) nil)) ,@b))

(defun use-package-as (pkg sym)
  (setf (gethash sym *package-hash* )
	(find-package pkg)))

;;;Readtable hacks are all below
(defparser parse-symbol-token (token)
  ""
  (let* ((split (split-sequence-if
		 (lambda (x)
		   (traitp +ct-package-marker+ (car x)))
		 (loop for x across (token-text token)
		      for y across (token-traits token)
		      collect (cons y x))))
	 (split (mapcar (lambda (x) (map 'string #'cdr x)) split))
	 (where (if (string= (car split) "")
		      (cl:find-package "KEYWORD")
		      *package*))
	 (split (loop for x on split
		   when (string/= (car x) "") return x))
	 (internal t))
    ;(format t "where: ~S~%" where)
    (when (or (null split)
	      (string= (car (last split)) ""))
      (reject t "Token can't end with a package marker ~s"
	      (token-text token)))
    (loop while split
       do (if (string= (first split) "")
	      (if (string= (second split) "")
		  (if (string= (third split) "")
		      (reject t "Too many package markers in token ~S" (token-text token))
		      (setf internal t
			    split (cddr split)))
		  (setf internal nil
			split (cdr split)))
	      (let ((name (first split)))
		(setf split (cdr split))
		(if internal
		    (setf where (intern name where))
		    (multiple-value-bind (sym where2) (find-symbol name where)
		      (if (eq where2 :external) 
			  (setf where sym)
			  (setf where
				(restart-case (error 'symbol-missing-in-package-error
						     :stream *input-stream* :package-name where :symbol-name name)
				  (make-symbol (&rest rest)
				    :report "Make the missing symbol in the specified package"
				    (declare (ignore rest))
				    (intern name where)))))))))
	 finally (accept 'symbol where))))

(defun parse-token (token)
  "
RETURN:  okp ; the parsed lisp object if okp, or an error message if (not okp)
"
  (let ((message nil))
    (macrolet
        ((rom (&body body)
           "Result Or Message"
           (if (null body)
               'nil
               (let ((vals (gensym)))
                 `(let ((,vals (multiple-value-list ,(car body))))
                    ;; (format *trace-output* "~S --> ~S~%" ',(car body) ,vals)
                    (if (first ,vals)
                        (values-list ,vals)
                        (progn
                          (when (second ,vals)
                            (setf message  (third ,vals)))
                          (rom ,@(cdr body)))))))))
      (multiple-value-bind (ok type object)
          (rom (parse-decimal-integer-token token)
               (parse-integer-token         token)
               (parse-ratio-token           token)
               (parse-float-1-token         token)
               (parse-float-2-token         token)
               ;; (parse-consing-dot-token     token)
               (parse-symbol-token          token))
        (declare (ignorable type))
        ;; (format *trace-output* "ok = ~S ; type = ~S ; object = ~S~%"
        ;;         ok type object)
        (values ok (if ok object message))))))

(defparameter +eof-value+ (gensym))

(defun read-read-macro (stream ch)
  (cl:unread-char ch stream)
  (let ((result (read-preserving-whitespace stream nil +eof-value+ t)))
    (if (eql result +eof-value+) (values) result)))

(defun make-dumb-readtable ()
  (let ((dumb-readtable (cl:copy-readtable nil)))
    (loop for code from 0 below cl:char-code-limit
       do (cl:set-macro-character
	   (handler-case (cl:code-char code)
	     (t () (cl:code-char 0)))
	   #'read-read-macro
	   nil 
	   dumb-readtable))
    dumb-readtable))

(defun switch-to-informatimago-readtable ()
    (setf cl:*readtable* (make-dumb-readtable)))

(defun setup-spm-in-informatimago-readtable () 
  (setf *readtable* (copy-readtable nil))
  (set-macro-character #\` #'reader-macro-backquote2)
  (set-macro-character #\, #'reader-macro-comma2)
  (setf (readtable-parse-token *readtable*) #'parse-token))

(defun install-spm ()
  (setup-spm-in-informatimago-readtable)
  (switch-to-informatimago-readtable))


(defun reader-macro-backquote2 (stream ch)
  "Standard ` macro reader."
  (declare (ignore ch))
  (backquote (read stream t nil t) stream))


(defun reader-macro-comma2 (stream ch)
  "Standard , macro reader."
  (declare (ignore ch))
  `(,(if (char= #\@ (peek-char nil stream t nil t))
	 (progn (read-char stream)
		'splice)
	 'unquote)
     ,(read stream t nil t)))

(defun backquote (stuff stream)
  (cond
    ((and (not (consp stuff))
	  (not (vectorp stuff)))
     (list 'cl:quote stuff))
    ((and (consp stuff)
	  (eql (car stuff) 'unquote))
    (second stuff)) 
    ((and (consp stuff)
	  (eql (car stuff) 'splice))
     (com.informatimago.common-lisp.lisp-reader.reader::serror
      'com.informatimago.common-lisp.lisp-reader.reader::simple-reader-error
      stream ",@ after backquote"))
    ((consp stuff)
     `(append 
      ,@(loop for (this . rest) on stuff
	     collect
	     (cond
	       ((and (consp this)
		     (eql (car this) 'unquote))
		`(list ,(second this)))
	       ((and (consp this)
		     (eql (car this) 'splice))
		(second this))
	       (t
		`(list ,(backquote this stream))))
	     when (and
		   (atom rest)
		   (not (null rest)))
	     collect (list 'cl:quote rest))))
    ((vectorp stuff)
     `(apply #'vector ,(backquote (map 'list #'identity stuff) stream)))))

(setup-spm-in-informatimago-readtable)
(named-readtables:register-readtable 'spm-readtable (make-dumb-readtable))
