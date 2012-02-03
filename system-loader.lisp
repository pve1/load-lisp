(defpackage :load-lisp
  (:use :cl))

(in-package :load-lisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defparameter *toplevel-function* nil)

(defun add-dir-to-asdf (dir &optional tree)
  (setf dir (concatenate 'string dir "/"))
  #+asdf2
  (push `(:source-registry
	  (,(if tree 
		:tree
		:directory)
	    ,(if (asdf::absolute-pathname-p dir)
		 dir
		 `(:here ,dir)))
	  :inherit-configuration)
	asdf:*default-source-registries*)
  #-asdf2
  (push (concatenate 'string dir "/")
	asdf:*central-registry*)) 

(defun add-tree-to-asdf (dir)
  #-asdf2 (error "Only ASDF2 supports the tree option.")
  (add-dir-to-asdf dir t)) 

(defun read-from-string-standard (s)
  (with-standard-io-syntax 
    (read-from-string s)))

(defparameter *flag-alist* 
  `(("-I" . ,#'add-dir-to-asdf)
    ("-T" . ,#'add-tree-to-asdf)
    ("-S" . ,#'asdf:load-system)
    ("-R" . ,#'require)
    
    ("--eval" . ,#'(lambda (x)
                     (eval (read-from-string-standard x))))

    ("--load" . ,#'load)

    ("--compile" . ,#'compile-file)
    
    ("--load-compile" . ,#'(lambda (x) (load (compile-file x))))
    
    ("--deps" . ,#'(lambda (x)
		     (mapc #'asdf:load-system 
			   (asdf::component-load-dependencies 
			    (asdf::find-system x)))))

    ("--main" . ,#'(lambda (x) 
                     (setf *toplevel-function* (read-from-string-standard x))))
    
    ("--save" . ,#'(lambda (x)
                     (let ((f (lambda (&rest rest)
                                (apply #'sb-ext:save-lisp-and-die x 
                                       :executable *toplevel-function* 
				       rest))))
                       (if *toplevel-function*
                           (funcall f :toplevel *toplevel-function*)
                           (funcall f)))))

    ("--swank" . ,#'(lambda (&optional x)
                      (let ((x (and x (read-from-string-standard x))))
                        (asdf:load-system :swank)
                        (if (integerp x)
                            (funcall (read-from-string-standard "SWANK::CREATE-SERVER") :port x)
                            (funcall (read-from-string-standard "SWANK::CREATE-SERVER"))))))
    
    ("--quit" . ,#'(lambda (&optional x)
                     (declare (ignore x))
                     (sb-ext:quit)))))


(defun prefixp (prefix string)
  (let ((offset (search prefix string)))
    (and (numberp offset) 
	 (zerop offset))))

(defun arbitrary-funcall-p (flag)
  (prefixp "---" flag))

;; example: ---ql:quickload cl-ppcre
(defun arbitrary-funcall (flag arg)
  (funcall (read-from-string-standard (subseq flag 3)) arg))

(defun handle-args (args)
  (loop :for (flag arg) :on args :by #'cdr
     :for fn = (cdr (assoc flag *flag-alist* :test #'string=))
     :do (cond ((arbitrary-funcall-p flag)
		(arbitrary-funcall flag arg))
	       (fn (funcall fn arg))))) 

(handle-args sb-ext:*posix-argv*)
