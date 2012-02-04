(defpackage :load-lisp
  (:use :cl))

(in-package :load-lisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defparameter *toplevel-function* nil)

(defun read-from-string-standard (s)
  (with-standard-io-syntax
    (read-from-string s)))

(defparameter *flag-alist*
  `(("-I" . add-dir-to-asdf)
    ("-T" . add-tree-to-asdf)
    ("-S" . asdf:load-system)
    ("-R" . require)
    ("--load" . load)
    ("--compile" . compile-file)
    ("--eval" . eval-flag)
    ("--load-compile" . load-compile-flag)
    ("--deps" . deps-flag)
    ("--main" . main-flag)
    ("--save" . save-flag)
    ("--save-exe" . save-exe-flag)
    ("--swank" . swank-flag)
    ("--quit" . quit-flag)
    ("--install" . install-arg-handler-flag)))

;;;; Flag functions

(defun eval-flag (x)
  (eval (read-from-string-standard x)))

(defun load-compile-flag (x)
  (load (compile-file x)))

(defun main-flag (x)
  (setf *toplevel-function* (read-from-string-standard x)))

(defun inhibit-userinit ()
  (setf sb-ext::*userinit-pathname-function* (constantly nil)))

(defun save-flag (x &key executable inhibit-userinit)
  (let ((f (lambda (&rest rest)
             (apply #'sb-ext:save-lisp-and-die x
                    :executable (or *toplevel-function*
                                    executable)
                    rest))))

    (when inhibit-userinit
      (pushnew 'inhibit-userinit sb-ext:*init-hooks*))

    (if *toplevel-function*
        (funcall f :toplevel *toplevel-function*)
        (funcall f))))

(defun save-exe-flag (x)
  (save-flag x :executable t :inhibit-userinit t))

(defun deps-flag (x)
  (mapc #'asdf:load-system
        (asdf::component-load-dependencies
         (asdf::find-system x))))

(defun swank-flag (&optional x)
  (let ((x (and x (read-from-string-standard x))))
    (asdf:load-system :swank)
    (if (integerp x)
        (funcall (read-from-string-standard "SWANK::CREATE-SERVER") :port x)
        (funcall (read-from-string-standard "SWANK::CREATE-SERVER")))))

(defun quit-flag (&optional x)
  (declare (ignore x))
  (sb-ext:quit))

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

(defun install-arg-handler-flag (&optional x)
  (declare (ignore x))
  (pushnew 'handle-posix-argv sb-ext:*init-hooks*))


;;;; Arbitrary-funcall "---print ..."

(defun prefixp (prefix string)
  (let ((offset (search prefix string)))
    (and (numberp offset)
         (zerop offset))))

(defun arbitrary-funcall-p (flag)
  (prefixp "---" flag))

(defun arbitrary-funcall (flag arg)
  (let ((fun (read-from-string-standard (subseq flag 3))))
    (cond ((or (null arg)
               (prefixp "-" arg) )
           (funcall fun))
          ((prefixp "\\-" arg)
           (funcall fun (subseq arg 1)))
          (t (funcall fun arg)))))



;;;; Apply flags, kind of silly.
;; Valid spec characters are:

;; - r: read-from-string
;; - f: funcall the function with the flag argument
;; - a: apply the function to the flag argument (use like --rap-foo '(arg1 arg2 arg3)')
;; - p: print the result


;; Examples:

;: load-lisp --rfp-foo 100
;; - read-from-string "100"
;; - funcall foo with argument 100
;; - print the result

;: load-lisp --rap-list '(1 2 3 4 5)'
;; - read-from-string "(1 2 3 4 5)"
;; - apply list to (1 2 3 4 5)
;; - print the result


(defun apply-flag-op (&rest functions)
  (lambda (function arg)
    (reduce (lambda (result f)
              (case f
                (apply-the-function (apply function result))
                (funcall-the-function (funcall function result))
                (t (funcall f result))))
            functions :initial-value arg)))

(defparameter *apply-flag-prefixes*
  `(("--f-" . ,(apply-flag-op 'funcall-the-function))
    ("--rf-" . ,(apply-flag-op 'read-from-string-standard 'funcall-the-function))
    ("--rfp-" . ,(apply-flag-op 'read-from-string-standard 'funcall-the-function 'print))
    ("--fp-" . ,(apply-flag-op 'funcall-the-function 'print))
    ("--ra-" . ,(apply-flag-op 'read-from-string-standard 'apply-the-function))
    ("--rap-" . ,(apply-flag-op 'read-from-string-standard 'apply-the-function 'print))))

(defun find-apply-flag (flag)
  (assoc-if (lambda (x) (prefixp x flag))
            *apply-flag-prefixes*))

(defun make-apply-flag-function (flag)
  (let* ((apply-flag-entry (find-apply-flag flag))
         (apply-flag-spec (car apply-flag-entry))
         (apply-flag-op (cdr apply-flag-entry))

         ;; --rf-foo-bar ==> foo-bar
         (function-name (when apply-flag-entry
                          (read-from-string-standard
                           (subseq flag (length apply-flag-spec))))))

    (assert apply-flag-op)
    (lambda (arg)
      (funcall apply-flag-op function-name arg))))

(defun apply-flag-p (flag)
  (find-apply-flag flag))

;;;; Argument handling

(defun handle-args (args)
  (loop :for (flag arg) :on args :by #'cdr
     :for fn = (cdr (assoc flag *flag-alist* :test #'string=))
     :do (cond ((arbitrary-funcall-p flag)
                (arbitrary-funcall flag arg))
               ((apply-flag-p flag)
                (funcall (make-apply-flag-function flag) arg))
               (fn (funcall fn arg)))))

(defun handle-posix-argv ()
  (handle-args sb-ext:*posix-argv*))

;; Handle args at load-time.
(handle-posix-argv)
