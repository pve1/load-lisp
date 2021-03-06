;; This software is Copyright (c) Peter von Etter, 2012.
;; You can distribute and use it as governed by the terms of the Lisp
;; Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

(defpackage :load-lisp
  (:use :cl)
  (:export #:handle-posix-argv
           #:install-arg-handler
           #:arg-handler-installed-p))

(in-package :load-lisp)

(defvar *toplevel-function* nil)
(defvar *remaining-flags* nil)
(defvar *batch-mode* nil)

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
    ("--clean" . clean-system-flag)
    ("--deps" . deps-flag)
    ("--main" . main-flag)
    ("--save" . save-flag)
    ("--save-exe" . save-exe-flag)
    ("--swank" . swank-flag)
    ("--quit" . quit-flag)
    ("--install" . install-arg-handler-flag)
    ("--batch" . batch-flag)))


;;;; Util 

(defun source-newer-than-fasl (source &optional fasl)
  (<= (file-write-date fasl)
      (file-write-date source)))

(defun need-recompile-p (source &optional fasl)
  (or (not (probe-file fasl))
      (source-newer-than-fasl source fasl)))

(defun inhibit-userinit ()
  (setf sb-ext::*userinit-pathname-function* (constantly nil)))


;;;; Flag functions

(defun eval-flag (x)
  (eval (read-from-string-standard x)))

(defun load-compile-flag (x)
  (let ((fasl (merge-pathnames
               (make-pathname :type sb-fasl::*fasl-file-type*)
               x)))
    (if (need-recompile-p x fasl)
        (load (compile-file x))
        (load fasl))))

(defun main-flag (x)
  (setf *toplevel-function* (read-from-string-standard x)))

(defun batch-flag (&optional x)
  (declare (ignore x))
  (setf *batch-mode* t)
  (sb-ext:disable-debugger))

(declaim (ftype function handle-args))

;;;; Arguments after --save or --save-exe are considered zombie
;;;; arguments, and will be processed in the saved core's init hook.

(defparameter *zombie-arguments* nil)

(defun set-zombie-arguments (args)
  (setf *zombie-arguments* args))

(defun handle-args-from-beyond-the-grave ()
  (handle-args *zombie-arguments*))


;; Current strategy:

;; toplevel -> executable
;; executable -> no debugger, inhibit-userinit

;; no toplevel, executable -> repl core without debugger
;; no toplevel, not executable -> normal core

(defun %save-flag (x &key executable inhibit-userinit
                   toplevel zombie-args)
  
  (let* ((executable (or executable toplevel))

         (f (lambda (&rest rest)

              (if executable
                  (sb-ext::disable-debugger)
                  (sb-ext::enable-debugger))

              (when inhibit-userinit
                (inhibit-userinit))

              ;; Don't want batch mode to carry over.
              (setf *batch-mode* nil)
              
              (when zombie-args
                (set-zombie-arguments zombie-args)
                (pushnew 'handle-args-from-beyond-the-grave
                         sb-ext:*init-hooks*))

              (apply #'sb-ext:save-lisp-and-die x rest))))

    (if toplevel
        (funcall f :toplevel toplevel
                 :executable executable)        
        (funcall f :executable executable))))

;; --save-exe foo.core --x --y --z
;;                     ^^^^^^^^^^^    
;;                     zombie args

(defun save-exe-flag (x)
  (%save-flag x 
              :executable t
              :inhibit-userinit t
              :toplevel *toplevel-function*
              :zombie-args (cddr *remaining-flags*)))

(defun save-flag (x)
  (%save-flag x 
              :executable nil
              :inhibit-userinit nil
              :toplevel nil
              :zombie-args (cddr *remaining-flags*)))

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

(defun clean-system-flag (system)
  (let ((compile-op (make-instance 'asdf:compile-op)))
    (labels ((clean (component)
               (typecase component
                 (asdf:module
                  (mapc #'clean (asdf::module-components component)))
                 (asdf:source-file
                  (mapc (lambda (x)
                          (when (probe-file x)
                            (format t "Deleting ~A~%" x)
                            (delete-file x)))
                        (asdf:output-files compile-op component))))))
      (clean (asdf:find-system system)))))

(defun install-arg-handler-flag (&optional x)
  (declare (ignore x))
  (pushnew 'handle-posix-argv sb-ext:*init-hooks*))

(defun arg-handler-installed-p ()
  (find 'handle-posix-argv sb-ext:*init-hooks*))

(setf (fdefinition 'install-arg-handler)
      #'install-arg-handler-flag)

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
  (let ((funcall (apply-flag-op 'funcall-the-function))
        (read-funcall (apply-flag-op 'read-from-string-standard 'funcall-the-function))
        (read-funcall-print (apply-flag-op 'read-from-string-standard 'funcall-the-function 'print))
        (funcall-print (apply-flag-op 'funcall-the-function 'print))
        (read-apply (apply-flag-op 'read-from-string-standard 'apply-the-function))
        (read-apply-print (apply-flag-op 'read-from-string-standard 'apply-the-function 'print)))

  `(("--f-" . ,funcall)
    ("--rf-" .  ,read-funcall)
    ("--rfp-" . ,read-funcall-print)
    ("--fp-" . ,funcall-print)
    ("--ra-" . ,read-apply)
    ("--rap-" . ,read-apply-print)

    ;; mirror format also supported, for extra sillyness and easier
    ;; mnemonics (pff! arrr!)

    ("--fr-" . ,read-funcall)
    ("--pfr-" . ,read-funcall-print)
    ("--pf-" . ,funcall-print)
    ("--ar-" . ,read-apply)
    ("--par-" . ,read-apply-print))))

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

(defun long-flag-p (string)
  (prefixp "--" string))

;;;; Argument handling

(defun handle-args (args)
  (let ((*package* (find-package :cl-user)))
    (loop :for (flag arg) :on args :by #'cdr
       :for *remaining-flags* = args :then (cdr *remaining-flags*)
       :for fn = (cdr (assoc flag *flag-alist* :test #'string=))
       :do (cond (fn (funcall fn arg))
                 ((and (long-flag-p flag)
                       (arbitrary-funcall-p flag))
                  (arbitrary-funcall flag arg))
                 ((and (long-flag-p flag)
                       (apply-flag-p flag))
                  (funcall (make-apply-flag-function flag) arg))))))


(defun handle-posix-argv ()
  (handle-args sb-ext:*posix-argv*)
  (when *batch-mode*
    (sb-ext::quit)))
