
(ignore-errors (make-package :load-lisp))
(defclass load-lisp::compileable (asdf:cl-source-file) ())
(defmethod perform ((op asdf:load-op)
                    (c load-lisp::compileable))
  nil)

(asdf:defsystem :load-lisp
  :components ((:file "system-loader")
               (load-lisp::compileable "handle-args")))

               
