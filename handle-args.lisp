;; Helper script

(in-package :cl-user)
(require :asdf)
(let ((me #.(or *compile-file-truename* *load-truename*)))
  (load (merge-pathnames "system-loader.lisp" me)))
(load-lisp:handle-posix-argv)
