;; Helper script

(in-package :cl-user)
(require :asdf)
(load (merge-pathnames "system-loader" *load-truename*))
(load-lisp:handle-posix-argv)
