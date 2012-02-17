;; Helper script

(in-package :cl-user)
(require :asdf)
(load (merge-pathnames "system-loader" *load-truename*))

;; Avoid recursive calls
(unless (load-lisp:arg-handler-installed-p)
  (load-lisp:handle-posix-argv))
