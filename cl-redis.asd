(in-package #:cl-user)

(asdf:defsystem :cl-redis
  :components ((:file "packages")
               (:file "cl-redis" :depends-on ("packages")))
  :depends-on (:usocket :babel))