(defpackage #:cl-redis-test
  (:use :cl :redis :lift)
  (:nicknames :redis-test))

(in-package :redis-test)

(deftestsuite redis-connection-handling () ())

(addtest (ensure-same (redis-ping) "PONG"))

(addtest (ensure-same (redis-quit) nil))

(addtest (ensure-same (redis-auth "foo") "OK"))

(addtest (ensure-same (redis-set "foo" "redis-set-test") "OK"))
(addtest (ensure-same (string-trim " " (redis-get "foo")) "bar"))

(addtest (ensure-same (redis-getset "foo" "bar") nil))