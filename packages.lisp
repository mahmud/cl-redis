(in-package #:cl-user)

(defpackage #:redis 
  (:use :cl :usocket :babel)
  (:export 
   ;; connection handling
   #:redis-quit #:redis-auth #:redis-ping

   ;; string values
   #:redis-set #:redis-get #:redis-getset #:redis-mget #:redis-setnx 
   #:redis-incr #:redis-incrby #:redis-decry #:redis-decrby
   #:redis-exists #:redis-del #:redis-type

   ;; key space
   #:redis-keys #:redis-randomkey #:redis-rename #:redis-dbsize #:redis-expire #:redis-ttl

   ;; lists
   #:redis-rpush #:redis-lpush #:redis-llen #:redis-lrange #:redis-ltrim #:redis-lindex
   #:redis-lset #:redis-lrem #:redis-lpop #:redis-rpop #:redis-rpoplpush
   
   ;; sets
   #:redis-sadd #:redis-srem #:redis-smove #:redis-scard #:redis-sismember #:redis-sinter
   #:redis-sinterstore #:redis-sunion #:redis-sunionstore #:redis-sdiff #:redis-sdiffstore
   #:redis-smembers

   ;; multiple-database handling
   #:redis-select #:redis-move #:redis-flushdb #:redis-flushall

   ;; sort ;; TODO
   
   ;; persitence control
   #:redis-save #:redis-bgsave #:redis-lastsave #:redis-shutdown

   ;; remote server control
   #:redis-info #:redis-monitor #:redis-slaveof

   ))
