;;; Redis client library for Common Lisp


(in-package :redis)

(defparameter *redis-host* "localhost")
(defparameter *redis-port* 6379)

(defconstant +cr+ (code-char 13))
(defconstant +lf+ (code-char 10))
(defparameter +dollar+ (char-code #\$))

(defparameter +crlf+ (concatenate 'string (string +cr+) (string +lf+)))

(defvar *redis-socket*)
(defvar *redis-stream*)


(defmacro with-redis (&body body)
  `(with-client-socket (socket *redis-stream* *redis-host* *redis-port* :element-type '(unsigned-byte 8))
    ,@body))

(defvar *redis-debug-stream* *debug-io*)

(defun write-octets (seq &optional (stream *redis-stream*))
  (let ((str (concatenate 'string seq (string +cr+) (string +lf+))))
#+debug-redis    (format *redis-debug-stream* "--> ~s~%" str)
    (write-sequence (babel:string-to-octets str) stream)
    (force-output stream)))
 
(defun read-terminated-octets (&optional (terminator (char-code +cr+)))
  (let ((octets (loop for x = (read-byte *redis-stream*) until (equalp x terminator) collecting x)))
    (coerce octets 'vector)))

(defun range (start end)
  (loop for x from start to end collecting x))
    

(defun vec-to-str (vector)
  (map 'string #'code-char vector))

(defun stringify (&rest strings)
  (let ((result ""))
    (loop for x in strings do
	  (setf result (concatenate 'string result " " x)))
    (string-trim " " result)))

;; (defun stringify (&rest strings)
;;   (let ((result ""))
;;     (loop for x in (butlast strings) do
;; 	  (setf result (concatenate 'string result " " x)))
;;     (string-trim " " (concatenate 'string result (car (last strings))))))


(defun skip-until ()
  (loop for x = (read-byte *redis-stream* nil) until (or (null x) (= x 10)))) ; collecting x))
;	(princ x)))


;; (with-input-from-string (*redis-stream* "Hello World$Joy")
;;   (loop for x = (read-char *redis-stream* nil) until (or (null x) (char= x #\$)) do
;; 	(princ x))
;;   (peek-char nil *redis-stream* nil nil))
  


(defun read-status ()
  (let* ((status (read-byte *redis-stream* nil nil)))
;	 (len (read-byte *redis-stream* nil nil)))
    (when status
      (let ((result (vec-to-str (read-terminated-octets))))
	(skip-until)
	(values result status)))))

(defun read-integer-result ()
  (let ((status (read-byte *redis-stream* nil)))
    (if (not (and status (= status (char-code #\:))))
	(values (vec-to-str (read-terminated-octets)) status)
	(let ((result (vec-to-str (read-terminated-octets))))
	  (skip-until)
	  (values result status)))))

(defun read-bulk-result ()
  (multiple-value-bind (length status) (read-status)
 ;   (format t "read-bulk-result: length = ~a~tstatus = ~a~%" length status)
    (when (= status +dollar+)
      (let ((size (parse-integer length :junk-allowed t)))
	(if (plusp size)
	    (let ((buf (make-array size :element-type '(unsigned-byte 8))))
	      (read-sequence buf *redis-stream*)
	      (skip-until)
	      (vec-to-str buf))
	    (values length status))))))

(defun read-multi-bulk-result (keys)
  (multiple-value-bind (len code) (read-status)
    (if (string-equal len "-1")
	(values len code)
	(progn
	  (skip-until)
	  (if (not (= code (char-code #\*)))
	      (error len)
	      (let ((results nil)
		    (size (parse-integer len)))
		(dotimes (x size)
		  (let ((y (read-bulk-result)))
		    (push (list (- size x) (nth (- size x) keys) y) results)))
		results))))))

(defun redis-ping ()
  "returns PONG"
  (with-redis
    (write-octets "PING")
    (read-status)))

(defun redis-quit ()
  "returns nothing"
  (with-redis
    (write-octets "QUIT")))

(defun redis-auth (password)
  "returns status code"
  (with-redis
    (write-octets (stringify "AUTH" password))
    (read-status)))

(defun redis-set (key value)
  "returns status code"
   (with-redis
     (write-octets (stringify "SET" key (princ-to-string (1+ (length value))) +crlf+  value))
     (read-status)))
    
(defun redis-get (name)
  "returns bulk reply"
  (with-redis
    (write-octets (stringify "GET" name))
    (read-bulk-result)))

(defun redis-getset (key value)
  "returns bulk reply"
  (with-redis
    (write-octets (stringify "GETSET" key (princ-to-string (1+ (length value))) +crlf+ value))
    (read-bulk-result)))

(defun redis-mget (&rest keys)
  "returns multibulk"
  (with-redis
    (write-octets (apply #'stringify "MGET" keys))
    (read-multi-bulk-result keys)))

(defun redis-setnx (key value)
  "returns integer code"
   (with-redis
     (write-octets (stringify "SETNX" key (princ-to-string (1+ (length value))) +crlf+  value))
     (read-integer-result)))

(defun redis-incr (key)
  "returns integer code"
   (with-redis
     (write-octets (stringify "INCR" key))
     (read-integer-result)))

(defun redis-incrby (key by)
  "returns integer code"
   (with-redis
     (write-octets (stringify "INCRBY" key by))
     (read-integer-result)))

(defun redis-decr (key)
  "returns integer code"
   (with-redis
     (write-octets (stringify "DECR" key))
     (read-integer-result)))

(defun redis-decrby (key by)
  "returns integer code"
   (with-redis
     (write-octets (stringify "DECRBY" key by))
     (read-integer-result)))

(defun redis-exists (key)
  "returns integer code"
   (with-redis
     (write-octets (stringify "EXISTS" key))
     (read-integer-result)))

(defun redis-del (key)
  "returns integer code"
   (with-redis
     (write-octets (stringify "DEL" key))
     (read-integer-result)))

(defun redis-type (key)
  "returns integer code"
   (with-redis
     (write-octets (stringify "TYPE" key))
     (read-status)))

;;; Key Space
(defun redis-keys (pattern)
  "returns bulk reply"
  (with-redis
    (write-octets (stringify "KEYS" pattern))
    (read-bulk-result)))

(defun redis-randomkey ()
  "single line reply"
  (with-redis
    (write-octets "RANDOMKEY")
    (read-status)))

(defun redis-rename (old-key new-key)
  "returns status code reply"
  (with-redis
    (write-octets (stringify "RENAME" old-key new-key))
    (read-status)))

(defun redis-renamenx (old-key new-key)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "RENAMENX" old-key new-key))
    (read-integer-result)))

(defun redis-dbsize ()
  "returns integer code reply"
  (with-redis
    (write-octets "DBSIZE")
    (read-integer-result)))

(defun redis-expire (key seconds)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "EXPIRE" key seconds))
    (read-integer-result)))

;;TODO Redis >= 1.1
(defun redis-expireat (key unix-time)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "EXPIREAT" key unix-time))
    (read-integer-result)))

(defun redis-ttl (key)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "TTL" key))
    (read-integer-result)))


;;; List commands
(defun redis-rpush (key string)
  "returns status code reply"
  (with-redis
    (write-octets (stringify "RPUSH" key (princ-to-string (1+ (length string))) +crlf+ string))
    (read-status)))

(defun redis-lpush (key string)
  "returns status code reply"
  (with-redis
    (write-octets (stringify "LPUSH" key (princ-to-string (1+ (length string))) +crlf+ string))
    (read-status)))

(defun redis-llen (key)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "LLEN" key))
    (read-integer-result)))

(defun redis-lrange (key start end)
  "returns multi-bulk reply"
  (with-redis
    (write-octets (stringify "LRANGE" key start end))
    (read-multi-bulk-result (range (parse-integer start) (parse-integer end)))))

(defun redis-ltrim (key start end)
  "returns status code reply"
  (with-redis
    (write-octets (stringify "LTRIM" key start end))
    (read-status)))

(defun redis-lset (key index value)
  "returns status code reply"
  (with-redis
    (write-octets (stringify "LSET" key index (princ-to-string (1+ (length value))) +crlf+ value))
    (read-status)))

(defun redis-lrem (key count value)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "LREM" key count (princ-to-string (1+ (length value))) +crlf+ value))
    (read-integer-result)))

(defun redis-lpop (key)
  "returns bulk reply"
  (with-redis
    (write-octets (stringify "LPOP" key))
    (read-bulk-result)))

(defun redis-rpop (key)
  "returns bulk reply"
  (with-redis
    (write-octets (stringify "RPOP" key))
    (read-bulk-result)))

(defun redis-rpoplpush (src-key dest-key)
  "returns bulk reply"
  (with-redis
    (write-octets (stringify "RPOPLPUSH" src-key dest-key))
    (read-bulk-result)))


;;; Set commands

(defun redis-sadd (key member)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "SADD" key (princ-to-string (1+ (length member))) +crlf+ member))
    (read-integer-result)))

(defun redis-srem (key member)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "SREM" key member))
    (read-integer-result)))

(defun redis-smove (src-key dest-key member)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "SMOVE" src-key dest-key (princ-to-string (1+ (length member))) +crlf+ member))
    (read-integer-result)))

(defun redis-scard (key)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "SCARD" key))
    (read-integer-result)))

(defun redis-sismember (key member)
  "returns integer code reply"
  (with-redis
    (write-octets (stringify "SISMEMBER" key (princ-to-string (1+ (length member))) +crlf+  member))
    (read-integer-result)))

(defun redis-sinter (&rest keys)
  "returns multi-bulk reply"
  (with-redis
    (write-octets (apply #'stringify "SINTER" keys))
    (read-multi-bulk-result keys)))


(defun redis-sinterstore (dest-key &rest keys)
  "returns integer reply [official docs are wrong]"
  (with-redis
    (write-octets (apply #'stringify "SINTERSTORE" dest-key keys))
    (read-integer-result)))

(defun redis-sunion (&rest keys)
  "returns multi-bulk reply"
  (with-redis
    (write-octets (apply #'stringify "SUNION" keys))
    (read-multi-bulk-result keys)))

(defun redis-sunionstore (dest-key &rest keys)
  "returns integer reply [official docs are wrong]"
  (with-redis
    (write-octets (apply #'stringify "SUNIONSTORE" dest-key keys))
    (read-integer-result)))

(defun redis-sdiff (&rest keys)
  "returns multi-bulk reply"
  (with-redis
    (write-octets (apply #'stringify "SDIFF" keys))
    (read-multi-bulk-result keys)))

(defun redis-sdiffstore (dest-key &rest keys)
  "returns integer reply [official docs are wrong]"
  (with-redis
    (write-octets (apply #'stringify "SDIFFSTORE" dest-key keys))
    (read-integer-result)))

(defun redis-smembers (key)
  "returns multi-bulk reply"
  (with-redis
    (write-octets (stringify "SMEMBERS" key))
    (read-multi-bulk-result (list key))))

;;; Multiple-databaes Commands
(defun redis-select (index)
  "returns status reply"
  (with-redis
    (write-octets (stringify "SELECT" index))
    (read-status)))

(defun redis-move (key db-index)
  "returns status reply"
  (with-redis
    (write-octets (stringify "MOVE" key db-index))
    (read-integer-result)))

(defun redis-flushdb ()
  "returns status reply code"
  (with-redis
    (write-octets "FLUSHDB")
    (read-status)))

(defun redis-flushall()
  "returns status reply code"
  (with-redis
    (write-octets "FLUSHALL")
    (read-status)))  

;;;; Sorting Commands TODO
(defun redis-sort ()
  "returns status reply code"
  (with-redis
    (write-octets "SORT")
    (read-status)))

;;;; Persistence Control Commands
(defun redis-save ()
  "returns status reply code"
  (with-redis
    (write-octets "SAVE")
    (read-status)))

(defun redis-bgsave ()
  "returns status reply code"
  (with-redis
    (write-octets "BGSAVE")
    (read-status)))

(defun redis-lastsave ()
  "returns status reply code"
  (with-redis
    (write-octets "LASTSAVE")
    (read-status)))

(defun redis-shutdown ()
  "returns status reply code"
  (with-redis
    (write-octets "SHUTDOWN")
    (read-status)))


;;;; Remote server control commands
(defun redis-info ()
  "returns bulk reply"
  (with-redis
    (write-octets "INFO")
    (read-bulk-result)))

(defun redis-monitor ()   ;; TODO
  "non-standard result set"
  (with-redis
    (write-octets "MONITOR")
    (read-status)))

(defun redis-slaveof (host port)
  "returns status reply code"
  (with-redis
    (write-octets (stringify "SLAVEOF" host port))
    (read-status)))

