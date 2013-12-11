(in-package :fenixedu-commonlisp-sdk)

(define-condition fenixedu-error (error)
  ((message :initarg :message :initform "" :reader message ))
  (:documentation "Any fenixedu error."))

(define-condition fenixedu-unknown-error (fenixedu-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Unknown response from: ~A~%" 
		     (message condition))))
  (:documentation "unknown fenixedu error"))

(define-condition fenixedu-unknownhost-error (fenixedu-error)
  ()
  (:documentation "host not found error.")
  (:report (lambda (condition stream)
	     (format stream "Unknown host: ~A~%" 
		     (message condition)))))

(define-condition fenixedu-nettimeout-error (fenixedu-error)
  ()
  (:documentation "host not found error.")
  (:report (lambda (condition stream)
	     (format stream "connection timed out: ~A~%" 
		     (message condition)))))

(define-condition fenixedu-network-error (fenixedu-error)
  ()
  (:documentation "generic connectivity problem.")
  (:report (lambda (condition stream)
	     (format stream "connection problem: ~A~%" 
		     (message condition)))))

(define-condition fenixedu-configuration-error (fenixedu-error)
  ()
  (:documentation "Error reading fenixedu.ini.")
  (:report (lambda (condition stream)
	     (format stream "no such option/section combination: ~A~%" 
		     (message condition)))))

