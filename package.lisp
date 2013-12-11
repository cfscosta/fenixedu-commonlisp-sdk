;;;; package.lisp

(defpackage #:fenixedu-commonlisp-sdk
  (:nicknames cl-fenix)
  (:use #:cl)
  (:export #:startup
	   #:get-api-url
	   #:get-about
	   #:get-course
	   #:get-course-evaluations
	   #:get-course-groups
	   #:get-course-schedule
	   #:get-course-students
	   #:get-degrees
	   #:get-degree
	   #:get-degree-courses
	   #:get-spaces
	   #:get-space
	   #:get-person
	   #:get-person-curriculum
	   #:get-classes-calendar
	   #:get-evaluations-calendar
	   #:get-courses
	   #:get-evaluations
	   #:get-payments
	   #:enrol-in-evaluation
	   #:get-evaluation
	   #:refresh-access-token
	   #:set-code
	   #:get-authentication-url))


