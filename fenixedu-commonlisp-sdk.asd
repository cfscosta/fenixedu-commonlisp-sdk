;;;; fenixedu-commonlisp-sdk.asd

(asdf:defsystem #:fenixedu-commonlisp-sdk
  :serial t
  :description "FenixEdu API SDK for common lisp"
  :author "Carlos Costa (carlos.fsilvacosta@gmail.com)"
  :depends-on (#:drakma
               #:jsown
	       #:py-configparser)
  :components ((:file "package")
	       (:file "errors")
	       (:file "information")
	       (:file "fenixedu-commonlisp-sdk")))

