fenixedu-commonlisp-sdk
=======================

The Common Lisp SDK for the FenixEdu project.

Requirements
============

The SDK has multiple dependencies:

+ jsown
+ py-configparser
+ Drakma

They are all available through quicklisp so it is recommended that you load the SDK through it as it will install all unmet dependencies.

Setup
=====

Before you start using the SDK, you need to setup the ```fenixedu.ini``` file with your application's credentials. 

Just copy the sample provided in ```fenixedu.sample.ini``` to ```fenixedu.ini``` and fill it with your credentials.

Usage
=====

```lisp
;; Load the SDK
(ql:quickload :fenixedu-commonlisp-sdk)

;; make the initial setup:
(cl-fenix:startup "path/to/fenixedu.ini") ;; or (cl-fenix:startup) if you are in the same directory as the ini file.

;; get the authentication url:
(cl-fenix:get-authentication-url)

;; redirect your users to that URL
;; the users will be redirected to an url like: 
;; redirect_uri?code=[code]
;; get the code parameter and run 
(cl-fenix:set-code [code])

;; if everything goes well you can test the API by making a private and a public call
(cl-fenix:get-about) ;; public call
(cl-fenix:get-person) ;; private call
```

All the calls return a json string so you can use your favourite json parser.
