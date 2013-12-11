(in-package #:fenixedu-commonlisp-sdk)

(defstruct (app-configuration (:conc-name api-))
  (client_id "" :type string)
  (redirect_uri "" :type string)
  (client_secret "" :type string)
  (base_url "" :type string)
  (api_endpoint "" :type string)
  (api_version "" :type string)
  ;; API specific 
  (oauth_endpoint  "oauth" :type string)
  (access_token_endpoint  "access_token" :type string)
  (access_token  "" :type string)
  (refresh_token  "" :type string)
  (expires)
  (code "" :type string)
  (error_key  "error" :type string)
  ;; API endpoints
  (person_endpoint  "person" :type string :read-only t)
  (about_endpoint  "about" :type string :read-only t)
  (courses_endpoint  "courses" :type string :read-only t)
  (evaluations_endpoint  "evaluations" :type string :read-only t)
  (schedule_endpoint  "schedule" :type string :read-only t)
  (groups_endpoint  "groups" :type string :read-only t)
  (students_endpoint  "students" :type string :read-only t)
  (degrees_endpoint  "degrees" :type string :read-only t)
  (calendar_endpoint  "calendar" :type string :read-only t)
  (payments_endpoint  "payments" :type string :read-only t)
  (spaces_endpoint  "spaces" :type string :read-only t)
  (classes_endpoint  "classes" :type string :read-only t)
  (curriculum_endpoint "curriculum" :type string :read-only t)
  (refresh_token_endpoint  "refresh_token" :type string :read-only t))

(defvar *base-info*)

;;(setf app-config (py-configparser:make-config))
;;(py-configparser:read-files app-config '("fenixedu-python-sdk/fenixedu.ini"))
;;(py-configparser:get-option app-config "fenixedu" "client_secret")

(defun base-start ()
  (setf *base-info* (make-app-configuration))
  (get-config))

;; sets storage 
(defmacro set-option (storage app-config section option)
  "Gets the desired option from the selected section in the configuration stored in app-config. 
   If it fails, it tries to get the option from the \"DEFAULT\" section"
  `(handler-case
       (setf ,storage (py-configparser:get-option ,app-config ,section ,option))
     (py-configparser:configparser-error () 
       (progn 
	 (handler-case
	     (setf ,storage (py-configparser:get-option ,app-config "DEFAULT" ,option))
	   (py-configparser:configparser-error ()
	     (progn 
	       (error 'fenixedu-configuration-error :message (format nil "~A/~A" ,option ,section))
	       )))))))
						     

;; make the setfs error handling macros so we know which parameter is causing the exception also checks the DEFAULT parameters
(defun get-config ()
  (let ((app-config (py-configparser:make-config))
	(section "fenixedu"))
    (py-configparser:read-files app-config '("fenixedu.ini"))
    (set-option (api-client_id *base-info*) app-config section "client_id")
    (set-option (api-redirect_uri *base-info*) app-config section "redirect_uri")
    (set-option (api-client_secret *base-info*) app-config section "client_secret")
    (set-option (api-base_url *base-info*) app-config section "base_url")
    (set-option (api-api_endpoint *base-info*) app-config section "api_endpoint")
    (set-option (api-api_version *base-info*) app-config section "api_version")
    t))

