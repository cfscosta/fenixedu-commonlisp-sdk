(in-package #:fenixedu-commonlisp-sdk)

;;(setf app-config (py-configparser:make-config))
;;(py-configparser:read-files app-config '("fenixedu-python-sdk/fenixedu.ini"))
;;(py-configparser:get-option app-config "fenixedu" "client_secret")

(defvar *json-parser* 'json-string)

(defun startup ()
  (base-start))

;; This macro allows us to set the appropriate options in drakma requests without setting them for the entire application
(defmacro with-fenix-content-types (&body body)
  "Evaluate BODY treating application/json as text."
  `(let ((drakma:*text-content-types* (cons '("application" . "json")
                                        drakma:*text-content-types*)))
     ,@body))

(defun parse-json (json)
  (funcall *json-parser* json))

(defun json-string (string)
  string)

(defun get-api-url ()
  (concatenate 'string (api-base_url *base-info*) (api-api_endpoint *base-info*) "v" (api-api_version *base-info*)))

(defun api-request (url &key (params nil) (method :get) (headers nil))
  "here we return the result only it no connection/unknown response errors occur."
  (with-fenix-content-types 
      (let ((response nil))
	(setf response (drakma:http-request url :method method :parameters params :additional-headers headers))
	(if (not (listp (jsown:parse response)))
	    (error 'fenixedu-unknown-error :message url ))
	response)))

(defun api-public-request (endpoint &key (params nil) (method :get) (headers nil))
  (let ((url (concatenate 'string (get-api-url) "/" endpoint)))
    (api-request url :params params :method method :headers headers)))


;; API methods 
;; Public Endpoints 
(defun get-about ()
  (parse-json (api-public-request (api-about_endpoint *base-info* ))))

(defun get-course (id)
  (parse-json (api-public-request (concatenate 'string (api-courses_endpoint *base-info*)  "/" id))))

(defun get-course-evaluations (id)
  (parse-json (api-public-request (concatenate 'string (api-courses_endpoint *base-info*) "/" id "/" 
				   (api-evaluations_endpoint *base-info*)))))

(defun get-course-groups (id)
  (parse-json (api-public-request (concatenate 'string (api-courses_endpoint *base-info*) "/" id "/" 
				   (api-groups_endpoint *base-info*)))))


(defun get-course-schedule (id)
  (parse-json (api-public-request (concatenate 'string (api-courses_endpoint *base-info*) "/" id "/" 
					       (api-schedule_endpoint *base-info*)))))


(defun get-course-students (id)
  (parse-json (api-public-request (concatenate 'string (api-courses_endpoint *base-info*) "/" id "/"  
					       (api-students_endpoint *base-info*)))))

(defun get-degrees ( &key (year nil))
  (let ((params nil))
    (if (not (null year))
	(setf params (list (cons "year" year))))
    (parse-json (api-public-request (api-degrees_endpoint *base-info*) :params params))))

(defun get-degree (id &key (year nil))
  (let ((params nil))
    (if (not (null year))
	(setf params (list (cons "year" year))))
    (parse-json (api-public-request (concatenate 'string (api-degrees_endpoint *base-info*) "/" id) 
				    :params params))))

(defun get-degree-courses (id)
  (parse-json (api-public-request (concatenate 'string (api-degrees_endpoint *base-info*) "/" id "/" 
					       (api-courses_endpoint *base-info*)))))

(defun get-spaces ()
  (parse-json (api-public-request (api-spaces_endpoint *base-info*))))


(defun get-space (id &key (day nil))
  (let ((params nil))
    (if (not (null day))
	(setf params (list (cons "day"  day)))
	(parse-json (api-public-request (concatenate 'string (api-spaces_endpoint *base-info*) "/" id) 
					:params params)))))


;; Private Endpoints 
(defun get-person()
  (parse-json (api-private-request (api-person_endpoint *base-info*))))

(defun get-classes-calendar(&key (format "json"))
  (let ((params nil))
    (setf params (list (cons "format" format)))
    (parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						  (api-calendar_endpoint *base-info*) "/" 
						  (api-classes_endpoint *base-info*)) :params params ))))

(defun get-person-curriculum ()
  (parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						(api-curriculum_endpoint *base-info*) ))))

(defun get-evaluations-calendar(&key (format "json"))
  (let ((params nil))
    (setf params (list (cons "format" format)))
    (parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						  (api-calendar_endpoint *base-info*)  "/" 
						  (api-evaluations_endpoint *base-info*)) :params params))))

(defun get-courses (&key (sem nil) (year nil))
  (let ((params nil))
    (if (not (null sem))
	(push (cons "sem" sem) params))
    (if (not (null year))
	(push  (cons "year" year) params))
    (parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						  (api-courses_endpoint *base-info*)) :params params))))

(defun get-evaluations ()
  (parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						(api-evaluations_endpoint *base-info*)))))

(defun get-payments ()
  (parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						(api-payments_endpoint *base-info*)))))

(defun enrol-in-evaluation(id &key (enrol-action nil))
  (let ((params nil))
    (if (not (null enrol-action))
	(setf params (list (cons "enrol" enrol-action)))
	(parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						      (api-evaluations_endpoint *base-info*) "/" id) 
					 :params params :method :put)))))
	
(defun get-evaluation (id)
  (parse-json (api-private-request (concatenate 'string (api-person_endpoint *base-info*) "/" 
						(api-evaluations_endpoint *base-info*) "/" id))))

(defun api-private-request (endpoint  &key (params nil)  (method :get) (headers nil))
    (let ((url (concatenate 'string (get-api-url) "/" endpoint))
	  (result nil)
	  (json-result nil))
      (push (cons "access_token" (api-access_token *base-info*)) params )
      (setf result (api-request url :params params :method method :headers headers))
      ;;(print result)
      (setf json-result (jsown:parse result))
      ;;(print json-result)
      (cond ((not (null (get-response-info json-result "error")))
	     (refresh-access-token)
	     (setf result (api-request url :params params :method method :headers headers))))
      result))


(defun refresh-access-token ()
  (let ((url (concatenate 'string (api-base_url *base-info*) "/" 
			  (api-oauth_endpoint *base-info*) "/" (api-refresh_token_endpoint *base-info*)))
	(params nil)
	(headers nil)
	(result nil)
	(json-result nil))
    (format t "Refreshing access token")
    (setf params (list (cons "client_id" (api-client_id *base-info*)) 
		       (cons "0client_secret" (api-client_secret *base-info*)) 
		       (cons "refresh_token" (api-refresh_token *base-info*)) 
		       (cons "grant_type"  "authorization_code") 
		       (cons "redirect_uri" (api-redirect_uri *base-info*))  
		       (cons "code" (api-code *base-info*))))
    ;;(setf headers (list (cons "content-type" "application/x-www-form-urlencoded")))
    (setf result (api-request url :params params :method :post  :headers headers))
    (setf json-result (jsown:parse result))
    (setf (api-access_token *base-info*) (get-response-info json-result "access_token"))
    (setf (api-expires *base-info*) (get-response-info json-result "expires_in"))))


(defun set-code (code)
  (let ((url (concatenate 'string (api-base_url *base-info*) 
			  (api-oauth_endpoint *base-info*) "/" 
			  (api-access_token_endpoint *base-info*)))
	(params nil)
	(headers nil)
	(result nil)
	(json-result nil))
    (setf params (list (cons "client_id" (api-client_id *base-info*))
		       (cons "client_secret" (api-client_secret *base-info*))
		       (cons "redirect_uri" (api-redirect_uri *base-info*))
		       (cons "code" code)
		       (cons "grant_type" "authorization_code")))
    ;;(setf headers (list (cons "content-type"
    ;;"application/x-www-form-urlencoded")))
    (setf result (api-request url :params params :method :post :headers headers))
    (setf json-result (jsown:parse result))
    (cond ((not (null (get-response-info json-result "error")))
	   (format t "Error tryng to get an access token")
	   (print result))
	  (t
	   (setf (api-access_token *base-info*) (get-response-info json-result "access_token"))
	   (setf (api-refresh_token *base-info*) (get-response-info json-result "refresh_token"))
	   (setf (api-expires *base-info*) (get-response-info json-result "expires_in"))
	   (setf (api-code *base-info*) code)
	   t))))

(defun get-response-info (response info)
  (handler-case
      (if (listp (jsown:val response info))
	  (first (jsown:val response info))
	  (jsown:val response info))
    (simple-error () nil)))

(defun get-authentication-url ()
  (concatenate 'string (api-base_url *base-info*) "/" 
	       (api-oauth_endpoint *base-info*) "/userdialog?client_id=" 
	       (api-client_id *base-info*) "&redirect_uri=" 
	       (api-redirect_uri *base-info*)))