(in-package #:fenixedu-commonlisp-sdk)

(defun string-contains (string tofind)
  (if (not (null (cl-ppcre:scan tofind string)))
      T
      nil))

(defun get-content-type (resp-type)
  (cdr (assoc ':content-type resp-type))) 

;; (defun get-filename (filename filetype)
;;   (if (null (probe-file (concatenate 'string filename "." filetype)))
;;       (return-from get-filename (concatenate 'string filename "." filetype)))
;;   (let ((counter 1))
;;     (loop 
;;        (if (null (probe-file (concatenate 'string filename " (" (format nil "~A" counter) ")" "." filetype)))
;; 	   (return-from get-filename (concatenate 'string filename " (" (format nil "~A" counter) ")" "." filetype)))
;;        (incf counter))))

(defun write-to-file (filename content)
  (write-bytes-to-file content :file-path filename))

(defun write-bytes-to-file (buffer &key (file-path "data.struct"))
  (let (length)
    (setf length (length buffer))
    (with-open-file (stream (merge-pathnames file-path)
			    :element-type '(unsigned-byte 8)
			    :direction :output
			    :if-exists :rename)
      (write-sequence buffer stream)))
  file-path)
				
  
