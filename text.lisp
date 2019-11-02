;;;; text.lisp --- plain-text resume format

;;;; Commentary:

;;; This file provides a simple plain-text resume format.

;;;; Code:

(in-package :resume)

(defclass text-resume-formatter (resume-formatter)
  ((header-underlines
    :initarg header-underlines
    :initform '("=" "-")
    :accessor header-underlines)
   (list-bullet
    :initarg list-bullet
    :initform "*"
    :accessor list-bullet))
  (:documentation "A simple, plain-text resume formatter."))

(defmethod format-section ((formatter text-resume-formatter)
                           (section (eql :basics)) section-data stream)
  (format stream "~a~%" (property-value :name section-data))
  (format-properties formatter
                     '((:address "Address")
                       (:phone "Phone")
                       (:email "Email"))
                     section-data stream)
  (alexandria:when-let ((summary (property-value :summary section-data)))
    (terpri stream)
    (format-header formatter "Summary" 0 stream)
    (format-value formatter summary stream)
    (terpri stream))
  (terpri stream))

(def-section-item text-resume-formatter :education
    (formatter education stream)
  (let* ((school (property-value :school education))
         (graduated (property-value :graduated education))
         (graduated-string (with-output-to-string (stream)
                             (format-value formatter graduated stream)))
         (header (format nil "~a (~a)" school graduated-string)))
    (format-header formatter header 1 stream))
  (format-properties formatter
                     '((:degree "Degree")
                       (:gpa "Overall GPA")
                       (:awards "Awards and designations"))
                     education stream)
  (terpri stream))

(def-section-item text-resume-formatter :experience
    (formatter experience stream)
  (let* ((title (property-value :title experience))
         (dates (property-value :dates experience))
         (date-string (with-output-to-string (stream)
                        (format-value formatter dates stream)))
         (header (format nil "~a (~a)" title date-string)))
    (format-header formatter header 1 stream))
  (format-properties formatter
                     '((:organization "Organization")
                       (:location "Location"))
                     experience stream)
  (alexandria:when-let ((experiences (property-value :experiences experience)))
    (format-value formatter experiences stream))
  (terpri stream))

(def-section-item text-resume-formatter :skills
    (formatter skill stream)
  (format-header formatter (property-value :skill skill) 1 stream)
  (alexandria:when-let ((details (property-value :details skill)))
    (format-value formatter details stream))
  (terpri stream))

(defmethod format-header ((formatter text-resume-formatter) header level stream)
  (format stream "~a~%" (escape formatter header))
  (let ((underline (or (nth level (header-underlines formatter))
                       (last (header-underlines formatter)))))
    (write-string (repeat-string underline (length header)) stream))
  (terpri stream))

(defmethod format-property ((formatter text-resume-formatter) property
                            (value list) stream)
  (format stream "~a:~%" (escape formatter property))
  (format-value formatter value stream))

(defmethod format-property ((formatter text-resume-formatter) property
                            value stream)
  (format stream "~a: " (escape formatter property))
  (let* ((indent (+ (length property) 2))
         (*wrap-subsequent-indent*
          ;; The concatenation here handles multiple levels of
          ;; indentation
          (concatenate 'string
                       *wrap-subsequent-indent*
                       (repeat-string " " indent))))
    (format-value formatter value stream))
  (terpri stream))

(defmethod format-value ((formatter text-resume-formatter) (list list) stream)
  (dolist (element list)
    (let ((bullet (list-bullet formatter)))
      (format stream "~a " bullet)
      (let* ((indent (1+ (length bullet)))
             (*wrap-subsequent-indent*
              (concatenate 'string
                           *wrap-subsequent-indent*
                           (repeat-string " " indent)))
             (*wrap-initial-offset* (+ *wrap-initial-offset* indent)))
        (format-value formatter element stream)))
    (terpri stream)))

(defmethod format-value ((formatter text-resume-formatter) (range date-range)
                         stream)
  (format-value formatter (start range) stream)
  (write-string " - " stream)
  (format-value formatter (or (end range) "Present") stream))

;;;; text.lisp ends here
