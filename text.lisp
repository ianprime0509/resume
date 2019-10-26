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

(def-formatter-section text-resume-formatter :basics
  (formatter section-data stream)
  (format stream "~a~%" (property :name section-data t))
  (format-properties formatter
                     '((:address "Address")
                       (:phone "Phone")
                       (:email "Email"))
                     section-data stream)
  (alexandria:when-let ((summary (property :summary section-data)))
    (terpri stream)
    (format-header formatter "Summary" 0 stream)
    (format-text formatter summary stream)
    (terpri stream))
  (format stream "~2%"))

(def-formatter-section text-resume-formatter :education
  (formatter section-data stream)
  (format-header formatter "Education" 0 stream)
  (dolist (education section-data)
    (let* ((school (property :school education t))
           (graduated (property :graduated education t))
           (graduated-string (with-output-to-string (stream)
                               (format-date formatter graduated stream)))
           (header (format nil "~a (~a)" school graduated-string)))
      (format-header formatter header 1 stream))    
    (format-properties formatter
                       '((:degree "Degree")
                         (:gpa "Overall GPA")
                         (:awards "Awards and designations"))
                       education stream)
    (terpri stream))
  (terpri stream))

(def-formatter-section text-resume-formatter :experience
  (formatter section-data stream)
  (format-header formatter "Experience" 0 stream)
  (dolist (experience section-data)
    (let* ((title (property :title experience t))
           (dates (property :dates experience t))
           (date-string (with-output-to-string (stream)
                          (format-date-range formatter dates stream)))
           (header (format nil "~a (~a)" title date-string)))
      (format-header formatter header 1 stream))
    (format-properties formatter
                       '((:organization "Organization")
                         (:location "Location"))
                       experience stream)
    (alexandria:when-let ((experiences (property :experiences experience)))
      (format-list formatter experiences stream))
    (terpri stream))
  (terpri stream))

(def-formatter-section text-resume-formatter :skills
  (formatter section-data stream)
  (format-header formatter "Skills" 0 stream)
  (dolist (skill section-data)
    (format-header formatter (property :skill skill t) 1 stream)
    (alexandria:when-let ((details (property :details skill)))
      (format-list formatter details stream))
    (terpri stream))
  (terpri stream))

(defmethod format-header ((formatter text-resume-formatter) header level stream)
  (format stream "~a~%" header)
  (let ((underline (or (nth level (header-underlines formatter))
                       (last (header-underlines formatter)))))
    (write-string (repeat-string underline (length header)) stream))
  (terpri stream))

(defmethod format-list ((formatter text-resume-formatter) list stream)
  (dolist (element list)
    (format stream "~a " (list-bullet formatter))
    (wrap-text element stream :subsequent-indent "  ")
    (terpri stream)))

(defmethod format-property ((formatter text-resume-formatter) property
                            (value list) stream)
  (format stream "~a:~%" property)
  (format-list formatter value stream))

(defmethod format-property ((formatter text-resume-formatter) property
                            (value string) stream)
  (format stream "~a: " property)
  (wrap-text value stream
             :subsequent-indent (repeat-string " " (+ (length property) 2)))
  (terpri stream))

(defmethod format-text ((formatter text-resume-formatter) text stream)
  (wrap-text text stream))

(defmethod format-date-range ((formatter text-resume-formatter) range stream)
  (if (and (consp range) (consp (car range)))
      (progn
        (format-date formatter (car range) stream)
        (write-string " - " stream)
        (format-date formatter (cdr range) stream))
      (progn
        (format-date formatter range stream)
        (write-string " - Present" stream))))

;;;; text.lisp ends here
