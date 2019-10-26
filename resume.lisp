;;;; resume.lisp --- core format-agnostic resume functions

;;;; Commentary:

;;; TODO

;;;; Code:

(in-package :resume)

(defclass resume-formatter ()
  ((sections
    :initarg sections
    :initform '(:education :experience :skills)
    :accessor sections
    :documentation "The sections of the resume, in the order they will be formatted."))
  (:documentation "A formatter for resume data."))

(define-condition missing-section-error (error)
  ((section :initarg section :reader missing-section))
  (:documentation "An error indicating that a required resume section is missing."))

(define-condition missing-property-error (error)
  ((property :initarg property :reader missing-property))
  (:documentation "An error indicating that a required section property is missing."))

(define-condition unsupported-section-error (error)
  ((section :initarg section :reader unsupported-section))
  (:documentation "An error indicating that a desired resume section is not supported by the formatter."))

(defgeneric format-resume (formatter resume-data stream)
  (:documentation "Format RESUME-DATA to STREAM using FORMATTER."))

(defgeneric format-section (formatter section section-data stream)
  (:documentation "Format SECTION-DATA, the data for the resume section identified by SECTION, to STREAM using FORMATTER."))

(defgeneric format-header (formatter header level stream)
  (:documentation "Format HEADER as a header to STREAM using FORMATTER.
LEVEL is a zero-based header level, with 0 being the top-most header."))

(defgeneric format-list (formatter list stream)
  (:documentation "Format LIST, a list of strings, to STREAM using FORMATTER."))

(defgeneric format-property (formatter property value stream)
  (:documentation "Format a key-value property with key PROPERTY and value VALUE to STREAM using FORMATTER."))

(defgeneric format-text (formatter text stream)
  (:documentation "Format TEXT to STREAM using FORMATTER."))

(defgeneric format-date (formatter date stream)
  (:documentation "Format the date DATE to STREAM using FORMATTER.
A date is a list containing a year, month and day, in that order.
Smaller components can be omitted if a date of that specificity is not
known or desired. If only a year remains, the date may be just a
number."))

(defgeneric format-date-range (formatter range stream)
  (:documentation "Format the date range RANGE to STREAM using FORMATTER.
A date range is either a cons cell containing start and end dates or
just a date if there is no end date."))

;;; Common method implementations

(defmethod format-resume ((formatter resume-formatter) resume-data stream)
  (flet ((format-section (section &optional required)
           (let ((section-data (cdr (assoc section resume-data))))
             (if section-data
                 (format-section formatter section section-data stream)
                 (when required
                   (error 'missing-section-error :section section))))))
    ;; The basics section is always formatted and always required
    (format-section :basics t)
    (dolist (section (sections formatter))
      (ctypecase section
        (keyword (format-section section))
        (list (format-section (first section)
                              (member :required (rest section))))))))

(defparameter *month-names*
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December")
  "The names of the months of the year.")

(defmethod format-date ((formatter resume-formatter) date stream)
  (if (numberp date)
      (format-date formatter (list date) stream)
      (destructuring-bind (year &optional month day) date
        (when month
          (format stream "~a " (nth (1+ month) *month-names*))
          (when day
            (format stream "~a, " day)))
        (format stream "~a" year))))

;;; Utility functions

(defmacro def-formatter-section (formatter-class section
                                 (formatter section-data stream)
                                 &body body)
  "Define a specialization of FORMAT-SECTION on FORMATTER-CLASS (the class of the formatter) and SECTION (the section keyword)."
  (let ((section-param (gensym)))
    `(defmethod format-section ((,formatter ,formatter-class)
                                (,section-param (eql ,section))
                                ,section-data ,stream)
       ,@body)))

(defun format-properties (formatter properties section-data stream)
  "Format PROPERTIES from SECTION-DATA to STREAM using FORMATTER.
PROPERTIES is a list of descriptions of the properties to pull from
SECTION-DATA. Each description is a list of the form (KEY NAME &KEY
METHOD REQUIRED), where KEY is the property key, NAME is the name of
the property (as displayed in the formatted output) and REQUIRED, if
non-nil, indicates that the property is required."
  (dolist (property properties)
    (destructuring-bind (key name &key required) property
      (alexandria:when-let (value (property key section-data required))
        (format-property formatter name value stream)))))

(defun property (property section-data &optional required)
  "Return the property PROPERTY in SECTION-DATA.
If the property is not present and REQUIRED is NIL, then return NIL.
Otherwise, signal an error of type MISSING-PROPERTY-ERROR."
  (let ((property-data (cadr (assoc property section-data))))
    (when (and required (not property-data))
      (error 'missing-property-error :property property))
    property-data))

(defun repeat-string (string times)
  "Return the result of repeating STRING TIMES times."
  (with-output-to-string (output)
    (loop repeat times do (write-string string output))))

(defun wrap-text (text stream &key (subsequent-indent "") (width 80) (initial-offset 0))
  "Write TEXT to STREAM, wrapping lines at word breaks so that the length of each line does not exceed WIDTH.
SUBSEQUENT-INDENT (default space) is output before each new line.
INITIAL-OFFSET is the subtracted from the maximum width of the first
line: if INITIAL-OFFSET is 10 and WIDTH is 80, the first line of text
will be at most 70 characters long."
  (let ((words (cl-utilities:split-sequence #\Space text
                                            :remove-empty-subseqs t))
        (length initial-offset)
        (start-of-line t))
    (dolist (word words)
      (when (> (+ length (length word)) width)
        (format stream "~%~a" subsequent-indent)
        (setf length (length subsequent-indent))
        (setf start-of-line t))
      (unless start-of-line
        (write-string " " stream)
        (incf length))
      (write-string word stream)
      (setf length (+ length (length word)))
      (setf start-of-line nil))))

;;;; resume.lisp ends here
