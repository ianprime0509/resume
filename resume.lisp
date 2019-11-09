;;;; resume.lisp --- core format-agnostic resume functions

;;;; Commentary:

;;; TODO

;;;; Code:

(in-package :resume)

(defparameter *resume-structure*
  '((:basics ((:name string :required t)
              (:address string)
              (:phone phone)
              (:email email)
              (:summary string))
     :required t
     :single t)
    (:education ((:school string :required t)
                 (:graduated date :required t)
                 (:degree string)
                 (:gpa string)
                 (:awards (list string))))
    (:experience ((:title string :required t)
                  (:dates date-range :required t)
                  (:organization string)
                  (:location string)
                  (:experiences (list string))))
    (:skills ((:skill string :required t)
              (:details (list string)))))
  "An alist of sections and their structures.
The structure of a section is specified as an alist of property keys
and arguments of the form (TYPE &KEY :REQUIRED). TYPE is the type of
the property, to which the value will be converted, and REQUIRED, if
true, enforces that the property is provided for each entry in the
section.")

;;; Data types

(defclass resume-formatter ()
  ((sections
    :initarg sections
    :initform '((:education "Education")
                (:experience "Experience")
                (:skills "Skills"))
    :reader sections
    :documentation "The sections of the resume, in the order they will be formatted."))
  (:documentation "A formatter for resume data."))

(defgeneric convert-value (type value)
  (:documentation "Convert VALUE to TYPE."))

(defmethod convert-value ((type (eql 'string)) value)
  (string value))

(defmethod convert-value ((type list) value)
  (let ((element-type (second type)))
    (mapcar #'(lambda (value) (convert-value element-type value))
            value)))

(defclass date ()
  ((year
    :type 'integer
    :initarg :year
    :initform (error "Must provide at least a year for a date.")
    :reader year)
   (month
    :type '(or (integer 1 12) null)
    :initarg :month
    :initform nil
    :reader month)
   (day
    :type '(or(integer 1 31) null)
    :initarg :day
    :initform nil
    :reader day))
  (:documentation "A date, consisting of a year and, optionally, a month and day."))

(defmethod initialize-instance :after ((date date) &key)
  (when (and (null (month date)) (not (null (day date))))
    (error "If a day is provided, a month must be provided as well.")))

(defmethod print-object ((date date) stream)
  (print-unreadable-object (date stream :type t)
    (format stream "~d ~d ~d" (year date) (month date) (day date))))

(defmethod convert-value ((type (eql 'date)) date)
  "Convert DATE, which is either a list (YEAR &OPTIONAL MONTH DAY) or a number YEAR to a DATE."
  (if (integerp date)
      (make-instance 'date :year date)
      (destructuring-bind (year &optional month day) date
        (make-instance 'date :year year :month month :day day))))

(defclass date-range ()
  ((start
    :type 'date
    :initarg :start
    :initform (error "Must provide a start date.")
    :reader start)
   (end
    :type '(or date null)
    :initarg :end
    :initform nil
    :reader end)))

(defmethod print-object ((range date-range) stream)
  (print-unreadable-object (range stream :type t)
    (format stream "~a ~a" (start range) (end range))))

(defmethod convert-value ((type (eql 'date-range)) range)
  "Convert RANGE, which is a pair of dates (START . END), to a DATE-RANGE."
  (make-instance 'date-range
                 :start (convert-value 'date (car range))
                 :end (when (cdr range) (convert-value 'date (cdr range)))))

(defclass phone ()
  ((phone-number
    :type 'string
    :initarg :phone-number
    :initform (error "Must provide a phone number.")
    :reader phone-number)))

(defmethod print-object ((phone phone) stream)
  (print-unreadable-object (phone stream :type t)
    (format stream "~a" (phone-number phone))))

(defmethod convert-value ((type (eql 'phone)) phone-number)
  "Convert PHONE-NUMBER, a string containing a phone number, to a PHONE."
  (make-instance 'phone :phone-number phone-number))

(defclass email ()
  ((email-address
    :type 'string
    :initarg :email-address
    :initform (error "Must provide an email address.")
    :reader email-address)))

(defmethod print-object ((email email) stream)
  (print-unreadable-object (email stream :type t)
    (format stream "~a" (email-address email))))

(defmethod convert-value ((type (eql 'email)) email-address)
  "Convert EMAIL-ADDRESS, a string containing an email address, to an EMAIL."
  (make-instance 'email :email-address email-address))

;;; Conditions

(define-condition missing-section-error (error)
  ((section :initarg section :reader missing-section))
  (:documentation "An error indicating that a required resume section is missing."))

(define-condition missing-property-error (error)
  ((property :initarg property :reader missing-property))
  (:documentation "An error indicating that a required section property is missing."))

(define-condition unsupported-section-error (error)
  ((section :initarg section :reader unsupported-section))
  (:documentation "An error indicating that a desired resume section is not supported by the formatter."))

;;; Methods

(defgeneric format-resume (formatter resume-data stream)
  (:documentation "Format RESUME-DATA to STREAM using FORMATTER."))

(defgeneric format-section (formatter section section-data stream)
  (:documentation "Format SECTION-DATA, the data for the resume section identified by SECTION, to STREAM using FORMATTER."))

(defgeneric format-section-item (formatter section item-data stream)
  (:documentation "Format ITEM-DATA, the data for a single item in SECTION, to STREAM using FORMATTER."))

(defgeneric format-header (formatter header level stream)
  (:documentation "Format HEADER as a header to STREAM using FORMATTER.
LEVEL is a zero-based header level, with 0 being the top-most header."))

(defgeneric format-value (formatter value stream)
  (:documentation "Format VALUE to STREAM using FORMATTER."))

(defgeneric format-property (formatter property value stream)
  (:documentation "Format a key-value property with key PROPERTY and value VALUE to STREAM using FORMATTER."))

(defgeneric escape (formatter text)
  (:documentation "Escape TEXT according to the format used by FORMATTER.
For example, an HTML output format will want to escape the & and <
characters."))

;;; Common method implementations

(defmethod format-resume ((formatter resume-formatter) resume-data stream)
  (let ((resume (convert-resume resume-data *resume-structure*)))
    ;; The basics section is always formatted
    (format-section formatter :basics (cdr (assoc :basics resume)) stream)
    (dolist (section (sections formatter))
      (destructuring-bind (name header) section
        (alexandria:when-let ((section-data (cdr (assoc name resume))))
          (format-header formatter header 0 stream)
          (format-section formatter name section-data stream))))))

(defmethod format-section ((formatter resume-formatter) section
                           section-data stream)
  (dolist (item-data section-data)
    (format-section-item formatter section item-data stream)))

(defparameter *wrap-subsequent-indent* "")
(defparameter *wrap-initial-offset* 0)

(defmethod format-value ((formatter resume-formatter) (text string) stream)
  "Format TEXT, a string, to STREAM, escaping and wrapping it.
This method passes the values of *WRAP-SUBSEQUENT-INDENT* and
*WRAP-INITIAL-OFFSET* to the corresponding keyword arguments of
WRAP-TEXT."
  (wrap-text (escape formatter text) stream
             :subsequent-indent *wrap-subsequent-indent*
             :initial-offset *wrap-initial-offset*))

(defparameter *month-names*
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December")
  "The names of the months of the year.")

(defmethod format-value ((formatter resume-formatter) (date date) stream)
  "Format DATE to STREAM in the format `Month Day, Year'."
  (with-accessors ((year year) (month month) (day day)) date
    (when month
      (format stream "~a " (nth (1+ month) *month-names*))
      (when day
        (format stream "~a, " day)))
    (format stream "~a" year)))

(defmethod format-value ((formatter resume-formatter) (email email) stream)
  (write-string (escape formatter (email-address email)) stream))

(defmethod format-value ((formatter resume-formatter) (phone phone) stream)
  (write-string (escape formatter (phone-number phone)) stream))

(defmethod escape ((formatter resume-formatter) text)
  text)

;;; Utility functions

(defun convert-resume (resume-data resume-structure)
  "Convert and validate the types of properties in RESUME-DATA according to RESUME-STRUCTURE.
Any sections or properties not described in RESUME-STRUCTURE will not
be present in the returned alist."
  (let ((resume nil))
    (dolist (section-description resume-structure resume)
      (destructuring-bind (name structure &key required single) section-description
        (let ((section-data (cdr (assoc name resume-data))))
          (when (and required (null section-data))
            (error 'missing-section-error :section name))
          (when section-data
            (let ((converted-section
                   (if single
                       (convert-section-item section-data structure)
                       (loop for section-item in section-data collect
                            (convert-section-item section-item structure)))))
              (push (cons name converted-section) resume))))))))

(defun convert-section-item (section-item section-structure)
  "Convert and validate the types of properties in SECTION-ITEM according to SECTION-STRUCTURE.
Any properties not described in SECTION-STRUCTURE will not be present
in the returned alist."
  (let ((section nil))
    (dolist (property section-structure section)
      (destructuring-bind (name type &key required) property
        (let ((value (cadr (assoc name section-item))))
          (when (and required (null value))
            (error 'missing-property-error :property name))
          (when value
            (push (cons name (convert-value type value)) section)))))))

(defmacro def-section-item (formatter-class section
                            (formatter item-data stream)
                            &body body)
  "Define a specialization of FORMAT-SECTION-ITEM on FORMATTER-CLASS (the class of the formatter) and SECTION (the section keyword)."
  (let ((section-param (gensym)))
    `(defmethod format-section-item ((,formatter ,formatter-class)
                                     (,section-param (eql ,section))
                                     ,item-data ,stream)
       ,@body)))

(defun format-properties (formatter properties section-data stream)
  "Format PROPERTIES from SECTION-DATA to STREAM using FORMATTER.
PROPERTIES is a list of descriptions of the properties to pull from
SECTION-DATA. Each description is a list of the form (KEY NAME), where
KEY is the property key and NAME is the name of the property (as
displayed in the formatted output)."
  (dolist (property properties)
    (destructuring-bind (key name) property
      (alexandria:when-let (value (property-value key section-data))
        (format-property formatter name value stream)))))

(defun property-value (property section-data)
  "Return the value of PROPERTY in the alist SECTION-DATA or NIL if there is no such property."
  (cdr (assoc property section-data)))

(defun repeat-string (string times)
  "Return the result of repeating STRING TIMES times."
  (with-output-to-string (output)
    (loop repeat times do (write-string string output))))

(defmacro with-property-values (properties section-data &body body)
  "Establishes bindings for each property in PROPERTIES in SECTION-DATA usable within BODY.
Each property in PROPERTIES is either a list (VARIABLE PROPERTY-NAME)
or just VARIABLE if the two are the same (in this case, PROPERTY-NAME
is just the keyword with the same name as VARIABLE)."
  (let ((data (gensym)))
    (flet ((make-binding (variable property)
             `(,variable (property-value ,property ,data))))
      (let ((bindings
             (loop for property in properties collect
                  (ctypecase property
                    (list
                     (destructuring-bind (variable property) property
                       (make-binding variable property)))
                    (symbol
                     (make-binding property
                                   (intern (string property) 'keyword)))))))
        `(let ((,data ,section-data))
           (symbol-macrolet ,bindings ,@body))))))

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
