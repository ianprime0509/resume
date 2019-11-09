;;;; latex.lisp --- LaTeX resume format

;;;; Commentary:

;;; This file provides a pretty-looking LaTeX resume format (that must
;;; be compiled by LaTeX).

;;;; Code:

(in-package :resume)

(defclass latex-resume-formatter (resume-formatter)
  ((preamble
    :initarg preamble
    :initform #.(alexandria:read-file-into-string "latex-preamble.tex")
    :accessor preamble))
  (:documentation "A pretty resume formatter using LaTeX as an output format."))

(defmethod format-resume :before ((formatter latex-resume-formatter)
                                  resume-data stream)
  (declare (ignore resume-data))
  (write-string (preamble formatter) stream)
  (format stream "\\begin{document}~%"))

(defmethod format-resume :after ((formatter latex-resume-formatter)
                                 resume-data stream)
  (declare (ignore formatter resume-data))
  (format stream "\\end{document}~%"))

(defmethod format-section ((formatter latex-resume-formatter)
                           (section (eql :basics)) section-data stream)
  (with-property-values (name address phone email summary) section-data
    (format stream "\\begin{center}~%")
    (format stream "  {\\Large\\bfseries ~a} \\\\~%" name)
    (loop for property in (list address phone email)
       when property
       do (write-string "  ")
         (format-value formatter property stream)
         (format stream " \\\\~%"))
    (format stream "\\end{center}~%")
    (when summary
      (terpri stream)
      (format-header formatter "Summary" 0 stream)
      (format-value formatter summary stream)
      (terpri stream)))
  (terpri stream))

(def-section-item latex-resume-formatter :education
    (formatter education stream)
  (with-property-values (school graduated degree gpa awards) education
    (let ((gpa-string (format nil "Overall GPA: ~a" gpa)))
      (format-entry formatter school graduated degree gpa-string stream))
    (when awards (format-value formatter awards stream)))
  (terpri stream))

(def-section-item latex-resume-formatter :experience
    (formatter experience stream)
  (with-property-values (title dates organization location experiences)
      experience
    (format-entry formatter title dates organization location stream)
    (when experiences (format-value formatter experiences stream)))
  (terpri stream))

(def-section-item latex-resume-formatter :skills
    (formatter skill stream)
  (with-property-values (skill details) skill
    (format-header formatter skill 1 stream)
    (when details (format-value formatter details stream)))
 (terpri stream))

(defmethod format-entry ((formatter latex-resume-formatter)
                         top-left top-right bottom-left bottom-right
                         stream)
  (write-string "\\entry" stream)
  (loop for value in (list top-left top-right bottom-left bottom-right)
     do (write-string "{" stream)
     if value do (format-value formatter value stream)
     do (format stream "}~%")))

(defmethod format-header ((formatter latex-resume-formatter)
                          header level stream)
  (format stream "\\~a{~a}~%"
          (if (= level 0) "section*" "textbf")
          (escape formatter header)))

(defmethod format-value ((formatter latex-resume-formatter) (list list) stream)
  (format stream "\\begin{itemize}~%")
  (dolist (value list)
    (write-string "\\item " stream)
    (let* ((*wrap-subsequent-indent* "  ")
           (*wrap-initial-offset* 6))
      (format-value formatter value stream))
    (terpri stream))
  (format stream "\\end{itemize}~%"))

(defmethod format-value ((formatter latex-resume-formatter) (range date-range)
                         stream)
  (format-value formatter (start range) stream)
  (write-string "--" stream)
  (format-value formatter (or (end range) "Present") stream))

(defmethod escape ((formatter latex-resume-formatter) text)
  ;; TODO
  text)

;;;; latex.lisp ends here
