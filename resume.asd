;;;; resume.asd --- ASDF system definition for resume

(defsystem "resume"
  :description "A system for writing resumes in various formats"
  :version "0.1.0"
  :author "Ian Johnson <ianprime0509@gmail.com>"
  :licence "MIT"
  :depends-on ("alexandria" "cl-utilities")
  :components ((:file "packages")
               (:file "resume" :depends-on ("packages"))
               (:file "latex" :depends-on ("resume"))
               (:file "text" :depends-on ("resume"))))

;;;; resume.asd ends here
