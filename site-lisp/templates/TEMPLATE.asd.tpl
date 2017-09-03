#|
 * Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
 *
 * Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
 * Date: (>>>VC_DATE<<<)
 * License: (>>>LICENSE<<<)
|#


;; (in-package #:cl-user)

;; (defpackage #:(>>>FILE_SANS<<<)-asd
;;   (:use #:cl #:asdf)
;;   #+sb-package-locks
;;   (:lock t))

;; (in-package #:(>>>FILE_SANS<<<)-asd)

(defsystem #:(>>>FILE_SANS<<<)
  :version "(>>>Version_number<<<)"
  :author "(>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>"
  :maintainer "(>>>USER_NAME<<<)"
  :license "(>>>LICENSE<<<)."
  :description "(>>>Short_description<<<)"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :depends-on ()
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "(>>>FILE_SANS<<<)")))))


;; (defmethod operation-done-p ((o test-op) (c (eql (find-system :(>>>FILE_SANS<<<)))))
;;   nil)

;; (defmethod perform ((o test-op) (c (eql (find-system :(>>>FILE_SANS<<<)))))
;;   (operate 'load-op :(>>>FILE_SANS<<<)-tests)
;;   (operate 'test-op :(>>>FILE_SANS<<<)-tests))

;; (defmethod perform ((o test-op) (c (eql (find-system :(>>>FILE_SANS<<<)))))
;;   (load-system :(>>>FILE_SANS<<<))
;;   (flet ((run-tests (&rest args)
;;            (apply (intern (string '#:retest-tests) '#:(>>>FILE_SANS<<<)) args)))
;;     (run-tests)))
