#|
 * Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
 *
 * Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
 * Date: (>>>VC_DATE<<<)
 * License: (>>>LICENSE<<<)
|#


(in-package :cl-user)

(defpackage (>>>FILE_SANS<<<)-asd
  (:use :cl :asdf))

(in-package (>>>FILE_SANS<<<)-asd)

(defsystem (>>>FILE_SANS<<<)
  :version "(>>>Version_number<<<)"
  :author "(>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>"
  :license "(>>>LICENSE<<<)."
  :description "(>>>Short_description<<<)"
  :long-description "(>>>Long_description<<<)"
  :depends-on ()
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
