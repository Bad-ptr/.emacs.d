#|
 * Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
 *
 * Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
 * Date: (>>>VC_DATE<<<)
 * License: (>>>LICENSE<<<)
|#


(in-package #:cl-user)

(defpackage #:(>>>LISP_PROJECT_NAME<<<)
  (:documentation "(>>>Documentation<<<)")
  (:use #:cl)
  #+sb-package-locks
  (:lock t))

(in-package #:(>>>LISP_PROJECT_NAME<<<))

(>>>POINT<<<)
