
(in-package #:hdf5)

(defparameter *test-failures* nil)

(defun testhdf5 ()
  (load-hdf5-foreign-libraries)
  (do-external-symbols (s (find-package "HDF5"))
    (print s)))

#+nil(eval-when (:load-toplevel :execute)
       (testhdf5)
       #+sbcl(sb-ext:quit)
       #+cmu(ext:quit))
