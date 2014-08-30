;;;; h5-crtgrp.lisp

;;; This example illustrates how to create and close a group.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtgrp.c

(in-package :hdf5-cffi)

(defparameter *FILE* "group.h5")

(let*
    ((fapl (h5pcreate +H5P-FILE-ACCESS+))
     (file (prog2
	       (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
	       ;; create the file
	       (h5fcreate *FILE* '(:trunc) +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let*
	   ((g (h5gcreate2 file "/MyGroup" +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	 (h5gclose g))
    ;; cleanup forms
    (h5fclose file)
    (h5pclose fapl)))

(in-package :cl-user)
