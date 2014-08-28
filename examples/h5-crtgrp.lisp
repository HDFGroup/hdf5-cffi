;;;; h5-crtgrp.lisp

;;; This example illustrates how to create and close a group.

(in-package :hdf5-cffi)

(defparameter *FILE* "group.h5")

(let*
    ((f (h5fcreate *FILE* '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+))
     (g (h5gcreate2 f "/MyGroup" +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
  
  ;; close all open handles
  (h5gclose g)
  (h5fclose f))

(in-package :cl-user)
