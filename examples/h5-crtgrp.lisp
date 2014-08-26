;;;; h5-crtgrp.lisp

;;; This example illustrates how to create and close a group.

(in-package :hdf5-cffi)

(let*
    ((f (h5fcreate "group.h5" '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+)) ; file
     (g (h5gcreate2 f "/MyGroup" +H5P-DEFAULT+ +H5P-DEFAULT+
		    +H5P-DEFAULT+)))                                  ; group
  ;; close all open handles
  (h5gclose g)
  (h5fclose f))

(in-package :cl-user)
