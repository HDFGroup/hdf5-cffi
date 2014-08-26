;;;; h5-crtgrpar.lisp

;;; This example illustrates the creation of groups using absolute and 
;;  relative names.

(in-package :hdf5-cffi)

(let*
    ((f (h5fcreate "groups.h5" '(:trunc) +H5P-DEFAULT+ +H5P-DEFAULT+)) ; file

     ;; Create group "MyGroup" in the root group using absolute name.
     (g1 (h5gcreate2 f "/MyGroup" +H5P-DEFAULT+ +H5P-DEFAULT+
		     +H5P-DEFAULT+))
     
     ;; Create group "Group_A" in group "MyGroup" using absolute name.
     (g2 (h5gcreate2 f "/MyGroup/Group_A" +H5P-DEFAULT+ +H5P-DEFAULT+
		     +H5P-DEFAULT+))
     
     ;; Create group "Group_B" in group "MyGroup" using relative name.
     (g3 (h5gcreate2 g1 "Group_B" +H5P-DEFAULT+ +H5P-DEFAULT+
		    +H5P-DEFAULT+)))

  ;; close all open handles
  (h5gclose g3)
  (h5gclose g2)
  (h5gclose g1)
  (h5fclose f))

(in-package :cl-user)
