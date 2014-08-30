;;;; h5-crtgrpar.lisp

;;; This example illustrates the creation of groups using absolute and 
;;; relative names.
;;; http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/examples/h5_crtgrpar.c

(in-package :hdf5-cffi)

(defparameter *FILE* "groups.h5")

(let*
    ((fapl (h5pcreate +H5P-FILE-ACCESS+))
     (file (prog2
	       (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
	       ;; create the file
	       (h5fcreate *FILE* '(:trunc) +H5P-DEFAULT+ fapl))))
  (unwind-protect
       (let*
	   ((g1 (h5gcreate2 file "/MyGroup" +H5P-DEFAULT+ +H5P-DEFAULT+
			    +H5P-DEFAULT+))
	    (g2 (h5gcreate2 file "/MyGroup/Group_A" +H5P-DEFAULT+ +H5P-DEFAULT+
			    +H5P-DEFAULT+))
	    (g3 (h5gcreate2 g1 "Group_B" +H5P-DEFAULT+ +H5P-DEFAULT+
			    +H5P-DEFAULT+)))
	 (h5gclose g3)
	 (h5gclose g2)
	 (h5gclose g1))
    ;; cleanup forms
    (h5fclose file)
    (h5pclose fapl)))

(in-package :cl-user)
