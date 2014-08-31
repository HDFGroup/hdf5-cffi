;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help @hdfgroup.org.

(asdf:defsystem #:hdf5-cffi
  :serial t
  :description "hdf5-cffi is a CFFI wrapper for the HDF5 library."
  :author "Gerd Heber <gheber@hdfgroup.org>"
  :license "BSD"
  :components ((:file "package")
               (:file "hdf5-cffi")
               (:file "h5")
	       (:file "h5i")
	       (:file "h5f")
	       (:file "h5t")
	       (:file "h5l")
	       (:file "h5o")
	       (:file "h5s")
	       (:file "h5d")
	       (:file "h5g")
	       (:file "h5a")
	       (:file "h5r")
	       (:file "h5z")
	       (:file "h5p"))
  :depends-on ("cffi"))

