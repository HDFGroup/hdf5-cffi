;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem hdf5-cffi
  :serial t
  :description "hdf5-cffi is a CFFI wrapper for the HDF5 library."
  :version "0.0.1"
  :author "Gerd Heber <gheber@hdfgroup.org>"
  :license "BSD"
  :depends-on (:cffi :cffi-grovel)
  :components
  ((:file "package")
   (:file "hdf5-cffi")
   (cffi-grovel:grovel-file "grovel")
   (cffi-grovel:grovel-file "h5-grovel")
   (:file "h5")
   (cffi-grovel:grovel-file "h5i-grovel")
   (:file "h5i")
   (cffi-grovel:grovel-file "h5f-grovel")
   (:file "h5f")
   (cffi-grovel:grovel-file "h5t-grovel")
   (:file "h5t")
   (cffi-grovel:grovel-file "h5l-grovel")
   (:file "h5l")
   (cffi-grovel:grovel-file "h5o-grovel")
   (:file "h5o")
;	       (:file "h5s")
;	       (:file "h5d")
;	       (:file "h5g")
;	       (:file "h5a")
;	       (:file "h5r")
;	       (:file "h5z")
					;	       (:file "h5p")))
	       ))
	       
