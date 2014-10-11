;;;; Copyright by The HDF Group.                                              
;;;; All rights reserved.
;;;;
;;;; This file is part of hdf5-cffi.
;;;; The full hdf5-cffi copyright notice, including terms governing
;;;; use, modification, and redistribution, is contained in the file COPYING,
;;;; which can be found at the root of the source code distribution tree.
;;;; If you do not have access to this file, you may request a copy from
;;;; help@hdfgroup.org.

;;; This example shows how to read and write a complex
;;; compound datatype to a dataset.  The program first writes
;;; complex compound structures to a dataset with a dataspace
;;; of DIM0, then closes the file.  Next, it reopens the file,
;;; reads back selected fields in the structure, and outputs
;;; them to the screen.

;;; Unlike the other datatype examples, in this example we
;;; save to the file using native datatypes to simplify the
;;; type definitions here.  To save using standard types you
;;; must manually calculate the sizes and offsets of compound
;;; types as shown in h5ex_t_cmpd.c, and convert enumerated
;;; values as shown in h5ex_t_enum.c.

;;; The datatype defined here consists of a compound
;;; containing a variable-length list of compound types, as
;;; well as a variable-length string, enumeration, double
;;;  array, object reference and region reference.  The nested
;;;  compound type contains an int, variable-length string and
;;;  two doubles.

;;; http://www.hdfgroup.org/ftp/HDF5/examples/examples-by-api/hdf5-examples/1_8/C/H5T/h5ex_t_cpxcmpd.c


#+sbcl(require 'asdf)
(asdf:operate 'asdf:load-op 'hdf5-cffi)

(in-package :hdf5)

(defparameter *FILE*    "h5ex_t_cpxcmpd.h5")
(defparameter *DATASET* "DS1")
(defparameter *DIM0*    2)

(cffi:defcstruct sensor-t
    "sensor_t"
  (serial-no   :int)
  (location    :string)
  (temperature :double)
  (pressure    :double))

(cffi:defcenum color-t
    "color_t"
  :RED
  :GREEN
  :BLUE)

(cffi:defcstruct vehicle-t
    "vehicle_t"
  (sensors        (:struct hvl-t))
  (name           :string)
  (color          color-t)
  (location       :double :count 3)
  (group          hobj-ref-t)
  (surveyed-areas (:struct hdset-reg-ref-t)))

(cffi:defcstruct rvehicle-t
    "rvehicle_t"
  (sensors (:struct hvl-t))
  (name    :string))



(let*
    ((fapl (h5pcreate +H5P-FILE-ACCESS+))
     (file (prog2
	       (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
	       (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl))))
  
  (unwind-protect
       
       (format t "Howdy!~%")
	 
    (h5fclose file)
    (h5pclose fapl)))

#+sbcl(sb-ext:quit)
