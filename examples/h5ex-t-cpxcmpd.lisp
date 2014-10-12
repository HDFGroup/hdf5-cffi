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
(defparameter *LEN*     4)

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

(cffi:with-foreign-objects
    ((dims 'hsize-t 1)
     (adims 'hsize-t 1)
     (adims2 'hsize-t 2)
     (start 'hsize-t 2)
     (count 'hsize-t 2)
     (coords 'hsize-t (* 3 2))
     (wdata  '(:struct vehicle-t) 2)
     (val    'color-t)
     (wdata2 :double (* 32 32)))

  (let*
      ((fapl (h5pcreate +H5P-FILE-ACCESS+))
       (file (prog2
		 (h5pset-fclose-degree fapl :H5F-CLOSE-STRONG)
		 (h5fcreate *FILE* +H5F-ACC-TRUNC+ +H5P-DEFAULT+ fapl)))
       (ptr (cffi:foreign-alloc '(:struct sensor-t) :count *LEN*)))
  
    (unwind-protect

	 (progn
	   ;; create a dataset to use for region references
	   (dotimes (i 32)
	     (dotimes (j 32)
	       (setf (cffi:mem-aref wdata2 :double (+ (* i 32) j))
		     (+ 70.0d0
			(* 0.1d0 (- i 16.0d0))
			(* 0.1d0 (- j 16.0d0))))))
	   (let*
	       ((shape (prog2
			   (setf (cffi:mem-aref adims2 'hsize-t 0) 32
				 (cffi:mem-aref adims2 'hsize-t 1) 32)
			   (h5screate-simple 2 adims2 (cffi:null-pointer))))
		(dset (h5dcreate2 file "Ambient Temperature"
				  +H5T-NATIVE-DOUBLE+ shape
				  +H5P-DEFAULT+ +H5P-DEFAULT+ +H5P-DEFAULT+)))
	     (h5dwrite dset +H5T-NATIVE-DOUBLE+ +H5S-ALL+ +H5S-ALL+
		       +H5P-DEFAULT+ wdata2)
	     (h5dclose dset)
	     (h5sclose shape))

	   ;; create groups to use for object references
	   (h5gclose (h5gcreate2 file "Land_Vehicles" +H5P-DEFAULT+
				 +H5P-DEFAULT+ +H5P-DEFAULT+))
	   (h5gclose (h5gcreate2 file "Air_Vehicles" +H5P-DEFAULT+
				 +H5P-DEFAULT+ +H5P-DEFAULT+))

	   ;; Initialize variable-length compound in the first data element.
	   (setf (cffi:foreign-slot-value
		  (cffi:foreign-slot-pointer
		   (cffi:mem-aptr wdata '(:struct vehicle-t) 0)
		   '(:struct vehicle-t) 'sensors)
		  '(:struct hvl-t) 'len) *LEN*)
	   (let*
	       ((ptr[0] (cffi:mem-aptr ptr '(:struct sensor-t) 0))
		(ptr[1] (cffi:mem-aptr ptr '(:struct sensor-t) 1))
		(ptr[2] (cffi:mem-aptr ptr '(:struct sensor-t) 2))
		(ptr[3] (cffi:mem-aptr ptr '(:struct sensor-t) 3)))
	     (cffi:with-foreign-slots
		 ((serial-no location temperature pressure)
		  ptr[0] (:struct sensor-t))
	       (setf serial-no 1153 location "Exterior (static)"
		     temperature 53.23d0 pressure 24.57d0))
	     (cffi:with-foreign-slots
		 ((serial-no location temperature pressure)
		  ptr[1] (:struct sensor-t))
	       (setf serial-no 1184 location "Intake"
		     temperature 55.12d0 pressure 22.95d0))
	     (cffi:with-foreign-slots
		 ((serial-no location temperature pressure)
		  ptr[2] (:struct sensor-t))
	       (setf serial-no 1027 location "Intake manifold"
		     temperature 103.55d0 pressure 31.23d0))
	     (cffi:with-foreign-slots
		 ((serial-no location temperature pressure)
		  ptr[3] (:struct sensor-t))
	       (setf serial-no 1313 location "Exhaust manifold"
		     temperature 1252.89d0 pressure 84.11d0)))
	   (setf (cffi:foreign-slot-value
		  (cffi:foreign-slot-pointer
		   (cffi:mem-aptr wdata '(:struct vehicle-t) 0)
		   '(:struct vehicle-t) 'sensors)
		  '(:struct hvl-t) 'p) ptr)

	   ;; Initialize other fields in the first data element.
	   
	   )

      (cffi:foreign-free ptr)
      (h5fclose file)
      (h5pclose fapl))))

#+sbcl(sb-ext:quit)
