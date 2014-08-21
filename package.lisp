;;;; package.lisp

(defpackage #:hdf5-cffi
  (:use #:cl #:cffi)
  (:export
   
   ;; h5
   
   :herr-t
   :hbool-t
   :htri-t
   :size-t
   :ssize-t
   :hsize-t
   :hssize-t
   :haddr-t
   :+HADDR-UNDEF+
   :+HADDR-MAX+
   :+H5P-DEFAULT+

   ;; h5i

   :hid-t
   :+H5I-INVALID-HID+))

