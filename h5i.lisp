;;;; See H5Ipublic.h .

(in-package #:hdf5-cffi)

;;; This will change in HDF5 1.10!

(defctype hid-t :int)

(defconstant +H5I-INVALID-HID+ -1)
