
(in-package :hdf5)

(defparameter +NULL+ (cffi:null-pointer))

(define-foreign-library hdf5
  (:darwin (:or (:framework "hdf5") "libhdf5.dylib")) ;; ?
  (:windows "libhdf5.dll" :convention :stdcall)
  (:unix (:or "libhdf5.so"
              "libhdf5_serial.so"
              "libhdf5_serial.so.100"
              "libhdf5_serial.so.100.0.1")))

(use-foreign-library hdf5)

