
(in-package #:hdf5)

(defparameter *test-failures* nil)

(defun testhdf5 ()
    (load-hdf5-foreign-libraries)
    (do-external-symbols (s (find-package "HDF5"))
      (print s))

    ;; basics
    (load "examples/basics/h5-cmprss")
    (load "examples/basics/h5-crtdat")
    (load "examples/basics/h5-crtatt")
    (load "examples/basics/h5-crtgrp")
    (load "examples/basics/h5-crtgrpar")
    (load "examples/basics/h5-crtgrpd")
    (load "examples/basics/h5-extend")
    (load "examples/basics/h5-rdwt")
    (load "examples/basics/h5-subset")

    ;; datasets
    (load "examples/datasets/h5ex-d-alloc")
    (load "examples/datasets/h5ex-d-checksum")
    (load "examples/datasets/h5ex-d-chunk")
    (load "examples/datasets/h5ex-d-compact")
    (load "examples/datasets/h5ex-d-extern")
    (load "examples/datasets/h5ex-d-fillval")
    (load "examples/datasets/h5ex-d-gzip")
    (load "examples/datasets/h5ex-d-hyper")
    (load "examples/datasets/h5ex-d-nbit")
    (load "examples/datasets/h5ex-d-rdwr")
    (load "examples/datasets/h5ex-d-shuffle")
    (load "examples/datasets/h5ex-d-sofloat")
    (load "examples/datasets/h5ex-d-soint")
    (load "examples/datasets/h5ex-d-szip")
    (load "examples/datasets/h5ex-d-transform")
    (load "examples/datasets/h5ex-d-unlimadd")
    (load "examples/datasets/h5ex-d-unlimgzip")
    (load "examples/datasets/h5ex-d-unlimmod")

    ;; groups
    (load "examples/groups/h5ex-g-compact")
    (load "examples/groups/h5ex-g-corder")
    (load "examples/groups/h5ex-g-create")
    (load "examples/groups/h5ex-g-intermediate")
    (load "examples/groups/h5ex-g-iterate")
    (load "examples/groups/h5ex-g-phase")
    (load "examples/groups/h5ex-g-traverse")
    (load "examples/groups/h5ex-g-visit"))

(eval-when (:load-toplevel :execute)
  (testhdf5)
  #+sbcl(sb-ext:exit))
