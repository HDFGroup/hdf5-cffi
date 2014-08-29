;;;; h5z.lisp - See H5Zpublic.h

(in-package #:hdf5-cffi)

(defctype h5z-filter-t :int)

(defconstant +H5Z-FILTER-ERROR+       -1)
(defconstant +H5Z-FILTER-NONE+         0)
(defconstant +H5Z-FILTER-DEFLATE+      1)
(defconstant +H5Z-FILTER-SHUFFLE+      2)
(defconstant +H5Z-FILTER-FLETCHER32+   3)
(defconstant +H5Z-FILTER-SZIP+         4)
(defconstant +H5Z-FILTER-NBIT+         5)
(defconstant +H5Z-FILTER-SCALEOFFSET+  6)
(defconstant +H5Z-FILTER-RESERVED+   256)

(defconstant +H5Z-FILTER-MAX+      65535)

(defconstant +H5Z-FILTER-ALL+          0)
(defconstant +H5Z-MAX-NFILTERS+       32)

(defbitfield h5z-flags
  (:defmask   #x00FF)
  (:mandatory #x0000)
  (:optional  #x0001)
  (:invmask   #xFF00)
  (:reverse   #x0100)
  (:skip-edc  #x0200))

(defconstant +H5-SZIP-ALLOW-K13-OPTION-MASK+ 1)
(defconstant +H5-SZIP-CHIP-OPTION-MASK+      2)
(defconstant +H5-SZIP-EC-OPTION-MASK+        4)
(defconstant +H5-SZIP-NN-OPTION-MASK+       32)
(defconstant +H5-SZIP-MAX-PIXELS-PER-BLOCK+ 32)

(defconstant +H5Z-SHUFFLE-USER-NPARMS+  0)
(defconstant +H5Z-SHUFFLE-TOTAL-NPARMS+ 1)

(defconstant +H5Z-SZIP-USER-NPARMS+  2)
(defconstant +H5Z-SZIP-TOTAL-NPARMS+ 4)
(defconstant +H5Z-SZIP-PARM-MASK+    0)
(defconstant +H5Z-SZIP-PARM-PPB+     1)
(defconstant +H5Z-SZIP-PARM-BPP+     2)
(defconstant +H5Z-SZIP-PARM-PPS+     3)

(defconstant +H5Z-NBIT-USER-NPARMS+ 0)

(defconstant +H5Z-SCALEOFFSET-USER-NPARMS+ 2)

(defconstant +H5Z-SO-INT-MINBITS-DEFAULT+ 0)

(defcenum h5z-so-scale-type-t
    (:H5Z-SO-FLOAT-DSCALE 0)
    :H5Z-SO-FLOAT-ESCALE
    :H5Z-SO-INT)

(defconstant +H5Z-CLASS-T-VERS+ 1)

(defcenum h5z-edc-t
  (:H5Z_ERROR_EDC -1)
  :H5Z-DISABLE-EDC
  :H5Z-ENABLE-EDC
  :H5Z-NO-EDC)

(defconstant +H5Z-FILTER-CONFIG-ENCODE-ENABLED #x0001)
(defconstant +H5Z-FILTER-CONFIG-DECODE-ENABLED #x0002)
