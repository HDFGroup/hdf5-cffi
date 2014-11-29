hdf5-cffi
=========

`hdf5-cffi` is a [CFFI](http://common-lisp.net/project/cffi/) wrapper for the HDF5 library. It does **not** provide a LISPy (= pretty) interface to HDF5. All it lets you do is to use HDF5 from Common LISP as you would from C. Not a pleasant sight, but hey, LISP is the *programmable programming language* (John Foderaro) and your imagination is the limit. If you'd like to see Common LISP and HDF5 in action, have a look at Gary Hollis' [cl-ana](https://github.com/ghollisjr/cl-ana) package, which was also the main inspiration for this package.

Bindings for FORTRAN, arguably the oldest high-level programming language, were introduced in HDF5 1.4.0, which was released about 13 years ago. LISP, "the greatest single programming language ever designed" (Alan Kay), has not gotten the attention it deserves.  This is a first step towards rectifying the situation.

## Installation

Install `hdf5-cffi` via [Quicklisp](http://www.quicklisp.org/). The installation might fail because the installer doesn't
find the HDF5 header files (`hdf5.h` & Co.) or the shared library (`libhdf5.so`). The former are necessary for the CFFI groveller and, yes, you also need a C compiler. If HDF5 is installed in a "standard" location such as `/usr/local`
you should be fine. Otherwise, you might have to adjust your `CC` and `LD_LIBRARY_PATH` environment variables, e.g.,
```
export CC=~/bin/h5cc
export LD_LIBRARY_PATH=~/lib:$LD_LIBRARY_PATH
```
Check out the examples in the `examples` directory. For example, to run `examples/datasets/h5ex-d-checksum.lisp` with SBCL,
open a shell and run `sbcl --load examples/datasets/h5ex-d-checksum.lisp`. The output should look like this:
```
This is SBCL 1.2.5, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
Filter type is: H5Z_FILTER_FLETCHER32
Maximum value in DS1 is: 1890
```
Enjoy!

## References

1. [Garry Hollis' cl-ana](https://github.com/ghollisjr/cl-ana)
2. [Daniel Herring's dh-misc/hdf5](https://gitorious.org/dh-misc/hdf5)
