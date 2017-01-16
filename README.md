hdf5-cffi
=========

`hdf5-cffi` is a [CFFI](http://common-lisp.net/project/cffi/) wrapper for the HDF5 library. It does **not** provide a LISPy (= pretty) interface to HDF5. All it lets you do is to use HDF5 from Common LISP as you would from C. Not a pleasant sight, but hey, LISP is the *programmable programming language* (John Foderaro) and your imagination is the limit. If you'd like to see Common LISP and HDF5 in action, have a look at Gary Hollis' [cl-ana](https://github.com/ghollisjr/cl-ana) package, which was also the main inspiration for this package.

Bindings for FORTRAN, arguably the oldest high-level programming language, were introduced in HDF5 1.4.0, which was released about 13 years ago. LISP, "the greatest single programming language ever designed" (Alan Kay), has not gotten the attention it deserves.  This is a first step towards rectifying the situation.

## Installation

There many different ways to install `hdf5-cffi`. Here's the maybe least painful way to get you started.

### Prerequisites

0. A Common Lisp compiler such as SBCL or CCL
1. [Quicklisp](http://www.quicklisp.org/)
2. An installation of HDF5 that includes:
   - The header files
   - A shared HDF5 library (`libhdf5.so` or `libhdf5.dylib`)
   - The `h5cc` compilation script (part of the standard installation)
   - The shared Szip compression library (`libsz.so` or `libsz.dylib`)
3. Make sure that the directories containing the shared libraries are in your `LD_LIBRARY_PATH`

### Build

1. Clone the `hdf5-cffi` repo into the `local-projects` subdirectory of your Quicklisp installation.
   (Alternatively, create a symbolic link called `hdf5-cffi` in that directory.)
2. In the `hdf5-cffi` top-level directory:
   - Edit the `Makefile` and set the `CL` variable for your Lisp compiler
   - `make hdf5-cffi`
3. If the build was successful run `make test`.

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
