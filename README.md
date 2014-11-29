hdf5-cffi
=========

`hdf5-cffi` is a [CFFI](http://common-lisp.net/project/cffi/) wrapper for the HDF5 library. It does **not** provide a LISPy (= pretty) interface to HDF5. All it lets you do is to use HDF5 from Common LISP as you would from C. Not a pleasant sight, but hey, LISP is the *programmable programming language* (John Foderaro) and your imagination is the limit. If you'd like to see Common LISP and HDF5 in action, have a look at Gary Hollis' [cl-ana](https://github.com/ghollisjr/cl-ana) package, which was also the main inspiration for this package.

Bindings for FORTRAN, arguably the oldest high-level programming language, were introduced in HDF5 1.4.0, which was released about 13 years ago. LISP, "the greatest single programming language ever designed" (Alan Kay), has not gotten the attention it deserves.  This is a first step towards rectifying the situation.

## References

1. [Garry Hollis' cl-ana](https://github.com/ghollisjr/cl-ana)
2. [Daniel Herring's dh-misc/hdf5](https://gitorious.org/dh-misc/hdf5.git)
