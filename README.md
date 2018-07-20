# ciftbx
##### The original Crystallographic Information File Toolbox with minor added functionality.

This version of the ciftbx package was originally copied as the ciftbx.cshar.Z archive from the following web site:
[http://www.bernstein-plus-sons.com/software/ciftbx/](http://www.bernstein-plus-sons.com/software/ciftbx)

It was uploaded to this GitHub repository so that users of the iopen source [EMsoft](http://github.com/marcdegraef/EMsoft) package can build one additional program that makes use of the ciftbx library.  The program, written in fortran 90, extracts a number of data fields from a CIF file and places them in a new text file in a particular order and format; this text file can then be re-directed to EMsoft's EMmkxtal program to generate an HDF5 file that can be read by other EMsoft programs. The new program is an optional program and is not required for EMsoft, hence we keep it completely separate as part of this ciftbx repository. The original programs that make up the ciftbx archive have not been altered and can be compiled and used as before.

The package leaves the original ciftbx.cshar.Z archive completely unchanged, and adds to it the following files:
- this README.md file
- a new Makefile called Makefile2 that allows the user to compile the CIFextract.f90 program
- the CIFextract.f90 program itself
- fortran 90 versions of the original function and variable definition files ciftbx.cmf and ciftbx.cmv, called ciftbx90.cmf and ciftbx90.cmv, respectively
- a text file, instructions.md, with a brief explanation of the new program

The new files are also covered under the original GPL license, and not under EMsoft's BSD-2 license.

### Compilation

In the ciftbx.src folder there is a Makefile2 file that contains all the instructions for building the CIFextract program; simply use the following command on the UNIX command line:
```fortran
make -f Makefile2
```
and this will build the necessary object files and the CIFextract program.

Note that execution of the command *make all* will build the object files and the original set of test programs **without** building the new CIFextract program.