############################################################################
#
#  Program:         ARPACK
#
#  Module:          ARmake.inc
#
#  Purpose:         Top-level Definitions
#
#  Creation date:   February 22, 1996
#
#  Modified:
#
#  Send bug reports, comments or suggestions to arpack@caam.rice.edu
#
############################################################################
#
# %---------------------------------%
# |  SECTION 1: PATHS AND LIBRARIES |
# %---------------------------------%
#
# %---------------------------------------------------------%
<<<<<<< HEAD
=======
# | If you are using a VENDOR SUPPLIED VERSION of MPI, you  |
# | must replace the mpif.h in the following directories    |
# |                                                         |
# |            ARPACK/PARPACK/SRC/MPI/mpif.h                |
# |            ARPACK/PARPACK/UTIL/MPI/mpif.h               |
# |            ARPACK/PARPACK/EXAMPLES/MPI/mpif.h           |
# |                                                         |
# | with the one for the native implementation.             |
# %---------------------------------------------------------%
#
# %---------------------------------------------------------%
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
# | The complete path to the top level of ARPACK directory, |
# | usually $(HOME)/ARPACK                                  |
# %---------------------------------------------------------%
#
home         = $(ROMSHOME)/Lib/ARPACK
#
<<<<<<< HEAD
#  %--------------------------------------%
#  | The platform identifier to suffix to |
#  | the end of library names             |
#  %--------------------------------------%
#
#PLAT        = dbg_daggoo
#PLAT        = daggoo
#PLAT        = dbg_moby
 PLAT        = moby
#
#  %------------------------------------------------------%
#  | The directories to find the various pieces of ARPACK |
#  %------------------------------------------------------%
=======
#
# %---------------------------------------------------------%
# | The platform identifier to suffix to the end of library |
# | names. Don't forget to include the underscore!          |
# |                                                         |
# |    PLAT = _myhost                                       |
# %---------------------------------------------------------%
#
PLAT          =
#
# %---------------------------------------------------------%
# | The communication library (for parallel library only)   |
# | Current availible platforms are: BLACS or MPI           |
# %---------------------------------------------------------%
#
COMMLIB       = MPI
#
#  %--------------------------------------------------------%
#  | The directories to find the various pieces of ARPACK   |
#  %--------------------------------------------------------%
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#
BLASdir      = $(home)/BLAS
LAPACKdir    = $(home)/LAPACK
UTILdir      = $(home)/UTIL
SRCdir       = $(home)/SRC
<<<<<<< HEAD
#
#
DIRS   = $(BLASdir) $(LAPACKdir) $(UTILdir) $(SRCdir)
#
# %-------------------------------------------------------------------%
# | Comment out the previous line and uncomment the following         |
# | if you already have the BLAS and LAPACK installed on your system. |
# | NOTE: ARPACK assumes the use of LAPACK version 2 codes.           |
# %-------------------------------------------------------------------%
# 
#DIRS   = $(UTILdir) $(SRCdir)
#
#
# %---------------------------------------------------%
# | The name of the libraries to be created/linked to |
# %---------------------------------------------------%
#
ARPACKLIB  = $(home)/libarpack_$(PLAT).a
LAPACKLIB = 
BLASLIB = 

ALIBS =  $(ARPACKLIB) $(LAPACKLIB) $(BLASLIB) 

=======
PSRCdir      = $(home)/PARPACK/SRC/$(COMMLIB)
PUTILdir     = $(home)/PARPACK/UTIL/$(COMMLIB)
#
DIRS   = $(BLASdir) $(LAPACKdir) $(UTILdir) $(SRCdir)
#
# %---------------------------------------------------------%
# | Comment out the previous line and uncomment the         |
# | following if you already have BLAS and LAPACK installed |
# | on your system.                                         |
# | NOTE: ARPACK assumes the use of LAPACK version 2 codes. |
# %---------------------------------------------------------%
#
#DIRS   = $(UTILdir) $(SRCdir)
#
# %---------------------------------------------------------%
# | The name of the libraries to be created/linked to.      |
# | If you set PLAT above make sure to include it in the    |
# | library name(s):                                        |
# |                                                         |
# |    ARPACKLIB  = $(home)/libarpack$(PLAT).a              |
# |    PARPACKLIB = $(home)/libparpack$(PLAT).a             |
# %---------------------------------------------------------%
#
ARPACKLIB  = $(home)/libarpack.a
PARPACKLIB = $(home)/libparpack.a
LAPACKLIB =
BLASLIB =

ALIBS =  $(ARPACKLIB) $(LAPACKLIB) $(BLASLIB)
#
# %---------------------------------------------------------%
# | If your system users custom MPI libraries specify them  |
# | here for linking with Parallel ARPACK.                  |
# |                                                         |
# |        MPILIBS = -lfmpi -lmpi                           |
# |                                                         |
# | If your system uses mpif90 or other wrapper scripts you |
# | should leave this blank.                                |
# %---------------------------------------------------------%
#
MPILIBS =

PLIBS = $(PARPACKLIB) $(ALIBS) $(MPILIBS)
#
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
# %---------------------------------------------------------%
# |                  SECTION 2: COMPILERS                   |
# |                                                         |
# | The following macros specify compilers, linker/loaders, |
# | the archiver, and their options.  You need to make sure |
# | these are correct for your system.                      |
# %---------------------------------------------------------%
#
#
# %------------------------------%
# | Make our own suffixes' list. |
# %------------------------------%
#
.SUFFIXES:
.SUFFIXES:	.f	.o
#
# %------------------%
# | Default command. |
# %------------------%
#
.DEFAULT:
	@$(ECHO) "Unknown target $@, try:  make help"

#
# %-------------------------------------------%
# |  Command to build .o files from .f files. |
# %-------------------------------------------%
#
.f.o:
	@$(ECHO) Making $@ from $<
	@$(FC) -c $(FFLAGS) $<

#
<<<<<<< HEAD
# %-----------------------------------------%
# | Various compilation programs and flags. |
# | You need to make sure these are correct |
# | for your system.                        |
# %-----------------------------------------%
#

FC	= pgf90
#FFLAGS	= -g
FFLAGS	= -u -Bstatic -fastsse -Mipa=fast

LDFLAGS = 
=======
# %---------------------------------------------------------%
# | Various compilation programs and flags.  You need to    |
# | make sure these are correct for your system.            |
# %---------------------------------------------------------%
#

FC	= ifort
FFLAGS	= -ip -O3 -fp-model precise

# %-----------------------------------%
# | Set the parallel fortran compiler |
# %-----------------------------------%

PFC     = mpif90
PFFLAGS = -ip -O3 -fp-model precise

LDFLAGS =
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
CD	= cd

ECHO	= echo

LN	= ln
LNFLAGS	= -s

MAKE	= make

RM	= rm
RMFLAGS	= -f

SHELL	= /bin/sh
#
<<<<<<< HEAD
#  %----------------------------------------------------------------%
#  | The archiver and the flag(s) to use when building an archive   |
#  | (library).  Also the ranlib routine.  If your system has no    |
#  | ranlib, set RANLIB = touch.                                    |
#  %----------------------------------------------------------------%
=======
#  %--------------------------------------------------------%
#  | The archiver and the flag(s) to use when building an   |
#  | archive (library).  Also the ranlib routine.  If your  |
#  | system has no ranlib, set RANLIB = touch.              |
#  %--------------------------------------------------------%
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#
AR = ar
ARFLAGS = rv
#RANLIB  = touch
RANLIB   = ranlib
#
# %----------------------------------%
# | This is the general help target. |
# %----------------------------------%
#
help:
	@$(ECHO) "usage: make ?"
