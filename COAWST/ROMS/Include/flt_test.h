/*
<<<<<<< HEAD
** svn $Id: flt_test.h 889 2018-02-10 03:32:52Z arango $
*******************************************************************************
** Copyright (c) 2002-2019 The ROMS/TOMS Group                               **
=======
** svn $Id: flt_test.h 1001 2020-01-10 22:41:16Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Floats Tracking Test.
**
** Application flag:   FLT_TEST
** Input script:       roms_flt_test2d.in,   roms_flt_test3d.in
**                     floats_flt_test2d.in, floats_flt_test3d.in
*/

#define ROMS_MODEL
#define UV_ADV
#define UV_QDRAG
#define UV_VIS2
#define MIX_S_UV
#define FLOATS
#define MASKING
#define ANA_GRID
#define ANA_INITIAL
#define ANA_SMFLUX

#ifdef SOLVE3D
# define DJ_GRADPS
# define SPLINES_VDIFF
# define SPLINES_VVISC
<<<<<<< HEAD
# define TS_A4HADVECTION
# define TS_A4VADVECTION
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
# define BODYFORCE
# define ANA_BTFLUX
# define ANA_STFLUX
#endif

