/*
<<<<<<< HEAD
** svn $Id: coupling_test.h 889 2018-02-10 03:32:52Z arango $
*******************************************************************************
** Copyright (c) 2002-2019 The ROMS/TOMS Group                               **
=======
** svn $Id: coupling_test.h 1001 2020-01-10 22:41:16Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Atmosphere-Ocean Two-Way Coupling Test (WRF/ROMS).
**
** Application flag:   COUPLING_TEST
** Input script:       roms_coupling_test.in
*/

#define UV_ADV
#define UV_COR
#define UV_QDRAG
#undef  UV_VIS2
#define SPLINES_VDIFF
#define SPLINES_VVISC
<<<<<<< HEAD
#define TS_U3HADVECTION
#define TS_C4VADVECTION
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#define DJ_GRADPS
#undef  TS_DIF2
#undef  MIX_GEO_TS
#define SALINITY
#define SOLVE3D
#define AVERAGES
#define ANA_GRID
#define ANA_INITIAL
#define ANA_STFLUX
#define ANA_SSFLUX
#define ANA_BTFLUX
#define ANA_BSFLUX
#define ANA_VMIX
