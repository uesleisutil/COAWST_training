/*
<<<<<<< HEAD
** svn $Id: natl.h 889 2018-02-10 03:32:52Z arango $
*******************************************************************************
** Copyright (c) 2002-2019 The ROMS/TOMS Group                               **
=======
** svn $Id: natl.h 995 2020-01-10 04:01:28Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for High Resolution North Atlantic Application.
**
** Application flag:   NATL
** Input script:       ocean_natl.in
*/

#define ROMS_MODEL
#define UV_ADV
#define UV_QDRAG
#define UV_COR
#define DJ_GRADPS
#define SPLINES_VDIFF
#define SPLINES_VVISC
#define TS_U3HADVECTION
#define TS_C4VADVECTION
#define SOLVE3D
#define SALINITY
#define NONLIN_EOS
#define CURVGRID
#define STATIONS
#define MASKING
#define WRITE_WATER
#define NO_WRITE_GRID
#define AVERAGES
#define SRELAXATION
#define QCORRECTION
#define SOLAR_SOURCE
#define ANA_BSFLUX
#define ANA_BTFLUX
#define LMD_MIXING
#ifdef LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_SKPP
# define LMD_NONLOCAL
# define RI_SPLINES
#endif
