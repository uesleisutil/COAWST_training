/*
<<<<<<< HEAD
** svn $Id: estuary_test.h 889 2018-02-10 03:32:52Z arango $
*******************************************************************************
** Copyright (c) 2002-2019 The ROMS/TOMS Group                               **
=======
** svn $Id: estuary_test.h 1001 2020-01-10 22:41:16Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Estuary with Sediment Transport Test.
**
** Application flag:   ESTUARY_TEST
** Input script:       roms_estuary_test.in
**                     sediment_estuary_test.in
*/

#define ROMS_MODEL
#define UV_ADV
#define UV_LOGDRAG
#define SPLINES_VDIFF
#define SPLINES_VVISC
<<<<<<< HEAD
#define TS_U3HADVECTION
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#define SALINITY
#define SOLVE3D
#define SEDIMENT
#ifdef SEDIMENT
# define SUSPLOAD
#endif
#define AVERAGES
#define GLS_MIXING
#undef  MY25_MIXING
#if defined GLS_MIXING || defined MY25_MIXING
# define KANTHA_CLAYSON
# undef  CANUTO_A
# define N2S2_HORAVG
# define RI_SPLINES
#endif
#define ANA_GRID
#define ANA_INITIAL
#define ANA_SEDIMENT
#define ANA_SMFLUX
#define ANA_STFLUX
#define ANA_BTFLUX
#define ANA_SSFLUX
#define ANA_BSFLUX
#define ANA_SPFLUX
#define ANA_BPFLUX
#define ANA_FSOBC
#define ANA_M2OBC
#define ANA_TOBC
