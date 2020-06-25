/*
<<<<<<< HEAD
** svn $Id: test_chan.h 889 2018-02-10 03:32:52Z arango $
*******************************************************************************
** Copyright (c) 2002-2019 The ROMS/TOMS Group                               **
=======
** svn $Id: test_chan.h 1001 2020-01-10 22:41:16Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Sediment Test Channel Case.
**
** Application flag:   TEST_CHAN
** Input scripts:      roms_test_chan.in
**                     sediment_test_chan.in
*/

#define ROMS_MODEL
#define WRITE_GRID
#define OUT_DOUBLE
#define ANA_GRID
#define UV_ADV
#undef  SALINITY
#define SOLVE3D
#define SPLINES_VDIFF
#define SPLINES_VVISC
#define ANA_INITIAL
#define ANA_SMFLUX
#define ANA_STFLUX
#define ANA_BTFLUX
#define ANA_SSFLUX
#define ANA_BSFLUX
#define ANA_SPFLUX
#define ANA_BPFLUX
<<<<<<< HEAD
#define TS_U3HADVECTION
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#define ANA_FSOBC
#define ANA_M2OBC
#define SEDIMENT
#define ANA_SEDIMENT
#define SUSPLOAD
#undef  ANA_VMIX
#define GLS_MIXING
#define KANTHA_CLAYSON
#define N2S2_HORAVG
#define RI_SPLINES
#define UV_LOGDRAG
#undef  PERFECT_RESTART
