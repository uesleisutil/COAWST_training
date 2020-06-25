#include "cppdefs.h"
#ifdef SEDIMENT
!!
<<<<<<< HEAD
!!svn $Id: read_sedpar.F 889 2018-02-10 03:32:52Z arango $
!!================================================= Hernan G. Arango ===
!! Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
=======
!!svn $Id: read_sedpar.F 995 2020-01-10 04:01:28Z arango $
!!================================================= Hernan G. Arango ===
!! Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!======================================================================
!!                                                                     !
!!  This routine reads and reports sediment model input parameters.    !
!!                                                                     !
!!======================================================================
!!
# include <sediment_inp.h>
#else
      SUBROUTINE read_SedPar
      END SUBROUTINE read_SedPar
#endif
