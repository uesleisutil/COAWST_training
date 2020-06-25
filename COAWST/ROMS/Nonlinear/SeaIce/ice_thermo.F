#include "cppdefs.h"
      MODULE ice_thermo_mod
#if defined ICE_MODEL && defined ICE_THERMO
!
!=======================================================================
<<<<<<< HEAD
!  Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
=======
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!================================================== Hernan G. Arango ===
!                                                                      !
!  This routine computes the ice thermodynamic growth and decay        !
!  term. Currently the only option is the Mellor-Kantha formulation.   !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE

# ifdef ICE_MK
      PUBLIC ice_thermo
      CONTAINS
#  include "ice_mk.h"
# endif

#endif

      END MODULE ice_thermo_mod
