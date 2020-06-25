#include "cppdefs.h"
      MODULE mod_sedbed

/*
<<<<<<< HEAD
** svn $Id: mod_sedbed.F 889 2018-02-10 03:32:52Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2019 The ROMS/TOMS Group                        **
=======
** svn $Id: mod_sedbed.F 995 2020-01-10 04:01:28Z arango $
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
** This module declares  Sediment Model Kernel Structure  containing  **
** several variables describing bed layers, stratigraphy, and bottom  **
** properties.                                                        **
**                                                                    **
** Note that the  *.h file is located in ROMS/Nonlinear/Sediment and  **
** it is included within <...> to allow the user customize it in the  **
** project directory, while keeping the distributed file intact       **
** (check build scripts for details).                                 **
**                                                                    **
************************************************************************
*/

#if defined SEDIMENT || defined BBL_MODEL
# include <sedbed_mod.h>
#endif

      END MODULE mod_sedbed
