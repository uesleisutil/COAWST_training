# svn $Id$
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
<<<<<<< HEAD
# Copyright (c) 2002-2016 The ROMS/TOMS Group             Kate Hedstrom :::
=======
# Copyright (c) 2002-2019 The ROMS/TOMS Group             Kate Hedstrom :::
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

local_sub  := SeaIce/Extra

local_lib  := libExtra.a
local_src  := $(wildcard $(local_sub)/*.F)

$(eval $(call make-library,$(local_lib),$(local_src)))

$(eval $(compile-rules))
