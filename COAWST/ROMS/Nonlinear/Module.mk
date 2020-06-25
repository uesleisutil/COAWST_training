<<<<<<< HEAD
# svn $Id: Module.mk 889 2018-02-10 03:32:52Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
# Copyright (c) 2002-2019 The ROMS/TOMS Group             Kate Hedstrom :::
=======
# svn $Id: Module.mk 995 2020-01-10 04:01:28Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
# Copyright (c) 2002-2020 The ROMS/TOMS Group             Kate Hedstrom :::
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

local_sub  := ROMS/Nonlinear

local_lib  := libNLM.a
local_src  := $(wildcard $(local_sub)/*.F)

$(eval $(call make-library,$(local_lib),$(local_src)))

$(eval $(compile-rules))
