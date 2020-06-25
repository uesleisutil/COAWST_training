#!/bin/csh -f
#
<<<<<<< HEAD
# svn $Id: job_op.sh 889 2018-02-10 03:32:52Z arango $
#######################################################################
# Copyright (c) 2002-2019 The ROMS/TOMS Group                         #
=======
# svn $Id: job_op.sh 995 2020-01-10 04:01:28Z arango $
#######################################################################
# Copyright (c) 2002-2020 The ROMS/TOMS Group                         #
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#   Licensed under a MIT/X style license                              #
#   See License_ROMS.txt                                              #
#######################################################################
#                                                                     #
#  Generalized Stability Theory: Optimal Perturbations                #
#                                                                     #
#  This script is used to run the ROMS/TOMS optimal perturbations     #
#  algorithm.                                                         #
#                                                                     #
#######################################################################

# Set ROOT of the directory to run application.  The following
# "dirname" command returns a path by removing any suffix from
# the last slash ('/').  It returns a path above current diretory.

set Dir=`dirname ${PWD}`

# Set basic state trajectory, forward file:

#set HISname=${Dir}/Forward/gyre3d_his_00.nc
 set HISname=${Dir}/Forward/gyre3d_his_01.nc

set FWDname=gyre3d_fwd.nc

if (-e $FWDname) then
  /bin/rm $FWDname
endif
ln -s -v $HISname $FWDname

# Set tangent linear model initial conditions file: zero fields.

set ITLname=gyre3d_itl.nc
if (-e $ITLname) then
  /bin/rm $ITLname
endif
ln -s -v ${Dir}/Data/gyre3d_ini_zero.nc $ITLname
