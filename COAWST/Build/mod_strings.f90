      MODULE mod_strings
!
!svn $Id: mod_strings.F 914 2018-07-07 20:57:52Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  cdt         F90/F95 compiler used.                                  !
!  fflags      F90/F95 compiler flags.                                 !
!  title       Title of model run.                                     !
!  Coptions    Activated C-preprocessing options.                      !
!  Mregion     Start index of message passage code profiling region.   !
!  Nregion     Number of total code profiling regions.                 !
!                                                                      !
!  StateMsg    Model state messages when reading:                      !
!                ( 1) state initial conditions                         !
!                ( 2) previous state initial conditions                !
!                ( 3) previous adjoint state solution                  !
!                ( 4) latest adjoint state solution                    !
!                ( 5) surface forcion and or OBC increments            !
!                ( 6) tangent linear model error forcing               !
!                ( 7) impulse forcing                                  !
!                ( 8) v-space increments                               !
!                ( 9) background state                                 !
!                (10) IC correlation standard deviation                !
!                (11) model error correlation standard deviation       !
!                (12) OBC correlation standard deviation               !
!                (13) surface forcing correlation standard deviation   !
!                (14) IC normalization factors                         !
!                (15) model error normalization factors                !
!                (16) OBC normalization factors                        !
!                (17) surface forcing normalization factors            !
!                                                                      !
!  Pregion     Model regions identifiers used for time profiling:      !
!                ( 1) Allocation and array initialization              !
!                ( 2) Ocean state initialization                       !
!                ( 3) Reading of input data                            !
!                ( 4) Processing of input data                         !
!                ( 5) Processing of output time averaged data          !
!                ( 6) Computation of vertical boundary conditions      !
!                ( 7) Computation of global information integrals      !
!                ( 8) Writing of output data                           !
!                ( 9) Model 2D kernel                                  !
!                (10) Lagrangian floats trajectories                   !
!                (11) Tidal forcing                                    !
!                (12) 2D/3D coupling, vertical metrics                 !
!                (13) Omega vertical velocity                          !
!                (14) Equation of state for seawater                   !
!                (15) Biological module, source/sink terms             !
!                (16) Sediment transport module, source/sink terms     !
!                (17) Atmosphere-Ocean bulk flux parameterization      !
!                (18) KPP vertical mixing parameterization             !
!                (19) GLS vertical mixing parameterization             !
!                (20) My2.5 vertical mixing parameterization           !
!                (21) 3D equations right-side terms                    !
!                (22) 3D equations predictor step                      !
!                (23) Pressure gradient                                !
!                (24) Harmonic mixing of tracers, S-surfaces           !
!                (25) Harmonic mixing of tracers, geopotentials        !
!                (26) Harmonic mixing of tracers, isopycnals           !
!                (27) Biharmonic mixing of tracers, S-surfaces         !
!                (28) Biharmonic mixing of tracers, geopotentials      !
!                (29) Biharmonic mixing of tracers, isopycnals         !
!                (30) Harmonic stress tensor, S-surfaces               !
!                (31) Harmonic stress tensor, geopotentials            !
!                (32) Biharmonic stress tensor, S-surfaces             !
!                (33) Biharmonic stress tensor, geopotentials          !
!                (34) Corrector time-step for 3D momentum              !
!                (35) Corrector time-step for tracers                  !
!                (36) Nesting Algorithm                                !
!                (37) Bottom boundary layer module                     !
!                (38) GST Analysis eigenproblem solution               !
!                (39) Two-way coupling to Atmosphere Model             !
!                (40) Two-way coupling to Sea Ice Model                !
!                (41) Two-way coupling to Wave Model                   !
!                (42) Reading model state vector                       !
!                (43) 4D-Var minimization solver                       !
!                (44) Background error covariance matrix               !
!                (45) Posterior error covariance matrix                !
!                (46) Unused 01                                        !
!                (47) Unused 02                                        !
!                (48) Unused 03                                        !
!                (49) Unused 04                                        !
!                (50) Unused 05                                        !
!                (51) Unused 06                                        !
!                (52) Unused 07                                        !
!                (53) Unused 08                                        !
!                (54) Unused 09                                        !
!                (55) Unused 10                                        !
!                (56) Unused 11                                        !
!                (57) Unused 12                                        !
!                (58) Unused 13                                        !
!                (59) Unused 14                                        !
!                (60) Message Passage: 2D halo exchanges               !
!                (61) Message Passage: 3D halo exchanges               !
!                (62) Message Passage: 4D halo exchanges               !
!                (63) Message Passage: lateral boundary exchanges      !
!                (64) Message Passage: data broadcast                  !
!                (65) Message Passage: data reduction                  !
!                (66) Message Passage: data gathering                  !
!                (67) Message Passage: data scattering                 !
!                (68) Message Passage: boundary data gathering         !
!                (69) Message Passage: point data gathering            !
!                (70) Message Passage: nesting point data gathering    !
!                (71) Message Passage: nesting array data gathering    !
!                (72) Message Passage: synchronization barrier         !
!                (73) Message Passage: multi-model coupling            !
!                                                                      !
!=======================================================================
!
        implicit none
        character (len=80)   :: title
        character (len=2048) :: Coptions
        integer, parameter :: Mregion = 60
        integer, parameter :: Nregion = 73
        character (len=48), dimension(17) :: StateMsg =                 &
     &    (/'state initial conditions,                       ',         & !01
     &      'previous state initial conditions,              ',         & !02
     &      'previous adjoint state solution,                ',         & !03
     &      'latest adjoint state solution,                  ',         & !04
     &      'surface forcing and or OBC increments,          ',         & !05
     &      'tangent linear model error forcing,             ',         & !06
     &      'impulse forcing,                                ',         & !07
     &      'v-space increments,                             ',         & !08
     &      'background state,                               ',         & !09
     &      'IC correlation standard deviation,              ',         & !10
     &      'model error correlation standard deviation,     ',         & !11
     &      'OBC correlation standard deviation,             ',         & !12
     &      'surface forcing correlation standard deviation, ',         & !13
     &      'IC normalization factors,                       ',         & !14
     &      'model error normalization factors,              ',         & !15
     &      'OBC normalization factors,                      ',         & !16
     &      'surface forcing normalization factors,          '/)          !17
        character (len=50), dimension(Nregion) :: Pregion =             &
     &    (/'Allocation and array initialization ..............',       & !01
     &      'Ocean state initialization .......................',       & !02
     &      'Reading of input data ............................',       & !03
     &      'Processing of input data .........................',       & !04
     &      'Processing of output time averaged data ..........',       & !05
     &      'Computation of vertical boundary conditions ......',       & !06
     &      'Computation of global information integrals ......',       & !07
     &      'Writing of output data ...........................',       & !08
     &      'Model 2D kernel ..................................',       & !09
     &      'Lagrangian floats trajectories ...................',       & !10
     &      'Tidal forcing ....................................',       & !11
     &      '2D/3D coupling, vertical metrics .................',       & !12
     &      'Omega vertical velocity ..........................',       & !13
     &      'Equation of state for seawater ...................',       & !14
     &      'Biological module, source/sink terms .............',       & !15
     &      'Sediment transport module, source/sink terms .....',       & !16
     &      'Atmosphere-Ocean bulk flux parameterization ......',       & !17
     &      'KPP vertical mixing parameterization .............',       & !18
     &      'GLS vertical mixing parameterization .............',       & !19
     &      'My2.5 vertical mixing parameterization ...........',       & !20
     &      '3D equations right-side terms ....................',       & !21
     &      '3D equations predictor step ......................',       & !22
     &      'Pressure gradient ................................',       & !23
     &      'Harmonic mixing of tracers, S-surfaces ...........',       & !24
     &      'Harmonic mixing of tracers, geopotentials ........',       & !25
     &      'Harmonic mixing of tracers, isopycnals ...........',       & !26
     &      'Biharmonic mixing of tracers, S-surfaces .........',       & !27
     &      'Biharmonic mixing of tracers, geopotentials ......',       & !28
     &      'Biharmonic mixing of tracers, isopycnals .........',       & !29
     &      'Harmonic stress tensor, S-surfaces ...............',       & !30
     &      'Harmonic stress tensor, geopotentials ............',       & !31
     &      'Biharmonic stress tensor, S-surfaces .............',       & !32
     &      'Biharmonic stress tensor, geopotentials ..........',       & !33
     &      'Corrector time-step for 3D momentum ..............',       & !34
     &      'Corrector time-step for tracers ..................',       & !35
     &      'Nesting algorithm ................................',       & !36
     &      'Bottom boundary layer module .....................',       & !37
     &      'GST Analysis eigenproblem solution ...............',       & !38
     &      'Two-way coupling to Atmosphere Model .............',       & !39
     &      'Two-way coupling to Sea Ice Model ................',       & !40
     &      'Two-way coupling to Wave Model ...................',       & !41
     &      'Reading model state vector .......................',       & !42
     &      '4D-Var minimization solver .......................',       & !43
     &      'Background error covariance matrix ...............',       & !44
     &      'Posterior error covariance matrix ................',       & !45
     &      'Unused 01 ........................................',       & !46
     &      'Unused 02 ........................................',       & !47
     &      'Unused 03 ........................................',       & !48
     &      'Unused 04 ........................................',       & !49
     &      'Unused 05 ........................................',       & !50
     &      'Unused 06 ........................................',       & !51
     &      'Unused 07 ........................................',       & !52
     &      'Unused 08 ........................................',       & !53
     &      'Unused 09 ........................................',       & !54
     &      'Unused 10 ........................................',       & !55
     &      'Unused 11 ........................................',       & !56
     &      'Unused 12 ........................................',       & !57
     &      'Unused 13 ........................................',       & !58
     &      'Unused 14 ........................................',       & !59
     &      'Message Passage: 2D halo exchanges ...............',       & !60
     &      'Message Passage: 3D halo exchanges ...............',       & !61
     &      'Message Passage: 4D halo exchanges ...............',       & !62
     &      'Message Passage: lateral boundary exchanges ......',       & !63
     &      'Message Passage: data broadcast ..................',       & !64
     &      'Message Passage: data reduction ..................',       & !65
     &      'Message Passage: data gathering ..................',       & !66
     &      'Message Passage: data scattering..................',       & !67
     &      'Message Passage: boundary data gathering .........',       & !68
     &      'Message Passage: point data gathering ............',       & !69
     &      'Message Passage: nesting point data gathering ....',       & !70
     &      'Message Passage: nesting array data gathering ....',       & !71
     &      'Message Passage: synchronization barrier .........',       & !72
     &      'Message Passage: multi-model coupling ............'/)        !73
!
!  The following variables are assigned during C-preprocessing.  They
!  have information about operating sytem, CPU hardware, compiling
!  system, compiler command, compiler flags, analytical directory,
!  header directory, application header file, and root directory.
!
        character (len=80)  :: my_os = "Linux"
        character (len=80)  :: my_cpu = "x86_64"
        character (len=80)  :: my_fort = "gfortran"
        character (len=80)  :: my_fc = "/usr/local/bin/mpif90"
        character (len=512) :: my_fflags = "-frepack-arrays -O3 -ffast-math -O3 -ftree-vectorize -ftree-loop-linear -funroll-loops -w -ffree-form -ffree-line-length-none -frecord-marker=4 -fconvert=big-endian -I/usr/local/include -I/usr/local/include -I/home/ueslei/Documents/INPE/COAWST_training/COAWST/WRF/main -I/home/ueslei/Documents/INPE/COAWST_training/COAWST/WRF/external/esmf_time_f90 -I/home/ueslei/Documents/INPE/COAWST_training/COAWST/WRF/frame -I/home/ueslei/Documents/INPE/COAWST_training/COAWST/WRF/share -ffree-form -ffree-line-length-none -ffree-form -ffree-line-length-none"
!
        character (len=256) :: Adir = "/home/ueslei/Documents/INPE/COAWST_training/COAWST/Projects/Sandy"
        character (len=256) :: Hdir = "/home/ueslei/Documents/INPE/COAWST_training/COAWST/Projects/Sandy"
        character (len=256) :: Hfile = "sandy.h"
        character (len=256) :: Rdir = "/home/ueslei/Documents/INPE/COAWST_training/COAWST"
!
      END MODULE mod_strings
