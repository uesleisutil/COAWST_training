      SUBROUTINE checkdefs
!
!svn $Id: checkdefs.F 927 2018-10-16 03:51:56Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine checks activated C-preprocessing options for        !
!  consistency.                                                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_scalars
      USE mod_strings
!
      USE strings_mod, ONLY : uppercase
!
      implicit none
!
!  Local variable declarations.
!
      integer :: iatms = 0
      integer :: ibbl = 0
      integer :: ibiology = 0
      integer :: idriver = 0
      integer :: itrcHadv = 0
      integer :: itrcVadv = 0
      integer :: itrcHadvtl = 0
      integer :: itrcVadvtl = 0
      integer :: ivelHadv = 0
      integer :: ivelVadv = 0
      integer :: ivmix = 0
      integer :: nearshore = 0
      integer :: is, lstr, ng
!
!-----------------------------------------------------------------------
!  Report activated C-preprocessing options.
!-----------------------------------------------------------------------
!
      Coptions=' '
      IF (Master) WRITE (stdout,10)
  10  FORMAT (/,' Activated C-preprocessing Options:',/)
  20  FORMAT (1x,a,t26,a)
!
      IF (Master) THEN
        WRITE (stdout,20) TRIM(ADJUSTL(MyAppCPP)), TRIM(ADJUSTL(title))
      END IF
      is=LEN_TRIM(Coptions)+1
      lstr=LEN_TRIM(MyAppCPP)
      Coptions(is:is+lstr)=TRIM(ADJUSTL(MyAppCPP))
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is)=','
!
      IF (Master) WRITE (stdout,20) 'ANA_BSFLUX',                       &
     &   'Analytical kinematic bottom salinity flux'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' ANA_BSFLUX,'
!
      IF (Master) WRITE (stdout,20) 'ANA_BTFLUX',                       &
     &   'Analytical kinematic bottom temperature flux'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' ANA_BTFLUX,'
!
      IF (Master) WRITE (stdout,20) 'ANA_FSOBC',                        &
     &   'Analytical free-surface boundary conditions'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' ANA_FSOBC,'
!
      IF (Master) WRITE (stdout,20) 'ANA_M2OBC',                        &
     &   'Analytical 2D momentum boundary conditions'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' ANA_M2OBC,'
!
      IF (ANY(Lnudging(:))) THEN
        IF (Master) WRITE (stdout,20) 'ANA_NUDGCOEF',                   &
     &     'Analytical spatially varying nudging time-scales'
        is=LEN_TRIM(Coptions)+1
        Coptions(is:is+14)=' ANA_NUDGCOEF,'
      END IF
!
      IF (Master) WRITE (stdout,20) 'ANA_SRFLUX',                       &
     &   'Analytical kinematic shortwave radiation flux'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' ANA_SRFLUX,'
!
      IF (Master) WRITE (stdout,20) '!ASSEMBLE_ALL...',                 &
     &   'Using mpi_isend/mpi_recv in mp_assemble routine'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+18)=' !ASSEMBLE_ALL...,'
!
      IF (Master) WRITE (stdout,20) 'ASSUMED_SHAPE',                    &
     &   'Using assumed-shape arrays'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+15)=' ASSUMED_SHAPE,'
!
      IF (Master) WRITE (stdout,20) 'ATM_PRESS',                        &
     &   'Impose atmospheric pressure onto sea surface'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' ATM_PRESS,'
      IF (Master) WRITE (stdout,20) 'ATM2OCN_FLUXES',                   &
     &   'Surface fluxes from ATM Model.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+13)=' ATM2OCN_FLUXES,'
!
      IF (Master) WRITE (stdout,20) 'AVERAGES',                         &
     &   'Writing out time-averaged nonlinear model fields'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+10)=' AVERAGES,'
      IF (Master) WRITE (stdout,20) '!BOUNDARY_ALLGATHER',               &
     &   'Using mpi_allreduce in mp_boundary routine'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' !BOUNDARY_ALLGATHER,'
!
      IF (Master) WRITE (stdout,20) '!COLLECT_ALL...',                  &
     &   'Using mpi_isend/mpi_recv in mp_collect routine'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+17)=' !COLLECT_ALL...,'
!
      IF (Master) WRITE (stdout,20) 'CHARNOK',                          &
     &   'Charnok surface roughness from wind stress'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' CHARNOK,'
!
      IF (Master) WRITE (stdout,20) 'CRAIG_BANNER',                     &
     &   'Craig and Banner wave breaking surface flux'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+14)=' CRAIG_BANNER,'
!
      IF (Master) WRITE (stdout,20) 'CURVGRID',                         &
     &   'Orthogonal curvilinear grid'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+10)=' CURVGRID,'
!
      IF (Master) WRITE (stdout,20) 'DJ_GRADPS',                        &
     &   'Parabolic Splines density Jacobian (Shchepetkin, 2002)'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' DJ_GRADPS,'
!
      IF (Master) WRITE (stdout,20) 'DOUBLE_PRECISION',                 &
     &   'Double precision arithmetic numerical kernel.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+18)=' DOUBLE_PRECISION,'
!
      IF (Master) WRITE (stdout,20) 'FRC_COUPLING',                     &
     &   'Atmospheric forcing fields are from ESM coupling'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+14)=' FRC_COUPLING,'
!
      IF (Master) WRITE (stdout,20) 'GLS_MIXING',                       &
     &   'Generic Length-Scale turbulence closure'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' GLS_MIXING,'
      ivmix=ivmix+1
!
      IF (Master) WRITE (stdout,20) 'KANTHA_CLAYSON',                   &
     &   'Kantha and Clayson stability function formulation'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+16)=' KANTHA_CLAYSON,'
!
      IF (Master) WRITE (stdout,20) 'LIMIT_VDIFF',                      &
     &   'Impose an upper limit on vertical diffusion coefficient'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+13)=' LIMIT_VDIFF,'
!
      IF (Master) WRITE (stdout,20) 'LIMIT_VVISC',                      &
     &   'Impose an upper limit on vertical viscosity coefficient'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+13)=' LIMIT_VVISC,'
!
      IF (Master) WRITE (stdout,20) 'MASKING',                          &
     &   'Land/Sea masking'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' MASKING,'
      IF (Master) WRITE (stdout,20) 'MCT_LIB',                          &
     &   'Using Model Coupling Toolkit library'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' MCT_LIB,'
!
      IF (Master) WRITE (stdout,20) 'MIX_GEO_TS',                       &
     &   'Mixing of tracers along geopotential surfaces'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' MIX_GEO_TS,'
!
      IF (Master) WRITE (stdout,20) 'MIX_S_UV',                         &
     &   'Mixing of momentum along constant S-surfaces'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+10)=' MIX_S_UV,'
!
      IF (Master) WRITE (stdout,20) 'MPI',                              &
     &   'MPI distributed-memory configuration'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+4)=' MPI,'
!
      IF (Master) WRITE (stdout,20) 'NESTING',                          &
     &   'Nesting grids: Composite and Refinement'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' NESTING,'
!
      IF (Master) WRITE (stdout,20) 'NONLINEAR',                        &
     &   'Nonlinear Model'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+10)=' NONLINEAR,'
!
      IF (Master) WRITE (stdout,20) 'NONLIN_EOS',                       &
     &   'Nonlinear Equation of State for seawater'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' NONLIN_EOS,'
!
      IF (Master) WRITE (stdout,20) 'N2S2_HORAVG',                      &
     &   'Horizontal smoothing of buoyancy and shear'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+13)=' N2S2_HORAVG,'
!
      IF (Master) WRITE (stdout,20) '!ONE_WAY',                         &
     &   'Two-way nesting in refinement grids'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+10)=' !ONE_WAY,'
!
      IF (Master) WRITE (stdout,20) 'POWER_LAW',                        &
     &   'Power-law shape time-averaging barotropic filter'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' POWER_LAW,'
!
      IF (Master) WRITE (stdout,20) 'PROFILE',                          &
     &   'Time profiling activated'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' PROFILE,'
!
      IF (Master) WRITE (stdout,20) 'K_GSCHEME',                        &
     &   'Third-order upstream advection of TKE fields'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' K_GSCHEME,'
!
      IF (Master) WRITE (stdout,20) 'RAMP_TIDES',                       &
     &   'Ramping tidal forcing for one day'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+14)=' RAMP_TIDES,'
!
      IF (Master) WRITE (stdout,20) 'REDUCE_ALLGATHER',                 &
     &   'Using mpi_allgather in mp_reduce routine'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+18)=' REDUCE_ALLGATHER,'
!
      IF (Master) WRITE (stdout,20) 'RI_SPLINES',                       &
     &   'Parabolic Spline Reconstruction for Richardson Number'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' RI_SPLINES,'
!
      IF (Master) WRITE (stdout,20) '!RST_SINGLE',                      &
     &   'Double precision fields in restart NetCDF file'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+13)=' !RST_SINGLE,'
!
      IF (Master) WRITE (stdout,20) 'SALINITY',                         &
     &   'Using salinity'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+10)=' SALINITY,'
!
      IF (Master) WRITE (stdout,20) 'SOLAR_SOURCE',                     &
     &   'Solar Radiation Source Term'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+14)=' SOLAR_SOURCE,'
!
      IF (Master) WRITE (stdout,20) 'SOLVE3D',                          &
     &   'Solving 3D Primitive Equations'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' SOLVE3D,'
!
      IF (Master) WRITE (stdout,20) 'SPLINES_VDIFF',                    &
     &   'Parabolic Spline Reconstruction for Vertical Diffusion'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+15)=' SPLINES_VDIFF,'
!
      IF (Master) WRITE (stdout,20) 'SPLINES_VVISC',                    &
     &   'Parabolic Spline Reconstruction for Vertical Viscosity'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+15)=' SPLINES_VVISC,'
!
      IF (Master) WRITE (stdout,20) 'SSH_TIDES',                        &
     &   'Add tidal elevation to SSH climatology'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+11)=' SSH_TIDES,'
!
      IF (Master) WRITE (stdout,20) 'TS_U3HADVECTION',                  &
     &   'Third-order upstream horizontal advection of tracers'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+17)=' TS_U3HADVECTION,'
      itrcHadv=itrcHadv+1
!
      IF (Master) WRITE (stdout,20) 'TS_C4VADVECTION',                  &
     &   'Fourth-order centered vertical advection of tracers'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+17)=' TS_C4VADVECTION,'
      itrcVadv=itrcVadv+1
!
      IF (Master) WRITE (stdout,20) 'TS_DIF2',                          &
     &   'Harmonic mixing of tracers'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' TS_DIF2,'
!
      IF (Master) WRITE (stdout,20) 'UV_ADV',                           &
     &   'Advection of momentum'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+8)=' UV_ADV,'
!
      IF (Master) WRITE (stdout,20) 'UV_COR',                           &
     &   'Coriolis term'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+8)=' UV_COR,'
!
      IF (Master) WRITE (stdout,20) 'UV_U3HADVECTION',                  &
     &   'Third-order upstream horizontal advection of 3D momentum'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+17)=' UV_U3HADVECTION,'
      ivelHadv=ivelHadv+1
!
      IF (Master) WRITE (stdout,20) 'UV_C4VADVECTION',                  &
     &   'Fourth-order centered vertical advection of momentum'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+16)=' UV_C4VADVECTION,'
      ivelVadv=ivelVadv+1
!
      IF (Master) WRITE (stdout,20) 'UV_LOGDRAG',                       &
     &   'Logarithmic bottom stress'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' UV_LOGDRAG,'
      ibbl=ibbl+1
!
      IF (Master) WRITE (stdout,20) 'UV_TIDES',                         &
     &   'Add tidal currents to 2D momentum climatologies'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+10)=' UV_TIDES,'
!
      IF (Master) WRITE (stdout,20) 'UV_VIS2',                          &
     &   'Harmonic mixing of momentum'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+9)=' UV_VIS2,'
!
      IF (Master) WRITE (stdout,20) 'VAR_RHO_2D',                       &
     &   'Variable density barotropic mode'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+12)=' VAR_RHO_2D,'
!
      IF (Master) WRITE (stdout,20) 'WRF_COUPLING',                     &
     &   'WRF model coupling.'
      is=LEN_TRIM(Coptions)+1
      Coptions(is:is+14)=' WRF_COUPLING,'
!
!-----------------------------------------------------------------------
!  Stop if unsupported C-preprocessing options or report issues with
!  particular options.
!-----------------------------------------------------------------------
!
      CALL checkadj
!
!-----------------------------------------------------------------------
!  Check C-preprocessing options.
!-----------------------------------------------------------------------
!
!  Stop if more than one vertical closure scheme is selected.
!
      IF (Master.and.(ivmix.gt.1)) THEN
        WRITE (stdout,30)
  30    FORMAT (/,' CHECKDEFS - only one vertical closure scheme',      &
     &            ' is allowed.')
        exit_flag=5
      END IF
!
!  Stop if more that one bottom stress formulation is selected.
!
      IF (Master.and.(ibbl.gt.1)) THEN
        WRITE (stdout,40)
  40    FORMAT (/,' CHECKDEFS - only one bottom stress formulation is', &
     &            ' allowed.')
        exit_flag=5
      END IF
!
!  Stop if no bottom stress formulation is selected.
!
      IF (Master.and.(ibbl.eq.0)) THEN
        WRITE (stdout,50)
  50    FORMAT (/,' CHECKDEFS - no bottom stress formulation is',       &
     &            ' selected.')
        exit_flag=5
      END IF
!
!  Stop if more than one biological module is selected.
!
      IF (Master.and.(ibiology.gt.1)) THEN
        WRITE (stdout,60)
  60    FORMAT (/,' CHECKDEFS - only one biology MODULE is allowed.')
        exit_flag=5
      END IF
!
!  Stop if more that one model driver is selected.
!
      IF (Master.and.(idriver.gt.1)) THEN
        WRITE (stdout,70)
  70    FORMAT (/,' CHECKDEFS - only one model example is allowed.')
        exit_flag=5
      END IF
!
!  Stop if more than one advection scheme has been activated.
!
      IF (Master.and.(ivelHadv.gt.1)) THEN
        WRITE (stdout,140) 'horizontal','momentum','ivelHadv =',ivelHadv
        exit_flag=5
      END IF
      IF (Master.and.(ivelVadv.gt.1)) THEN
        WRITE (stdout,140) 'vertical','momentum','ivelVadv =',ivelVadv
        exit_flag=5
      END IF
      IF (Master.and.(itrcHadv.gt.1)) THEN
        WRITE (stdout,140) 'horizontal','tracers','itrcHadv =',itrcHadv
        exit_flag=5
      END IF
      IF (Master.and.(itrcVadv.gt.1)) THEN
        WRITE (stdout,140) 'vertical','tracers','itrcVadv =',itrcVadv
        exit_flag=5
      END IF
 140  FORMAT (/,' CHECKDEFS - only one ',a,' advection scheme',         &
     &        /,13x,'is allowed for ',a,', ',a,1x,i1)
      RETURN
      END SUBROUTINE checkdefs
