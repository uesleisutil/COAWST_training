      SUBROUTINE initial
!
!svn $Id: initial.F 882 2017-11-23 05:41:19Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine initializes all model variables.                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_nesting
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
!
      USE analytical_mod
      USE dateclock_mod,     ONLY : time_string
      USE distribute_mod,    ONLY : mp_bcasti
      USE ini_hmixcoef_mod,  ONLY : ini_hmixcoef
      USE metrics_mod,       ONLY : metrics
      USE nesting_mod,       ONLY : nesting
      USE set_depth_mod,     ONLY : set_depth0, set_depth
      USE omega_mod,         ONLY : omega
      USE rho_eos_mod,       ONLY : rho_eos
      USE set_massflux_mod,  ONLY : set_massflux
      USE set_masks_mod,     ONLY : set_masks
      USE stiffness_mod,     ONLY : stiffness
      USE mct_coupler_params
      USE strings_mod,       ONLY : FoundError
      USE ocean_coupler_mod, ONLY : ocn2atm_coupling
      USE ocean_coupler_mod, ONLY : ocnfatm_coupling
!
      implicit none
!
!  Local variable declarations.
!
      logical, save :: First = .TRUE.
      logical :: update = .FALSE.
      integer :: Fcount, iw, ia, io
      integer :: ng, thread, tile
      integer :: ig, nl
      integer :: cr, i, m
      integer, dimension(Ngrids) :: IniRec, Tindex
!
!=======================================================================
!   Initialize model variables.
!=======================================================================
!
!$OMP MASTER
      IF (Master) THEN
        WRITE (stdout,20) 'INITIAL: Configuring and initializing ',     &
     &                    'forward nonlinear model ...'
 20     FORMAT (/,1x,a,a,/,1x,'*******')
      END IF
!$OMP END MASTER
!
!-----------------------------------------------------------------------
!  Initialize time stepping indices and counters.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        iif(ng)=1
        indx1(ng)=1
        kstp(ng)=1
        krhs(ng)=1
        knew(ng)=1
        PREDICTOR_2D_STEP(ng)=.FALSE.
!
        iic(ng)=0
        nstp(ng)=1
        nrhs(ng)=1
        nnew(ng)=1
!
        IniRec(ng)=nrrec(ng)
        Tindex(ng)=1
!
        synchro_flag(ng)=.TRUE.
        first_time(ng)=0
        tdays(ng)=dstart
        time(ng)=tdays(ng)*day2sec
!$OMP MASTER
        ntstart(ng)=INT((time(ng)-dstart*day2sec)/dt(ng))+1
        ntend(ng)=ntstart(ng)+ntimes(ng)-1
        ntfirst(ng)=ntstart(ng)
!$OMP END MASTER
!$OMP BARRIER
        step_counter(ng)=0
        CALL time_string (time(ng), time_code(ng))
      END DO
!
!-----------------------------------------------------------------------
!  Start time wall clocks.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO thread=MyRank,MyRank
          CALL wclock_on (ng, iNLM, 2, 199, "ROMS/Nonlinear/initial.F")
        END DO
      END DO
!$OMP BARRIER
!
!=======================================================================
!  On first pass of ensemble/perturbation/iteration loop, initialize
!  model configuration.
!=======================================================================
!
      IF (Nrun.eq.ERstr) THEN
!
!-----------------------------------------------------------------------
!  Set horizontal grid, bathymetry, and Land/Sea masking (if any).
!  Use analytical functions or read in from a grid NetCDF.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP MASTER
          CALL get_grid (ng, iNLM)
!$OMP END MASTER
          CALL mp_bcasti (ng, iNLM, exit_flag)
!$OMP BARRIER
          IF (FoundError(exit_flag, NoError, 312,                       &
     &                   "ROMS/Nonlinear/initial.F")) RETURN
        END DO
!
!-----------------------------------------------------------------------
!  Set vertical S-coordinate transformation function.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP MASTER
          CALL set_scoord (ng)
!$OMP END MASTER
        END DO
!$OMP BARRIER
!
!-----------------------------------------------------------------------
!  Set barotropic time-steps average weighting function.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
!$OMP MASTER
          CALL set_weights (ng)
!$OMP END MASTER
        END DO
!$OMP BARRIER
!
!-----------------------------------------------------------------------
!  Compute various metric term combinations.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL metrics (ng, tile, iNLM)
          END DO
!$OMP BARRIER
        END DO
!
!-----------------------------------------------------------------------
!  If nesting, initialize grid spacing (on_u and om_v) in REFINED(:)
!  structure.  They are used to impose mass flux at the finer grid
!  physical boundaries.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          CALL nesting (ng, iNLM, ndxdy)
        END DO
!
!-----------------------------------------------------------------------
!  If appropriate, set spatially varying nudging coefficients time
!  scales.
!-----------------------------------------------------------------------
!
        DO ng=1,Ngrids
          IF (Lnudging(ng)) THEN
            DO tile=first_tile(ng),last_tile(ng),+1
              CALL ana_nudgcoef (ng, tile, iNLM)
            END DO
!$OMP BARRIER
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Initialize horizontal mixing coefficients. If applicable, scale
!  mixing coefficients according to the grid size (smallest area).
!  Also increase their values in sponge areas using the "visc_factor"
!  and/or "diff_factor" read from input Grid NetCDF file.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL ini_hmixcoef (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!=======================================================================
!  Initialize model state variables and forcing.  This part is
!  executed for each ensemble/perturbation/iteration run.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Set primitive variables initial conditions.
!-----------------------------------------------------------------------
!
!  Read in initial conditions from initial NetCDF file.
!
      DO ng=1,Ngrids
!$OMP MASTER
        CALL get_state (ng, iNLM, 1, INI(ng)%name,                      &
     &                  IniRec(ng), Tindex(ng))
!$OMP END MASTER
        CALL mp_bcasti (ng, iNLM, exit_flag)
!$OMP BARRIER
        IF (FoundError(exit_flag, NoError, 585,                         &
     &                 "ROMS/Nonlinear/initial.F")) RETURN
        time(ng)=io_time                     ! needed for shared-memory
      END DO
!
!-----------------------------------------------------------------------
!  Compute time independent (Zt_avg1=0) anf initial time dependent
!  depths and level thicknesses.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL set_depth0 (ng, tile, iNLM)
          CALL set_depth  (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Compute initial horizontal mass fluxes, Hz*u/n and Hz*v/m.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL set_massflux (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Compute initial S-coordinates vertical velocity. Compute initial
!  density anomaly from potential temperature and salinity via equation
!  of state for seawater.  Also compute other equation of state related
!  quatities.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL omega (ng, tile, iNLM)
          CALL rho_eos (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Read in initial forcing, climatology and assimilation data from
!  input NetCDF files.  It loads the first relevant data record for
!  the time-interpolation between snapshots.
!-----------------------------------------------------------------------
!
!  If applicable, close all input boundary, climatology, and forcing
!  NetCDF files and set associated parameters to the closed state. This
!  step is essential in iterative algorithms that run the full TLM
!  repetitively. Then, Initialize several parameters in their file
!  structure, so the appropriate input single or multi-file is selected
!  during initialization/restart.
!
      DO ng=1,Ngrids
!$OMP MASTER
        CALL close_inp (ng, iNLM)
        CALL check_multifile (ng, iNLM)
!$OMP END MASTER
        CALL mp_bcasti (ng, iNLM, exit_flag)
!$OMP BARRIER
        IF (FoundError(exit_flag, NoError, 841,                         &
     &                 "ROMS/Nonlinear/initial.F")) RETURN
      END DO
!
!  If applicable, read in input data.
!
      DO ng=1,Ngrids
!$OMP MASTER
        CALL get_idata (ng)
        CALL get_data (ng)
!$OMP END MASTER
        CALL mp_bcasti (ng, iNLM, exit_flag)
!$OMP BARRIER
        IF (FoundError(exit_flag, NoError, 856,                         &
     &                 "ROMS/Nonlinear/initial.F")) RETURN
      END DO
!
!-----------------------------------------------------------------------
!  Set internal I/O mask arrays.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL set_masks (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  If nesting and Land/Sea masking, scale horizontal interpolation
!  weights to account for land contact points.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL nesting (ng, iNLM, nmask)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  If nesting, process state fields initial conditions in the contact
!  regions.
!-----------------------------------------------------------------------
!
!  Free-surface and 2D-momentum.
!
      DO nl=1,NestLayers
        DO ig=1,GridsInLayer(nl)
          ng=GridNumber(ig,nl)
          IF (ANY(CompositeGrid(:,ng))) THEN
            CALL nesting (ng, iNLM, nFSIC)        ! free-surface
          END IF
        END DO
      END DO
!
!  Determine vertical indices and vertical interpolation weights in
!  the contact zone using initial unperturbed depth arrays.
!
      DO ng=1,Ngrids
        CALL nesting (ng, iNLM, nzwgt)
      END DO
!
!  3D-momentum and tracers.
!
      DO nl=1,NestLayers
        DO ig=1,GridsInLayer(nl)
          ng=GridNumber(ig,nl)
          IF (ANY(CompositeGrid(:,ng))) THEN
            CALL nesting (ng, iNLM, n3dIC)        ! 3D momentum
            CALL nesting (ng, iNLM, nTVIC)        ! Tracer variables
          END IF
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Compute grid stiffness.
!-----------------------------------------------------------------------
!
      IF (Lstiffness) THEN
        Lstiffness=.FALSE.
        DO ng=1,Ngrids
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL stiffness (ng, tile, iNLM)
          END DO
!$OMP BARRIER
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Read in initial forcing from coupled atm model.
!-----------------------------------------------------------------------
!
      allocate(roms_facoup(Nocn_grids))
      allocate(roms_2acoup(Nocn_grids))
      DO io=1,Nocn_grids
        roms_facoup(io)=0
        roms_2acoup(io)=0
      END DO
      DO ng=1,Ngrids
        DO ia=1,Natm_grids
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL ocnfatm_coupling (ng, ia, tile)
          END DO
!$OMP BARRIER
          IF (Master) WRITE (stdout,'(/)')
        END DO
      END DO
      DO ng=1,Ngrids
        DO ia=1,Natm_grids
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL ocn2atm_coupling (ng, ia, tile)
          END DO
!$OMP BARRIER
          IF (Master) WRITE (stdout,'(/)')
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Initialize time-stepping counter and clock.
!-----------------------------------------------------------------------
!
!  Subtract one time unit to avoid special case due to initialization
!  in the main time-stepping routine.
!
      DO ng=1,Ngrids
        iic(ng)=ntstart(ng)-1
        time(ng)=time(ng)-dt(ng)
      END DO
!
!-----------------------------------------------------------------------
!  Turn off initialization time wall clock.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO thread=MyRank,MyRank
          CALL wclock_off (ng, iNLM, 2, 1119, "ROMS/Nonlinear/initial.F")
        END DO
!$OMP BARRIER
      END DO
      RETURN
      END SUBROUTINE initial
