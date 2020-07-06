      SUBROUTINE main3d (RunInterval)
!
!svn $Id: main3d.F 927 2018-10-16 03:51:56Z arango $
!=======================================================================
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This subroutine is the main driver for nonlinear ROMS/TOMS when     !
!  configurated as a full 3D baroclinic ocean model.  It  advances     !
!  forward the primitive equations for all  nested  grids, if any,     !
!  for the specified time interval (seconds), RunInterval.             !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mct_coupler_params
      USE mod_iounits
      USE mod_nesting
      USE mod_scalars
      USE mod_stepping
!
      USE atm2ocn_flux_mod,     ONLY : atm2ocn_flux
      USE dateclock_mod,        ONLY : time_string
      USE diag_mod,             ONLY : diag
      USE gls_corstep_mod,      ONLY : gls_corstep
      USE gls_prestep_mod,      ONLY : gls_prestep
      USE ini_fields_mod,       ONLY : ini_fields, ini_zeta
      USE nesting_mod,          ONLY : nesting
      USE nesting_mod,          ONLY : do_twoway
      USE ocean_coupler_mod,    ONLY : ocean_coupling
      USE omega_mod,            ONLY : omega
      USE rho_eos_mod,          ONLY : rho_eos
      USE rhs3d_mod,            ONLY : rhs3d
      USE set_avg_mod,          ONLY : set_avg
      USE set_depth_mod,        ONLY : set_depth
      USE set_massflux_mod,     ONLY : set_massflux
      USE set_tides_mod,        ONLY : set_tides
      USE set_vbc_mod,          ONLY : set_vbc
      USE set_zeta_mod,         ONLY : set_zeta
      USE step2d_mod,           ONLY : step2d
      USE step3d_t_mod,         ONLY : step3d_t
      USE step3d_uv_mod,        ONLY : step3d_uv
      USE strings_mod,          ONLY : FoundError
      USE wvelocity_mod,        ONLY : wvelocity
!
      implicit none
!
!  Imported variable declarations.
!
      real(dp), intent(in) :: RunInterval
!
!  Local variable declarations.
!
      logical :: DoNestLayer, Time_Step
      integer :: Nsteps, Rsteps
      integer :: ig, iw, ia, il, istep, ng, nl, tile
      integer :: my_iif, next_indx1
!
!=======================================================================
!  Time-step nonlinear 3D primitive equations by the specified time.
!=======================================================================
!
!  Time-step the 3D kernel for the specified time interval (seconds),
!  RunInterval.
!
      Time_Step=.TRUE.
      DoNestLayer=.TRUE.
!
      KERNEL_LOOP : DO WHILE (Time_Step)
!
!  In nesting applications, the number of nesting layers (NestLayers) is
!  used to facilitate refinement grids and composite/refinament grids
!  combinations. Otherwise, the solution it is looped once for a single
!  grid application (NestLayers = 1).
!
        nl=0
        TwoWayInterval(1:Ngrids)=0.0_r8
!
        NEST_LAYER : DO WHILE (DoNestLayer)
!
!  Determine number of time steps to compute in each nested grid layer
!  based on the specified time interval (seconds), RunInterval. Non
!  nesting applications have NestLayers=1. Notice that RunInterval is
!  set in the calling driver. Its value may span the full period of the
!  simulation, a multi-model coupling interval (RunInterval > ifac*dt),
!  or just a single step (RunInterval=0).
!
          CALL ntimesteps (iNLM, RunInterval, nl, Nsteps, Rsteps)
          IF (FoundError(exit_flag, NoError, 222,                       &
     &                   "ROMS/Nonlinear/main3d.F")) RETURN
          IF ((nl.le.0).or.(nl.gt.NestLayers)) EXIT
!
!  Time-step governing equations for Nsteps.
!
          STEP_LOOP : DO istep=1,Nsteps
!
!  Set time indices and time clock.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              iic(ng)=iic(ng)+1
              nstp(ng)=1+MOD(iic(ng)-ntstart(ng),2)
              nnew(ng)=3-nstp(ng)
              nrhs(ng)=nstp(ng)
              time(ng)=time(ng)+dt(ng)
              tdays(ng)=time(ng)*sec2day
              CALL time_string (time(ng), time_code(ng))
              IF (step_counter(ng).eq.Rsteps) Time_Step=.FALSE.
            END DO
!
!-----------------------------------------------------------------------
!  Read in required data, if any, from input NetCDF files.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
!$OMP MASTER
              CALL get_data (ng)
!$OMP END MASTER
!$OMP BARRIER
              IF (FoundError(exit_flag, NoError, 254,                   &
     &                       "ROMS/Nonlinear/main3d.F")) RETURN
            END DO
!
!-----------------------------------------------------------------------
!  If applicable, process input data: time interpolate between data
!  snapshots.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=first_tile(ng),last_tile(ng),+1
                CALL set_data (ng, tile)
              END DO
!$OMP BARRIER
            END DO
            IF (FoundError(exit_flag, NoError, 270,                     &
     &                     "ROMS/Nonlinear/main3d.F")) RETURN
!
!-----------------------------------------------------------------------
!  Initialize all time levels and compute other initial fields.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (iic(ng).eq.ntstart(ng)) THEN
!
!  Initialize free-surface and compute initial level thicknesses and
!  depths.
!
                DO tile=first_tile(ng),last_tile(ng),+1
                  CALL ini_zeta (ng, tile, iNLM)
                  CALL set_depth (ng, tile, iNLM)
                END DO
!$OMP BARRIER
!
!  Initialize other state variables.
!
                DO tile=last_tile(ng),first_tile(ng),-1
                  CALL ini_fields (ng, tile, iNLM)
                END DO
!$OMP BARRIER
!
!  Extract donor grid initial data at contact points and store it in
!  REFINED structure so it can be used for the space-time interpolation.
!
                IF (RefinedGrid(ng)) THEN
                  CALL nesting (ng, iNLM, ngetD)
                END IF
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  Compute horizontal mass fluxes (Hz*u/n and Hz*v/m), density related
!  quatities and report global diagnostics.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=first_tile(ng),last_tile(ng),+1
                CALL set_massflux (ng, tile, iNLM)
                CALL rho_eos (ng, tile, iNLM)
                CALL diag (ng, tile)
              END DO
!$OMP BARRIER
            END DO
            IF (FoundError(exit_flag, NoError, 351,                     &
     &                     "ROMS/Nonlinear/main3d.F")) RETURN
!
!-----------------------------------------------------------------------
!  Couple ocean to atmosphere and wave model grids.
!-----------------------------------------------------------------------
            IF (iic(ng).ne.ntstart(ng)) THEN
              CALL ocean_coupling (nl)
            END IF
!
!-----------------------------------------------------------------------
!  Set fields for vertical boundary conditions. Process tidal forcing,
!  if any.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=first_tile(ng),last_tile(ng),+1
                CALL atm2ocn_flux (ng, tile)
                CALL set_vbc (ng, tile)
                CALL set_tides (ng, tile)
              END DO
!$OMP BARRIER
            END DO
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for bottom stress variables.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, nbstr)
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  Compute time-dependent vertical/horizontal mixing coefficients for
!  momentum and tracers. Compute S-coordinate vertical velocity,
!  diagnostically from horizontal mass divergence.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL omega (ng, tile, iNLM)
                CALL wvelocity (ng, tile, nstp(ng))
              END DO
!$OMP BARRIER
            END DO
!
!-----------------------------------------------------------------------
!  Set free-surface to it time-averaged value.  If applicable,
!  accumulate time-averaged output data which needs a irreversible
!  loop in shared-memory jobs.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=first_tile(ng),last_tile(ng),+1     ! irreversible
                CALL set_zeta (ng, tile)
                CALL set_avg (ng, tile)
              END DO
!$OMP BARRIER
            END DO
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for 3D kernel free-surface.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, nzeta)
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  If appropriate, write out fields into output NetCDF files.  Notice
!  that IO data is written in delayed and serial mode.  Exit if last
!  time step.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
!$OMP MASTER
              CALL output (ng)
!$OMP END MASTER
!$OMP BARRIER
              IF ((FoundError(exit_flag, NoError, 630,                  &
     &                        "ROMS/Nonlinear/main3d.F")).or.                            &
     &            ((iic(ng).eq.(ntend(ng)+1)).and.(ng.eq.Ngrids))) THEN
                RETURN
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  If refinement grid, interpolate (space, time) state variables
!  contact points from donor grid extracted data.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                CALL nesting (ng, iNLM, nputD)
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  Compute right-hand-side terms for 3D equations.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL rhs3d (ng, tile)
                CALL gls_prestep (ng, tile)
              END DO
!$OMP BARRIER
            END DO
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for right-hand-side terms
!  (tracers).
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, nrhst)
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  Solve the vertically integrated primitive equations for the
!  free-surface and barotropic momentum components.
!-----------------------------------------------------------------------
!
            LOOP_2D : DO my_iif=1,MAXVAL(nfast)+1
!
!  Set time indices for predictor step. The PREDICTOR_2D_STEP switch
!  it is assumed to be false before the first time-step.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                next_indx1=3-indx1(ng)
                IF (.not.PREDICTOR_2D_STEP(ng).and.                     &
     &              my_iif.le.(nfast(ng)+1)) THEN
                  PREDICTOR_2D_STEP(ng)=.TRUE.
                  iif(ng)=my_iif
                  IF (iif(ng).eq.1) THEN
                    kstp(ng)=indx1(ng)
                  ELSE
                    kstp(ng)=3-indx1(ng)
                  END IF
                  knew(ng)=3
                  krhs(ng)=indx1(ng)
                END IF
!
!  Predictor step - Advance barotropic equations using 2D time-step
!  ==============   predictor scheme.  No actual time-stepping is
!  performed during the auxiliary (nfast+1) time-step. It is needed
!  to finalize the fast-time averaging of 2D fields, if any, and
!  compute the new time-evolving depths.
!
                IF (my_iif.le.(nfast(ng)+1)) THEN
                  DO tile=last_tile(ng),first_tile(ng),-1
                    CALL step2d (ng, tile)
                  END DO
!$OMP BARRIER
                END IF
              END DO
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the state variables
!  associated with the 2D engine Predictor Step section.
!  If refinement, check mass flux conservation between coarse and
!  fine grids during debugging.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (ANY(CompositeGrid(:,ng))) THEN
                  CALL nesting (ng, iNLM, n2dPS)
                END IF
                IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                  CALL nesting (ng, iNLM, nmflx)
                END IF
              END DO
!
!  Set time indices for corrector step.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (PREDICTOR_2D_STEP(ng)) THEN
                  PREDICTOR_2D_STEP(ng)=.FALSE.
                  knew(ng)=next_indx1
                  kstp(ng)=3-knew(ng)
                  krhs(ng)=3
                  IF (iif(ng).lt.(nfast(ng)+1)) indx1(ng)=next_indx1
                END IF
!
!  Corrector step - Apply 2D time-step corrector scheme.  Notice that
!  ==============   there is not need for a corrector step during the
!  auxiliary (nfast+1) time-step.
!
                IF (iif(ng).lt.(nfast(ng)+1)) THEN
                  DO tile=first_tile(ng),last_tile(ng),+1
                    CALL step2d (ng, tile)
                  END DO
!$OMP BARRIER
                END IF
              END DO
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the state variables
!  associated with the 2D engine Corrector step section.
!  If refinement, check mass flux conservation between coarse and
!  fine grids during debugging.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (ANY(CompositeGrid(:,ng))) THEN
                  CALL nesting (ng, iNLM, n2dCS)
                END IF
                IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                  CALL nesting (ng, iNLM, nmflx)
                END IF
              END DO
            END DO LOOP_2D
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the time-averaged
!  momentum fluxes (DU_avg1, DV_avg1) and free-surface (Zt_avg).
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, n2dfx)
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  Recompute depths and thicknesses using the new time filtered
!  free-surface.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL set_depth (ng, tile, iNLM)
              END DO
!$OMP BARRIER
            END DO
!
!  If nesting, determine vertical indices and vertical interpolation
!  weights in the contact zone using new depth arrays.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              CALL nesting (ng, iNLM, nzwgt)
            END DO
!
!-----------------------------------------------------------------------
!  Time-step 3D momentum equations.
!-----------------------------------------------------------------------
!
!  Time-step 3D momentum equations and couple with vertically
!  integrated equations.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL step3d_uv (ng, tile)
              END DO
!$OMP BARRIER
            END DO
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for 3D momentum (u,v),
!  adjusted 2D momentum (ubar,vbar), and fluxes (Huon, Hvom).
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, n3duv)
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  Time-step vertical mixing turbulent equations and passive tracer
!  source and sink terms, if applicable.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=first_tile(ng),last_tile(ng),+1
                CALL omega (ng, tile, iNLM)
                CALL gls_corstep (ng, tile)
              END DO
!$OMP BARRIER
            END DO
!
!-----------------------------------------------------------------------
!  Time-step tracer equations.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL step3d_t (ng, tile)
              END DO
!$OMP BARRIER
            END DO
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for Tracer Variables.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, n3dTV)
              END IF
            END DO
!
!-----------------------------------------------------------------------
!  If refinement grids, perform two-way coupling between fine and
!  coarse grids. Correct coarse grid tracers values at the refinement
!  grid with refined accumulated fluxes.  Then, replace coarse grid
!  state variable with averaged refined grid values (two-way nesting).
!  Update coarse grid depth variables.
!
!  The two-way exchange of infomation between nested grids needs to be
!  done at the correct time-step and in the right sequence.
!-----------------------------------------------------------------------
!
            DO il=NestLayers,1,-1
              DO ig=1,GridsInLayer(il)
                ng=GridNumber(ig,il)
                IF (do_twoway(iNLM, nl, il, ng, istep)) THEN
                  CALL nesting (ng, iNLM, n2way)
                END IF
              END DO
            END DO
!
!-----------------------------------------------------------------------
!  If donor to a finer grid, extract data for the external contact
!  points. This is the latest solution for the coarser grid.
!
!  It is stored in the REFINED structure so it can be used for the
!  space-time interpolation when "nputD" argument is used above.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (DonorToFiner(ng)) THEN
                CALL nesting (ng, iNLM, ngetD)
              END IF
            END DO
          END DO STEP_LOOP
        END DO NEST_LAYER
      END DO KERNEL_LOOP
      RETURN
      END SUBROUTINE main3d
