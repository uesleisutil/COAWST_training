      SUBROUTINE get_grid (ng, model)
!
!svn $Id: get_grid.F 927 2018-10-16 03:51:56Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine reads grid information from GRID NetCDF file.       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_nesting
      USE mod_netcdf
      USE mod_scalars
!
      USE exchange_2d_mod
      USE mp_exchange_mod, ONLY : mp_exchange2d
      USE nesting_mod,     ONLY : fill_contact
      USE nf_fread2d_mod,  ONLY : nf_fread2d
      USE strings_mod,     ONLY : FoundError, find_string
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
!
!  Local variable declarations.
!
      integer :: tile, LBi, UBi, LBj, UBj
      integer :: cr, gtype, i, status, vindex
      integer :: Vsize(4)
      real(dp), parameter :: Fscl = 1.0_dp
      real(r8) :: Fmax, Fmin
      character (len=256) :: ncname
!
      SourceFile="ROMS/Utility/get_grid.F"
!
!-----------------------------------------------------------------------
!  Inquire about the contents of grid NetCDF file:  Inquire about
!  the dimensions and variables.  Check for consistency.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 74,                            &
     &               "ROMS/Utility/get_grid.F")) RETURN
      ncname=GRD(ng)%name
!
!  Open grid NetCDF file for reading.
!
      IF (GRD(ng)%ncid.eq.-1) THEN
        CALL netcdf_open (ng, model, ncname, 0, GRD(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 82,                          &
     &                 "ROMS/Utility/get_grid.F")) THEN
          WRITE (stdout,10) TRIM(ncname)
          RETURN
        END IF
      END IF
!
!  Check grid file dimensions for consitency.
!
      CALL netcdf_check_dim (ng, model, ncname,                         &
     &                       ncid = GRD(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 93,                            &
     &               "ROMS/Utility/get_grid.F")) RETURN
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, ncname,                           &
     &                     ncid = GRD(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 100,                           &
     &               "ROMS/Utility/get_grid.F")) RETURN
!
!  Determine contact region index "cr" for which nested grid "ng" is
!  the receiver grid.
!
      DO i=1,Ncontact
        IF (Rcontact(i)%receiver_grid.eq.ng) THEN
          cr=i
          EXIT
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Check if required variables are available.
!-----------------------------------------------------------------------
!
      IF (.not.find_string(var_name,n_var,'xl',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'xl', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'el',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'el', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'spherical',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'spherical', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'h',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'h', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'f',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'f', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'pm',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'pm', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'pn',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'pn', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'dndx',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'dndx', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'dmde',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'dmde', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'angle',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'angle', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'mask_rho',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'mask_rho', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'mask_u',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'mask_u', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'mask_v',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'mask_v', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'mask_psi',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'mask_psi', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (LuvSponge(ng)) THEN
        IF (.not.find_string(var_name,n_var,'visc_factor',vindex)) THEN
          IF (Master) WRITE (stdout,20) 'visc_factor', TRIM(ncname)
          exit_flag=2
          RETURN
        END IF
      END IF
      IF (ANY(LtracerSponge(:,ng))) THEN
        IF (.not.find_string(var_name,n_var,'diff_factor',vindex)) THEN
          IF (Master) WRITE (stdout,20) 'diff_factor', TRIM(ncname)
          exit_flag=2
          RETURN
        END IF
      END IF
!
!  Read in logical switch for spherical grid configuration.
!
      spherical=.FALSE.
      IF (find_string(var_name,n_var,'spherical',vindex)) THEN
        CALL netcdf_get_lvar (ng, model, ncname, 'spherical',           &
     &                        spherical,                                &
     &                        ncid = GRD(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 267,                         &
     &                 "ROMS/Utility/get_grid.F")) RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Read in grid variables.
!-----------------------------------------------------------------------
!
!  Set 2D arrays bounds.
!
      tile=MyRank
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!  Set Vsize to zero to deativate interpolation of input data to model
!  grid in "nf_fread2d".
!
      DO i=1,4
        Vsize(i)=0
      END DO
!
!  Scan the variable list and read in needed variables.
!
      IF (Master) WRITE (stdout,'(1x)')
!
      DO i=1,n_var
        SELECT CASE (TRIM(ADJUSTL(var_name(i))))
!
!  Read in basin X-length.
!
          CASE ('xl')
            CALL netcdf_get_fvar (ng, model, ncname, 'xl',              &
     &                            xl(ng),                               &
     &                            ncid = GRD(ng)%ncid)
            IF (FoundError(exit_flag, NoError, 308,                     &
     &                     "ROMS/Utility/get_grid.F")) EXIT
!
!  Read in basin Y-length.
!
          CASE ('el')
            CALL netcdf_get_fvar (ng, model, ncname, 'el',              &
     &                            el(ng),                               &
     &                            ncid = GRD(ng)%ncid)
            IF (FoundError(exit_flag, NoError, 317,                     &
     &                     "ROMS/Utility/get_grid.F")) EXIT
!
!  Read in bathymetry.
!
          CASE ('h')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % h)
            IF (FoundError(status, nf90_noerr, 333,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              Hmin(ng)=Fmin
              Hmax(ng)=Fmax
              IF (Master) THEN
                WRITE (stdout,30) 'bathymetry at RHO-points: h',        &
     &                            ng, TRIM(ncname), hmin(ng), hmax(ng)
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, var_name(i), spval_check,         &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % h,                   &
     &                        GRID(ng) % h)
            IF (FoundError(exit_flag, NoError, 358,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % h)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % h)
!
!  Read in Land/Sea masking at RHO-points.
!
          CASE ('mask_rho')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % rmask)
            IF (FoundError(status, nf90_noerr, 386,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'mask on RHO-points: mask_rho',       &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, 'rmask', spval_check,             &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % rmask,               &
     &                        GRID(ng) % rmask)
            IF (FoundError(exit_flag, NoError, 404,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % rmask)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % rmask)
!
!  Read in Land/Sea masking at U-points.
!
          CASE ('mask_u')
            gtype=u2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask,                         &
     &                        GRID(ng) % umask)
            IF (FoundError(status, nf90_noerr, 431,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'mask on U-points: mask_u',           &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Ucontact(cr)%Npoints, Ucontact,       &
     &                        u2dvar, 'umask', spval_check,             &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % umask,               &
     &                        GRID(ng) % umask)
            IF (FoundError(exit_flag, NoError, 449,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_u2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % umask)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % umask)
!
!  Read in Land/Sea masking at V-points.
!
          CASE ('mask_v')
            gtype=v2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask,                         &
     &                        GRID(ng) % vmask)
            IF (FoundError(status, nf90_noerr, 476,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'mask on V-points: mask_v',           &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Vcontact(cr)%Npoints, Vcontact,       &
     &                        v2dvar, 'vmask', spval_check,             &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % vmask,               &
     &                        GRID(ng) % vmask)
            IF (FoundError(exit_flag, NoError, 494,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_v2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % vmask)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % vmask)
!
!  Read in Land/Sea masking at PSI-points.
!
          CASE ('mask_psi')
            gtype=p2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % pmask,                         &
     &                        GRID(ng) % pmask)
            IF (FoundError(status, nf90_noerr, 521,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'mask on PSI-points: mask_psi',       &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_p2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % pmask)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % pmask)
!
!  Read in horizontal, spatially varying factor to increase/decrease
!  viscosity (nondimensional) in specific areas of the domain.
!
          CASE ('visc_factor')
            IF (LuvSponge(ng)) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i), 0,              &
     &                          gtype, Vsize, LBi, UBi, LBj, UBj,       &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % rmask,                       &
     &                          MIXING(ng) % visc_factor)
              IF (FoundError(status, nf90_noerr, 640,                   &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'horizontal viscosity sponge '//    &
     &                              'factor: visc_factor',              &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
                CALL exchange_r2d_tile (ng, tile,                       &
     &                                  LBi, UBi, LBj, UBj,             &
     &                                  MIXING(ng) % visc_factor)
              END IF
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            EWperiodic(ng), NSperiodic(ng),       &
     &                            MIXING(ng) % visc_factor)
            END IF
!
!  Read in horizontal, spatially varying factor to increase/decrease
!  diffusivity (nondimensional) in specific areas of the domain.
!
          CASE ('diff_factor')
            IF (ANY(LtracerSponge(:,ng))) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i), 0,              &
     &                          gtype, Vsize, LBi, UBi, LBj, UBj,       &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % rmask,                       &
     &                          MIXING(ng) % diff_factor)
              IF (FoundError(status, nf90_noerr, 682,                   &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'horizontal diffusivity sponge '//  &
     &                              'factor: diff_factor',              &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
                CALL exchange_r2d_tile (ng, tile,                       &
     &                                  LBi, UBi, LBj, UBj,             &
     &                                  MIXING(ng) % diff_factor)
              END IF
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            EWperiodic(ng), NSperiodic(ng),       &
     &                            MIXING(ng) % diff_factor)
            END IF
!
!  Read in Coriolis parameter.
!
          CASE ('f')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % f)
            IF (FoundError(status, nf90_noerr, 723,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'Coriolis parameter at RHO-points: f',&
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, var_name(i), spval_check,         &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % f,                   &
     &                        GRID(ng) % f)
            IF (FoundError(exit_flag, NoError, 741,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % f)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % f)
!
!  Read in coordinate transfomation metrics (m) associated with the
!  differential distances in XI.
!
          CASE ('pm')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % pm)
            IF (FoundError(status, nf90_noerr, 771,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'reciprocal XI-grid spacing: pm',     &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, var_name(i), spval_check,         &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % pm,                  &
     &                        GRID(ng) % pm)
            IF (FoundError(exit_flag, NoError, 789,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % pm)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % pm)
!
!  Read in coordinate transfomation metrics (n) associated with the
!  differential distances in ETA.
!
          CASE ('pn')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % pn)
            IF (FoundError(status, nf90_noerr, 819,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'reciprocal ETA-grid spacing: pn',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, var_name(i), spval_check,         &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % pn,                  &
     &                        GRID(ng) % pn)
            IF (FoundError(exit_flag, NoError, 837,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % pn)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % pn)
!
!  Read in derivatives of inverse metrics factors: d(m)/d(eta).
!
          CASE ('dmde')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % dmde)
            IF (FoundError(status, nf90_noerr, 867,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'ETA-derivative of inverse metric '// &
     &                            'factor pm: dmde',                    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, var_name(i), spval_check,         &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % dmde,                &
     &                        GRID(ng) % dmde)
            IF (FoundError(exit_flag, NoError, 886,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % dmde)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % dmde)
!
!  Read in derivatives of inverse metrics factors: d(n)/d(xi).
!
          CASE ('dndx')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % dndx)
            IF (FoundError(status, nf90_noerr, 915,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'XI-derivative of inverse metric '//  &
     &                            'factor pn: dndx',                    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, var_name(i), spval_check,         &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % dndx,                &
     &                        GRID(ng) % dndx)
            IF (FoundError(exit_flag, NoError, 934,                     &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % dndx)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % dndx)
!
!  Read in X-coordinates at PSI-points.
!
          CASE ('x_psi')
            gtype=p2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % pmask,                         &
     &                        GRID(ng) % xp)
            IF (FoundError(status, nf90_noerr, 964,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of PSI-points: x_psi',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % xp)
!
!  Read in Y-coordinates at PSI-points.
!
          CASE ('y_psi')
            gtype=p2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % pmask,                         &
     &                        GRID(ng) % yp)
            IF (FoundError(status, nf90_noerr, 996,                     &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of PSI-points: y-psi',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % yp)
!
!  Read in X-coordinates at RHO-points.
!
          CASE ('x_rho')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % xr)
            IF (FoundError(status, nf90_noerr, 1028,                    &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of RHO-points: x-rho',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Rcontact(cr)%Npoints, Rcontact,     &
     &                          r2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Xr,                &
     &                          GRID(ng) % xr)
              IF (FoundError(exit_flag, NoError, 1047,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % xr)
!
!  Read in Y-coordinates at RHO-points.
!
          CASE ('y_rho')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % yr)
            IF (FoundError(status, nf90_noerr, 1072,                    &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of RHO-points: y_rho',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Rcontact(cr)%Npoints, Rcontact,     &
     &                          r2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Yr,                &
     &                          GRID(ng) % yr)
              IF (FoundError(exit_flag, NoError, 1091,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % yr)
!
!  Read in X-coordinates at U-points.
!
          CASE ('x_u')
            gtype=u2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask,                         &
     &                        GRID(ng) % xu)
            IF (FoundError(status, nf90_noerr, 1116,                    &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of U-points: x_u',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Ucontact(cr)%Npoints, Ucontact,     &
     &                          u2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Xu,                &
     &                          GRID(ng) % xu)
              IF (FoundError(exit_flag, NoError, 1135,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % xu)
!
!  Read in Y-coordinates at U-points.
!
          CASE ('y_u')
            gtype=u2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask,                         &
     &                        GRID(ng) % yu)
            IF (FoundError(status, nf90_noerr, 1160,                    &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of U-points: y_u',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Ucontact(cr)%Npoints, Ucontact,     &
     &                          u2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Yu,                &
     &                          GRID(ng) % yu)
              IF (FoundError(exit_flag, NoError, 1179,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % yu)
!
!  Read in X-coordinates at V-points.
!
          CASE ('x_v')
            gtype=v2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask,                         &
     &                        GRID(ng) % xv)
            IF (FoundError(status, nf90_noerr, 1204,                    &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of V-points: x_v',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Vcontact(cr)%Npoints, Vcontact,     &
     &                          v2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Xv,                &
     &                          GRID(ng) % xv)
              IF (FoundError(exit_flag, NoError, 1223,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % xv)
!
!  Read in Y-coordinates at V-points.
!
          CASE ('y_v')
            gtype=v2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask,                         &
     &                        GRID(ng) % yv)
            IF (FoundError(status, nf90_noerr, 1248,                    &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of V-points: y_v',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Vcontact(cr)%Npoints, Vcontact,     &
     &                          v2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Yv,                &
     &                          GRID(ng) % yv)
              IF (FoundError(exit_flag, NoError, 1267,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          .FALSE., .FALSE.,                       &
     &                          GRID(ng) % yv)
!
!  Read in longitude at PSI-points.
!
          CASE ('lon_psi')
            IF (spherical) THEN
              gtype=p2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % pmask,                       &
     &                          GRID(ng) % lonp)
              IF (FoundError(status, nf90_noerr, 1293,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of PSI-points: lon_psi', &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % lonp)
            END IF
!
!  Read in latitude at PSI-points.
!
          CASE ('lat_psi')
            IF (spherical) THEN
              gtype=p2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % pmask,                       &
     &                          GRID(ng) % latp)
              IF (FoundError(status, nf90_noerr, 1327,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of PSI-points lat_psi',   &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % latp)
            END IF
!
!  Read in longitude at RHO-points.
!
          CASE ('lon_rho')
            IF (spherical) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, LonMin(ng), LonMax(ng),           &
     &                          GRID(ng) % rmask,                       &
     &                          GRID(ng) % lonr)
              IF (FoundError(status, nf90_noerr, 1361,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of RHO-points: lon_rho', &
     &                              ng, TRIM(ncname),                   &
     &                              LonMin(ng), LonMax(ng)
                END IF
              END IF
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Rcontact(cr)%Npoints, Rcontact,     &
     &                          r2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Xr,                &
     &                          GRID(ng) % lonr)
              IF (FoundError(exit_flag, NoError, 1380,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % lonr)
            END IF
!
!  Read in latitude at RHO-points.
!
          CASE ('lat_rho')
            IF (spherical) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, LatMin(ng), LatMax(ng),           &
     &                          GRID(ng) % rmask,                       &
     &                          GRID(ng) % latr)
              IF (FoundError(status, nf90_noerr, 1406,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of RHO-points lat_rho',   &
     &                              ng, TRIM(ncname),                   &
     &                              LatMin(ng), LatMax(ng)
                END IF
              END IF
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Rcontact(cr)%Npoints, Rcontact,     &
     &                          r2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Yr,                &
     &                          GRID(ng) % latr)
              IF (FoundError(exit_flag, NoError, 1425,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % latr)
            END IF
!
!  Read in longitude at U-points.
!
          CASE ('lon_u')
            IF (spherical) THEN
              gtype=u2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % umask,                       &
     &                          GRID(ng) % lonu)
              IF (FoundError(status, nf90_noerr, 1451,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of U-points: lon_u',     &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Ucontact(cr)%Npoints, Ucontact,     &
     &                          u2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Xu,                &
     &                          GRID(ng) % lonu)
              IF (FoundError(exit_flag, NoError, 1469,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % lonu)
            END IF
!
!  Read in latitude at U-points.
!
          CASE ('lat_u')
            IF (spherical) THEN
              gtype=u2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % umask,                       &
     &                          GRID(ng) % latu)
              IF (FoundError(status, nf90_noerr, 1495,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of U-points: lat_u',      &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Ucontact(cr)%Npoints, Ucontact,     &
     &                          u2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Yu,                &
     &                          GRID(ng) % latu)
              IF (FoundError(exit_flag, NoError, 1513,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % latu)
            END IF
!
!  Read in longitude at V-points.
!
          CASE ('lon_v')
            IF (spherical) THEN
              gtype=v2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % vmask,                       &
     &                          GRID(ng) % lonv)
              IF (FoundError(status, nf90_noerr, 1539,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of V-points: lon_v',     &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Vcontact(cr)%Npoints, Vcontact,     &
     &                          v2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Xv,                &
     &                          GRID(ng) % lonv)
              IF (FoundError(exit_flag, NoError, 1557,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % lonv)
            END IF
!
!  Read in latitude at V-points.
!
          CASE ('lat_v')
            IF (spherical) THEN
              gtype=v2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % vmask,                       &
     &                          GRID(ng) % latv)
              IF (FoundError(status, nf90_noerr, 1583,                  &
     &                       "ROMS/Utility/get_grid.F")) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of V-points: lat_v',      &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              CALL fill_contact(ng, model, tile,                        &
     &                          cr, Vcontact(cr)%Npoints, Vcontact,     &
     &                          v2dvar, var_name(i), spval_check,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CONTACT_METRIC(cr) % Yv,                &
     &                          GRID(ng) % latv)
              IF (FoundError(exit_flag, NoError, 1601,                  &
     &                       "ROMS/Utility/get_grid.F")) RETURN
              CALL mp_exchange2d (ng, tile, model, 1,                   &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            NghostPoints,                         &
     &                            .FALSE., .FALSE.,                     &
     &                            GRID(ng) % latv)
            END IF
!
!  Read in angle (radians) between XI-axis and EAST at RHO-points.
!
          CASE ('angle')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % angler)
            IF (FoundError(status, nf90_noerr, 1626,                    &
     &                     "ROMS/Utility/get_grid.F")) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'angle between XI-axis and EAST: '//  &
     &                            'angler',                             &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            CALL fill_contact(ng, model, tile,                          &
     &                        cr, Rcontact(cr)%Npoints, Rcontact,       &
     &                        r2dvar, 'angler', spval_check,            &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        CONTACT_METRIC(cr) % angler,              &
     &                        GRID(ng) % angler)
            IF (FoundError(exit_flag, NoError, 1645,                    &
     &                     "ROMS/Utility/get_grid.F")) RETURN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % angler)
            END IF
            CALL mp_exchange2d (ng, tile, model, 1,                     &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          GRID(ng) % angler)
        END SELECT
      END DO
      IF (FoundError(exit_flag, NoError, 1783,                          &
     &               "ROMS/Utility/get_grid.F")) THEN
        IF (Master) WRITE (stdout,40) TRIM(var_name(i)), TRIM(ncname)
        RETURN
      END IF
!
! Close GRID NetCDF file.
!
      CALL netcdf_close (ng, model, GRD(ng)%ncid, ncname, .FALSE.)
      IF (FoundError(exit_flag, NoError, 1951,                          &
     &               "ROMS/Utility/get_grid.F")) RETURN
!
  10  FORMAT (/,' GET_GRID - unable to open grid NetCDF file: ',a)
  20  FORMAT (/,' GET_GRID - unable to find grid variable: ',a,         &
     &        /,12x,'in grid NetCDF file: ',a)
  30  FORMAT (3x,' GET_GRID    - ',a,/,19x,                             &
     &        '(Grid = ',i2.2,', File: ',a,')',/,19x,                   &
     &        '(Min = ', 1p,e15.8,0p,' Max = ',1p,e15.8,0p,')')
  40  FORMAT (/,' GET_GRID - error while reading variable: ',a,         &
     &        /,12x,'in grid NetCDF file: ',a)
  50  FORMAT (/,' GET_GRID - Reading adjoint sensitivity scope arrays', &
     &        ' from file:',/12x,a)
      RETURN
      END SUBROUTINE get_grid
