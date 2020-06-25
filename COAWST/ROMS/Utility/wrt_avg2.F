#include "cppdefs.h"
#ifdef AVERAGES2
<<<<<<< HEAD
#undef AVERAGES_DETIDE
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      SUBROUTINE wrt_avg2 (ng)
!
!svn $Id: wrt_avg.F 702 2008-08-12 16:44:47Z kate $
!================================================== Hernan G. Arango ===
<<<<<<< HEAD
!  Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
=======
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine writes model time-averaged fields into averages     !
!  NetCDF file.                                                        !
!                                                                      !
<<<<<<< HEAD
=======
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_average2
      USE mod_forces
      USE mod_grid
<<<<<<< HEAD
#ifdef ICE_MODEL
      USE mod_ice
#endif
=======
# ifdef ICE_MODEL
      USE mod_ice
# endif
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
<<<<<<< HEAD
# if defined SEDIMENT || defined BBL_MODEL
      USE mod_sediment
# endif
# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
      USE mod_tides
# endif
# ifdef DISTRIBUTE
!
      USE distribute_mod, ONLY : mp_bcasti
# endif
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
# ifdef SOLVE3D
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
# endif
=======
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE strings_mod,     ONLY : FoundError
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: Fcount, gfactor, gtype, i, itrc, status

      real(r8) :: scale
!
<<<<<<< HEAD
=======
      SourceFile=__FILE__
!
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Write out time-averaged fields when appropriate.
!-----------------------------------------------------------------------
!
<<<<<<< HEAD
      if (exit_flag.ne.NoError) RETURN
=======
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
# if defined WRITE_WATER && defined MASKING
        gfactor=-1
# else
        gfactor=1
# endif
!
!  Set time record index.
!
      AVG2(ng)%Rindex=AVG2(ng)%Rindex+1
      Fcount=AVG2(ng)%Fcount
      AVG2(ng)%Nrec(Fcount)=AVG2(ng)%Nrec(Fcount)+1
!
!  Write out averaged time.
!
      CALL netcdf_put_fvar (ng, iNLM, AVG2(ng)%name,                    &
     &                      TRIM(Vname(idtime,ng)), AVG2time(ng:),      &
     &                      (/AVG2(ng)%Rindex/), (/1/),                 &
     &                      ncid = AVG2(ng)%ncid,                       &
     &                      varid = AVG2(ng)%Vid(idtime))
<<<<<<< HEAD
      IF (exit_flag.ne.NoError) RETURN
=======
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out free-surface (m).
!
      IF (Aout2(idFsur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idFsur),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgzeta)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idFsur)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD

# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided free-surface (m).
!
      scale=1.0_r8
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                       &
     &                   AVG2(ng)%Vid(idFsuD),                          &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % rmask_avg2,                         &
#  endif
     &                   TIDES(ng) % zeta_detided)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idFsuD)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout2(idUbar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUbar),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgu2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbar)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD

# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 2D momentum component (m/s) in the XI-direction.
!
      scale=1.0_r8
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                       &
     &                   AVG2(ng)%Vid(idu2dD),                          &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % umask_avg2,                         &
#  endif
     &                   TIDES(ng) % ubar_detided)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idu2dD)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout2(idVbar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVbar),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgv2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbar)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD

# if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 2D momentum component (m/s) in the ETA-direction.
!
      scale=1.0_r8
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid, AVG2(ng)%Vid(idv2dD), &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % vmask_avg2,                         &
#  endif
     &                   TIDES(ng) % vbar_detided)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idv2dD)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out 2D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout2(idu2dE,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idu2dE),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgu2dE)
<<<<<<< HEAD
        IF (FoundError(status, nf90_noerr, __LINE__,                   &
    &                  __FILE__)) THEN
=======
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu2dE)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout2(idv2dN,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idv2dN),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgv2dN)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv2dN)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

<<<<<<< HEAD
# if defined FORWARD_WRITE && defined SOLVE3D
!
!  Write out time-averaged mass fluxes for 3D momentum coupling.
!
      scale=1.0_r8
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid, AVG2(ng)%Vid(idUfx1), &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % umask_avg2,                         &
#  endif
     &                   AVERAGE2(ng) % avgDU_avg1)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUfx1)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid, AVG2(ng)%Vid(idUfx2), &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % umask_avg2,                         &
#  endif
     &                   AVERAGE2(ng) % avgDU_avg2)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUfx2)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid, AVG2(ng)%Vid(idVfx1), &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % vmask_avg2,                         &
#  endif
     &                   AVERAGE2(ng) % avgDV_avg1)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVfx1)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid, AVG2(ng)%Vid(idVfx2), &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#  ifdef MASKING
     &                   GRID(ng) % umask_avg2,                         &
#  endif
     &                   AVERAGE2(ng) % avgDV_avg2)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVfx2)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
# ifdef NEARSHORE_MELLOR
!
!  Write out 2D radiation stress, Sxx-component.
!
      IF (Aout2(idW2xx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idW2xx),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSxx2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xx)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Sxy-component.
!
      IF (Aout2(idW2xy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idW2xy),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSxy2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2xy)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress, Syy-component.
!
      IF (Aout2(idW2yy,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idW2yy),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSyy2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idW2yy)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress in the XI-direction.
!
      IF (Aout2(idU2rs,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idU2rs),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgu2RS)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2rs)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D radiation stress in the ETA-direction
!
      IF (Aout2(idV2rs,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idV2rs),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgv2RS)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2rs)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D stokes momentum component (m/s) in the XI-direction.
!
      IF (Aout2(idU2Sd,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idU2Sd),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgu2Sd)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idU2Sd)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D stokes momentum component (m/s) in the ETA-direction.
!
      IF (Aout2(idV2Sd,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idV2Sd),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % vmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgv2Sd)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idV2Sd)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
# ifdef SOLVE3D
!
!  Write out 3D momentum component (m/s) in the XI-direction.
!
      IF (Aout2(idUvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUvel),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % umask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgu3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUvel)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD

#  if defined AVERAGES_DETIDE && (defined SSH_TIDES || defined UV_TIDES)
!
!  Write out detided 3D momentum component (m/s) in the XI-direction.
!
      scale=1.0_r8
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid, AVG2(ng)%Vid(idu3dD), &
     &                   AVG2(ng)%Rindex, gtype,                        &
     &                   LBi, UBi, LBj, UBj, scale,                     &
#   ifdef MASKING
     &                   GRID(ng) % umask_avg2,                         &
#   endif
     &                   TIDES(ng) % u_detided)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idu3dD)), AVG2(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
#  endif
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out 3D momentum component (m/s) in the ETA-direction.
!
      IF (Aout2(idVvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVvel),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
<<<<<<< HEAD
     &                     LBi, UBi, LBj, UBj, scale,                   &
=======
     &                     LBi, UBi, LBj, UBj, scale,                     &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#  ifdef MASKING
     &                     GRID(ng) % vmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgv3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvel)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout2(idu3dE,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idu3dE),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
<<<<<<< HEAD
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgu3dE)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
=======
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgu3dE)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu3dE)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout2(idv3dN,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idv3dN),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
<<<<<<< HEAD
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgv3dN)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
=======
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgv3dN)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv3dN)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD

=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        IF (Aout2(idTvar(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                   &
     &                       AVG2(ng)%Tid(itrc),                        &
     &                       AVG2(ng)%Rindex, gtype,                    &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % rmask_avg2,                     &
#  endif
     &                       AVERAGE2(ng) % avgt(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
<<<<<<< HEAD
    &                    __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTvar(itrc))),            &
     &                        AVG2(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# ifdef BIO_GOANPZ
!---------------------------------------------
!  Write out stationary tracer type variables.
!---------------------------------------------
      DO itrc=1,NTS(ng)
        !print *, 'XXX',AVG2(ng)%ncid,avg2TSid(itrc,ng),AVG2(ng)%Rindex
        !print *, 'xxx',Aout2(idTSvar(itrc),ng)
        IF (Aout2(idTSvar(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                   &
     &                       AVG2(ng)%TSid(itrc),                       &
     &                       AVG2(ng)%Rindex, gtype,                    &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % rmask_avg2,                     &
#  endif
     &                       AVERAGE2(ng) % avgst(:,:,1,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
    &                    __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTSvar(itrc))),           &
     &                        AVG2(ng)%Rindex
=======
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTvar(itrc))),            &
     &                          AVG2(ng)%Rindex
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
<<<<<<< HEAD
# endif
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out density anomaly.
!
      IF (Aout2(idDano,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idDano),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgrho)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idDano)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD
=======

>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#  ifdef LMD_SKPP
!
!  Write out depth of surface boundary layer.
!
      IF (Aout2(idHsbl,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idHsbl),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avghsbl)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHsbl)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  ifdef LMD_BKPP
!
!  Write out depth of bottom boundary layer.
!
      IF (Aout2(idHbbl,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idHbbl),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avghbbl)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHbbl)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
<<<<<<< HEAD
=======
#  ifdef SSSFLX
!
!  Write out sea surface salinity correction flux
!
      IF (Aout2(idSSSf,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSSSf),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgsssflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSSSf)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#  ifdef ICE_MODEL
!
!  Write out ice 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout2(idUice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUice),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % umask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avguice)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out ice 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout2(idVice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVice),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % vmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgvice)
<<<<<<< HEAD
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
    &                __FILE__)) THEN
=======
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward ice momentum component (m/s) at RHO-points.
!
      IF (Aout2(idUiceE,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUiceE),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
<<<<<<< HEAD
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avguiceE)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
=======
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avguiceE)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUiceE)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
<<<<<<< HEAD
          ioerror=status
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          RETURN
        END IF
      END IF
!
!  Write out 2D Northward ice momentum component (m/s) at RHO-points.
!
      IF (Aout2(idViceN,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idViceN),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
<<<<<<< HEAD
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgviceN)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
=======
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgviceN)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idViceN)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD

=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
! Write out ice concentration.
!
      IF (Aout2(idAice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idAice),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgaice)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice thickness.
!
      IF (Aout2(idHice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idHice),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avghice)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice/snow surface temperature.
!
      IF (Aout2(idTice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idTice),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgtice)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice internal temperature.
!
      IF (Aout2(idTimid,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idTimid),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgtimid)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTimid)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
<<<<<<< HEAD
=======
! Write out ice top temperature.
!
      IF (Aout2(idT2ice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idT2ice),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgt2ice)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idT2ice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
! Write out snow thickness.
!
      IF (Aout2(idHsno,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idHsno),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avghsno)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHsno)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
<<<<<<< HEAD
#ifdef MELT_PONDS
!
! Write out surface water fraction (on ice).
!
      IF (Aout2(idApond,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idApond),                       &
=======
!
! Write out ice age.
!
      IF (Aout2(idAgeice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idAgeice),                      &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgageice)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAgeice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice-ocean mass flux
!
      IF (Aout2(idIomflx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idIomflx),                      &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                   AVERAGE2(ng) % avgiomflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIomflx)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out internal ice stress component 11
!
      IF (Aout2(idSig11,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSig11),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgsig11)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig11)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out internal ice stress component 12
!
      IF (Aout2(idSig12,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSig12),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgsig12)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig12)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out internal ice stress component 22
!
      IF (Aout2(idSig22,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSig22),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgsig22)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig22)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out temperature of molecular sublayer under ice.
!
      IF (Aout2(idT0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idT0mk),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgT0mk)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idT0mk)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out salinity of molecular sublayer under ice.
!
      IF (Aout2(idS0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idS0mk),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgS0mk)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idS0mk)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out frazil ice growth rate.
!
      IF (Aout2(idWfr,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idWfr),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgWfr)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWfr)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice growth/melt rate.
!
      IF (Aout2(idWai,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idWai),                         &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
<<<<<<< HEAD
#   endif
     &                     AVERAGE2(ng) % avgapond)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idApond)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out surface water thickness (on ice).
!
      IF (Aout2(idHpond,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idHpond),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avghpond)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idHpond)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
#endif
!
! Write out ice age.
!
      IF (Aout2(idAgeice,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idAgeice),                      &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgageice)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAgeice)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice-ocean mass flux
!
      IF (Aout2(idIomflx,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idIomflx),                      &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgiomflx)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idIomflx)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
=======
#   endif
     &                     AVERAGE2(ng) % avgWai)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWai)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice growth/melt rate.
!
      IF (Aout2(idWao,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idWao),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgWao)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWao)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice growth/melt rate.
!
      IF (Aout2(idWio,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idWio),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgWio)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWio)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice melt runoff rate.
!
      IF (Aout2(idWro,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idWro),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgWro)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWro)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice divergence rate.
!
      IF (Aout2(idWdiv,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idWdiv),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgWdiv)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWdiv)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice-water friction velocity
!
      IF (Aout2(idTauiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idTauiw),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgutau_iw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTauiw)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out ice-water momentum transfer coefficient
!
      IF (Aout2(idChuiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idChuiw),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgchu_iw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idChuiw)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
#  endif
#  ifdef ICE_DIAGS
!
!  Write out ocean albedo
!
      IF (Aout2(idAlbe,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idAlbe),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgAlbe)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAlbe)),                    &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice albedo
!
      IF (Aout2(idAlbI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idAlbI),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgAlbI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idAlbI)),                    &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward longwave radiation
!
      IF (Aout2(idLWdn,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idLWdn),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgLWdn)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLWdn)),                    &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out downward shortwave
!
      IF (Aout2(idSWdn,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSWdn),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSWdn)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWdn)),                    &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out latent heat over ice
!
      IF (Aout2(idLatI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idLatI),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgLatI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLatI)),                    &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat over ice
!
      IF (Aout2(idSenI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSenI),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSenI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSenI)),                    &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward longwave over ice
!
      IF (Aout2(idLWupI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idLWupI),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgLWupI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLWupI)),                   &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward shortwave over ice
!
      IF (Aout2(idSWupI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSWupI),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSWupI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWupI)),                   &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out upward shortwave over ice
!
      IF (Aout2(idSWinI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSWinI),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSWinI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWinI)),                   &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave through ice
!
      IF (Aout2(idSWthruI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSWthruI),                     &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSWthruI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSWthruI)),                 &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux from ice
!
      IF (Aout2(idSaltI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSaltI),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSaltI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSaltI)),                   &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out salt flux in leads
!
      IF (Aout2(idSaltL,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSaltL),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSaltL)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSaltL)),                   &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out air-ocean heat flux
!
      IF (Aout2(idQao,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idQao),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgQao)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQao)),                     &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-air heat flux
!
      IF (Aout2(idQai,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idQai),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgQai)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQai)),                     &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice-ocean heat flux
!
      IF (Aout2(idQio,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idQio),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgQio)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQio)),                     &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out ice heat flux
!
      IF (Aout2(idQi2,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idQi2),                         &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgQi2)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idQi2)),                     &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out snow-ice conversion
!
      IF (Aout2(idSnoI,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSnoI),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgSnoI)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSnoI)),                    &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
# endif
!
!  Write out 2D potential vorticity.
!
      IF (Aout2(id2dPV,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*p2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(id2dPV),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % pmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgpvor2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id2dPV)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D relative vorticity.
!
      IF (Aout2(id2dRV,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*p2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(id2dRV),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % pmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgrvor2d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id2dRV)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# ifdef SOLVE3D
!
!  Write out 3D potential vorticity.
!
      IF (Aout2(id3dPV,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*p3dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(id3dPV),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % pmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgpvor3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id3dPV)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          RETURN
        END IF
      END IF
!
<<<<<<< HEAD
! Write out internal ice stress component 11
!
      IF (Aout2(idSig11,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSig11),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgsig11)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig11)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out internal ice stress component 12
!
      IF (Aout2(idSig12,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSig12),                       &
=======
!  Write out 3D relative vorticity.
!
      IF (Aout2(id3dRV,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*p3dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(id3dRV),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % pmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgrvor3d)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,id3dRV)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
# ifdef SOLVE3D
#  if defined RUNOFF
!
!  Write out surface fresh water from land.
!
      IF (Aout2(idRunoff,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idRunoff),                      &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
<<<<<<< HEAD
     &                     AVERAGE2(ng) % avgsig12)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig12)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out internal ice stress component 22
!
      IF (Aout2(idSig22,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSig22),                       &
=======
     &                     AVERAGE2(ng) % avgRunoff)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idRunoff)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  if defined BULK_FLUXES || defined ECOSIM || defined ATM_PRESS
!
!  Write out surface air pressure.
!
      IF (Aout2(idPair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idPair),                        &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
<<<<<<< HEAD
     &                   AVERAGE2(ng) % avgsig22)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSig22)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out temperature of molecular sublayer under ice.
!
      IF (Aout2(idT0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idT0mk),                        &
=======
     &                     AVERAGE2(ng) % avgPair)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPair)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
#  if defined BULK_FLUXES || defined ECOSIM
!
!  Write out surface winds.
!
      IF (Aout2(idUair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUair),                        &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
<<<<<<< HEAD
     &                     AVERAGE2(ng) % avgT0mk)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idT0mk)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          RETURN
        END IF
      END IF
!
! Write out salinity of molecular sublayer under ice.
!
      IF (Aout2(idS0mk,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idS0mk),                        &
=======
     &                     AVERAGE2(ng) % avgUwind)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUair)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

      IF (Aout2(idVair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVair),                        &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
<<<<<<< HEAD
     &                     AVERAGE2(ng) % avgS0mk)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idS0mk)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
=======
     &                     AVERAGE2(ng) % avgVwind)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVair)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          RETURN
        END IF
      END IF
!
<<<<<<< HEAD
! Write out ice-water friction velocity
!
      IF (Aout2(idTauiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idTauiw),                       &
=======
!  Write out 2D Eastward surface wind (m/s) at RHO-points.
!
      IF (Aout2(idUairE,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUairE),                       &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
<<<<<<< HEAD
     &                     AVERAGE2(ng) % avgutau_iw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTauiw)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
=======
     &                     AVERAGE2(ng) % avguwindE)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
     &               __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUairE)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          RETURN
        END IF
      END IF
!
<<<<<<< HEAD
! Write out ice-water momentum transfer coefficient
!
      IF (Aout2(idChuiw,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idChuiw),                       &
=======
!  Write out 2D Northward surface wind (m/s) at RHO-points.
!
      IF (Aout2(idVairN,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVairN),                       &
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
<<<<<<< HEAD
     &                     AVERAGE2(ng) % avgchu_iw)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idChuiw)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
=======
     &                     AVERAGE2(ng) % avgvwindN)
      IF (FoundError(status, nf90_noerr, __LINE__,                      &
     &               __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVairN)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          RETURN
        END IF
      END IF
#  endif
<<<<<<< HEAD
# endif
# ifdef SOLVE3D
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!  Write out surface net heat flux.
!
      IF (Aout2(idTsur(itemp),ng)) THEN
#  ifdef ADJOINT
<<<<<<< HEAD
#   if defined AD_SENSITIVITY   || defined OBS_SENSITIVITY || \
        defined OPT_OBSERVATIONS
        scale=1.0_r8/(rho0*Cp)
=======
#   if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
       defined OPT_OBSERVATIONS
       scale=1.0_r8/(rho0*Cp)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#   else
        scale=1.0_r8
#   endif
#  else
        scale=rho0*Cp
#  endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idTsur(itemp)),                 &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgstf)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTsur(itemp))),             &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface salt flux  (PSU m/s = kg salt/m2/s).
!
      IF (Aout2(idTsur(isalt),ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idTsur(isalt)),                 &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgswf)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTsur(isalt))),             &
     &                        AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  ifdef BULK_FLUXES
!
!  Write out latent heat flux.
!
      IF (Aout2(idLhea,ng)) THEN
#   ifdef ADJOINT
        scale=1.0_r8
#   else
        scale=rho0*Cp
#   endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idLhea),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avglhf)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLhea)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat flux.
!
      IF (Aout2(idShea,ng)) THEN
#   ifdef ADJOINT
        scale=1.0_r8
#   else
        scale=rho0*Cp
#   endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idShea),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgshf)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idShea)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out longwave radiation flux.
!
      IF (Aout2(idLrad,ng)) THEN
<<<<<<< HEAD
#    ifdef ADJOINT
=======
#   ifdef ADJOINT
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
        scale=1.0_r8
#   else
        scale=rho0*Cp
#   endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idLrad),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avglrf)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLrad)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD
!
!  Write out surface u-wind.
!
      IF (Aout2(idUair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUair),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avguwind)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUair)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface v-wind.
!
      IF (Aout2(idVair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVair),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#  ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#  endif
     &                     AVERAGE2(ng) % avgvwind)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVair)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward surface wind (m/s) at RHO-points.
!
      IF (Aout2(idUairE,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUairE),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avguwindE)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUairE)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Northward surface wind (m/s) at RHO-points.
!
      IF (Aout2(idVairN,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVairN),                       &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgvwindN)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVairN)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF

=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#   ifdef EMINUSP
!
!  Write out evaportaion rate (kg/m2/s).
!
      IF (Aout2(idevap,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idevap),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#    endif
     &                     AVERAGE2(ng) % avgevap)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idevap)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out precipitation rate (kg/m2/s).
!
      IF (Aout2(idrain,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idrain),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#    endif
     &                     AVERAGE2(ng) % avgrain)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idrain)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#   endif
#  endif
#  ifdef SHORTWAVE
!
!  Write out shortwave radiation flux.
!
      IF (Aout2(idSrad,ng)) THEN
<<<<<<< HEAD
#    ifdef ADJOINT
=======
#    if defined ADJOINT || defined SOGLOBEC
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
        scale=1.0_r8
#    else
        scale=rho0*Cp
#    endif
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idSrad),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
<<<<<<< HEAD
#   ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#   endif
     &                     AVERAGE2(ng) % avgsrf)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
    &                  __FILE__)) THEN
=======
#    ifdef MASKING
     &                     GRID(ng) % rmask_avg2,                       &
#    endif
     &                     AVERAGE2(ng) % avgsrf)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSrad)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
# endif
!
!  Write out surface u-momentum stress.
!
      IF (Aout2(idUsms,ng)) THEN
# ifdef ADJOINT
<<<<<<< HEAD
#  if defined AD_SENSITIVITY   || defined OBS_SENSITIVITY || \
       defined OPT_OBSERVATIONS
=======
#  if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
      defined OPT_OBSERVATIONS
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
        scale=1.0_r8/rho0
#  else
        scale=1.0_r8
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUsms),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgsus)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUsms)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface v-momentum stress.
!
      IF (Aout2(idVsms,ng)) THEN
# ifdef ADJOINT
<<<<<<< HEAD
#  if defined AD_SENSITIVITY   || defined OBS_SENSITIVITY || \
       defined OPT_OBSERVATIONS
=======
#  if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
      defined OPT_OBSERVATIONS
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
        scale=1.0_r8/rho0
#  else
        scale=1.0_r8
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVsms),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgsvs)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVsms)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom u-momentum stress.
!
      IF (Aout2(idUbms,ng)) THEN
# ifdef ADJOINT
<<<<<<< HEAD
#  if defined AD_SENSITIVITY   || defined OBS_SENSITIVITY || \
       defined OPT_OBSERVATIONS
=======
#  if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
      defined OPT_OBSERVATIONS
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
        scale=1.0_r8/rho0
#  else
        scale=1.0_r8
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idUbms),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % umask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgbus)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbms)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom v-momentum stress.
!
      IF (Aout2(idVbms,ng)) THEN
# ifdef ADJOINT
<<<<<<< HEAD
#  if defined AD_SENSITIVITY   || defined OBS_SENSITIVITY || \
       defined OPT_OBSERVATIONS
=======
#  if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
      defined OPT_OBSERVATIONS
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
        scale=1.0_r8/rho0
#  else
        scale=1.0_r8
#  endif
# else
        scale=rho0
# endif
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                     &
     &                     AVG2(ng)%Vid(idVbms),                        &
     &                     AVG2(ng)%Rindex, gtype,                      &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % vmask_avg2,                       &
# endif
     &                     AVERAGE2(ng) % avgbvs)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
<<<<<<< HEAD
    &                  __FILE__)) THEN
=======
     &                 __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbms)), AVG2(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
<<<<<<< HEAD
# if defined SEDIMENT && defined BEDLOAD
!
!  Write out bed load transport in U-direction.
!
      DO itrc=1,NST
        IF (Aout2(idUbld(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*u2dvar
          status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                   &
     &                       AVG2(ng)%Vid(idUbld(itrc)),                &
     &                       AVG2(ng)%Rindex, gtype,                    &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % umask_avg2,                     &
#  endif
     &                       AVERAGE2(ng) % avgbedldu(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
    &                    __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idUbld(itrc))),            &
     &                          AVG2(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
!  Write out bed load transport in V-direction.
!
        IF (Aout2(idVbld(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*v2dvar
          status=nf_fwrite2d(ng, iNLM, AVG2(ng)%ncid,                   &
     &                       AVG2(ng)%Vid(idVbld(itrc)),                &
     &                       AVG2(ng)%Rindex, gtype,                    &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % vmask_avg2,                     &
#  endif
     &                       AVERAGE2(ng) % avgbedldv(:,:,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
    &                    __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idVbld(itrc))),            &
     &                          AVG2(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
# endif
=======
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!-----------------------------------------------------------------------
!  Synchronize time-average NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, iNLM, AVG2(ng)%name, AVG2(ng)%ncid)
<<<<<<< HEAD
      IF (exit_flag.ne.NoError) RETURN

      IF (Master) WRITE (stdout,20) AVG2(ng)%Rindex
!
  10  FORMAT (/,' WRT_AVG2 - error while writing variable: ',a,/,11x,   &
     &        'into averages NetCDF file for time record: ',i4)
  20  FORMAT (6x,'WRT_AVG2   - wrote averaged fields into time ',       &
     &        'record =',t72,i7.7)
#else
      SUBROUTINE wrt_avg2
=======
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

# ifdef NESTING
      IF (Master) WRITE (stdout,20) AVG2(ng)%Rindex, ng
# else
      IF (Master) WRITE (stdout,20) AVG2(ng)%Rindex
# endif
!
  10  FORMAT (/,' WRT_AVG2 - error while writing variable: ',a,/,11x,   &
     &        'into averages NetCDF file for time record: ',i4)
  20  FORMAT (6x,'WRT_AVG2     - wrote averaged',t39,'fields',t58,      &
# ifdef NESTING
     &        'in record = ',i7.7,t92,i2.2)
# else
     &        'in record = ',i7.7)
# endif
#else
      SUBROUTINE wrt_avg2(ng)
      integer, intent(in) :: ng
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
#endif
      RETURN
      END SUBROUTINE wrt_avg2
