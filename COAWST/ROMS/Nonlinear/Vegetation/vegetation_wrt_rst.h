/*
** svn $Id: vegetation_wrt.h 429 2015-06-10 10:40:26Z arango $
*************************************************** Hernan G. Arango ***
<<<<<<< HEAD
** Copyright (c) 2002-2017 The ROMS/TOMS Group                        **
=======
** Copyright (c) 2002-2019 The ROMS/TOMS Group                        **
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
**    See License_ROMS.txt                                            **
*************************************************** John C. Warner    **
*************************************************** Neil K. Ganju     **
*************************************************** Alexis Beudin     **
*************************************************** Tarandeep S. Kalra**
**                                                                    **
**  Writes vegetation input parameters into output restart            **
**  NetCDF files.                                                     **
**  It is included in routine "wrt_rst.F".                            **
**                                                                    **
************************************************************************
*/
# if defined VEG_DRAG || defined VEG_BIOMASS
!
!  Write out vegetation properties 
! 
      DO i=1,NVEGP 
          scale=1.0_r8
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, RST(ng)%ncid,                    &
     &                       RST(ng)%Vid(idvprp(i)),                    &
     &                       RST(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, NVEG, scale,        &
#  ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#  endif
     &                       VEG(ng) % plant(:,:,:,i))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN 
              WRITE (stdout,10) TRIM(Vname(1,idvprp(i))), RST(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
      END DO 
# endif 
!
# ifdef VEG_STREAMING 
!
!  Write out wave dissipation due to vegetation 
! 
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idWdvg), &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     VEG(ng)%Dissip_veg )
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN 
            WRITE (stdout,10) TRIM(Vname(1,idWdvg)), RST(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
# endif
! 
<<<<<<< HEAD
# ifdef MARSH_WAVE_THRUST
!
!  Write out initial masking for marshes 
=======
# ifdef MARSH_DYNAMICS
#  ifdef MARSH_WAVE_THRUST
!
!  Store marsh masking from marsh cells. 
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
! 
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idTims), &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
<<<<<<< HEAD
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     VEG(ng)%marsh_mask)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
=======
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     VEG(ng)%marsh_mask)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                   __FILE__)) THEN
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          IF (Master) THEN 
            WRITE (stdout,10) TRIM(Vname(1,idTims)), RST(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
!
<<<<<<< HEAD
!  Write out wave thrust on marsh output 
! 
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idTmsk), &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     VEG(ng)%mask_thrust)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN 
            WRITE (stdout,10) TRIM(Vname(1,idTmsk)), RST(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
!
!  Define maximum thrust due to waves. 
!
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idTmax), &
     &                     RST(ng)%Rindex, gtype,                       &    
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     VEG(ng)%Thrust_max)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTmax)), RST(ng)%Rindex
=======
!  Total thrust from all directions due to waves. 
!
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idTtot), &
     &                     RST(ng)%Rindex, gtype,                       &    
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     VEG(ng)%Thrust_total)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                   __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTtot)), RST(ng)%Rindex
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
!
<<<<<<< HEAD
!  Define maximum thrust due to waves. 
!
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idTmax), &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     VEG(ng)%Thrust_max)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTmax)), RST(ng)%Rindex
=======
#   ifdef MARSH_SED_EROSION 
!
!  Marsh sediment flux out from marsh cells from each sedclass.
!
      DO i=1,NST
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid,                      &
     &                     RST(ng)%Vid(idTmfo(i)),                      &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
#    ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#    endif
     &                     VEG(ng) % marsh_flux_out(:,:,i))
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                   __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTmfo(i))), RST(ng)%Rindex
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
<<<<<<< HEAD
!
!  Define Tonelli masking based thrust due to waves. 
!
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idTton), &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
# endif
     &                     VEG(ng)%Thrust_tonelli)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                 __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTton)), RST(ng)%Rindex
=======
      END DO
#   endif
!
#   ifdef MARSH_RETREAT
!
!  Amount of marsh retreat from all directions. 
!
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, RST(ng)%ncid, RST(ng)%Vid(idTmmr), &
     &                     RST(ng)%Rindex, gtype,                       &    
     &                     LBi, UBi, LBj, UBj, scale,                   &
#   ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#   endif
     &                     VEG(ng)%marsh_retreat)
        IF (FoundError(status, nf90_noerr, __LINE__,                    &
     &                   __FILE__)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTmmr)), RST(ng)%Rindex
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
<<<<<<< HEAD
=======
!
#   endif
#  endif 
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
# endif 

