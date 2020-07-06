      SUBROUTINE set_contact (ng, model)
!
!svn $Id: set_contact.F 923 2018-09-26 21:20:30Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads nested grids contact points for each contact     !
!  region from input NetCDF file and allocates/initializes all the     !
!  contact region structures declared in "mod_nesting".                !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_nesting
      USE mod_netcdf
      USE mod_scalars
!
      USE strings_mod, ONLY : FoundError, find_string
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
!
!  Local variable declarations.
!
      logical, dimension((Ngrids-1)*2) :: Lcoincident
      logical, dimension((Ngrids-1)*2) :: Lcomposite
      logical, dimension((Ngrids-1)*2) :: Lmosaic
      logical, dimension((Ngrids-1)*2) :: Lrefinement
      logical, dimension((Ngrids-1)*2) :: Linterpolate
!
      integer :: cr, dg, ibry, ic, ig, ip,  m, rg, vindex
      integer :: my_Ncontact, my_Ngrids, my_nLweights, my_nQweights
      integer :: NGCid
      integer, dimension(Ngrids) :: my_Lm, my_Mm
      integer, dimension(Ngrids) :: refine_factor
      integer, dimension((Ngrids-1)*2) :: NpointsR
      integer, dimension((Ngrids-1)*2) :: NpointsU
      integer, dimension((Ngrids-1)*2) :: NpointsV
!
      real(r8), allocatable :: Lweight(:,:)
      real(r8), allocatable :: Xrg(:), Yrg(:)
      real(r8), allocatable :: angle(:)
      real(r8), allocatable :: dmde(:), dndx(:)
      real(r8), allocatable :: f(:)
      real(r8), allocatable :: h(:)
      real(r8), allocatable :: mask(:)
      real(r8), allocatable :: pm(:), pn(:)
!
!-----------------------------------------------------------------------
!  Read in nesting grids contact point packed information into local
!  arrays.
!-----------------------------------------------------------------------
!
!  Open contact points NetCDF file for reading.
!
      CALL netcdf_open (ng, model, NGCname, 0, NGCid)
      IF (FoundError(exit_flag, NoError, 73,                            &
     &               "ROMS/Utility/set_contact.F")) THEN
        WRITE (stdout,20) TRIM(NGCname)
  20    FORMAT (/,' READ_CONTACT - unable to open contact points ',     &
     &            ' NetCDF file: ',a)
        RETURN
      END IF
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, NGCname,                          &
     &                     ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 85,                            &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Check if the number of nested grids is correct.
!
      CALL netcdf_get_dim (ng, model, NGCname,                          &
     &                     ncid = NGCid,                                &
     &                     DimName = 'Ngrids',                          &
     &                     DimSize = my_Ngrids)
      IF (FoundError(exit_flag, NoError, 94,                            &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (my_Ngrids.ne.Ngrids) THEN
        IF (Master) THEN
          WRITE (stdout,10) 'inconsistent parameter, Ngrids = ',        &
     &                      Ngrids, my_Ngrids
  10      FORMAT (/,' READ_CONTACT - ', a, i4, 2x, i4,                  &
     &            /,16x,'in input file:'2x,a)
        END IF
        exit_flag=5
        RETURN
      END IF
!
!  Check number of contact regions, Ncontact = (Ngrids-1)*2.
!
      CALL netcdf_get_dim (ng, model, NGCname,                          &
     &                     ncid = NGCid,                                &
     &                     DimName = 'Ncontact',                        &
     &                     DimSize = my_Ncontact)
      IF (FoundError(exit_flag, NoError, 114,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (my_Ncontact.ne.(Ngrids-1)*2) THEN
        IF (Master) THEN
          WRITE (stdout,10) 'inconsistent parameter, Ncontact = ',      &
     &                      (Ngrids-1)*2, my_Ncontact
        END IF
        exit_flag=5
        RETURN
      END IF
      Ncontact=(Ngrids-1)*2
!
!  Check number of contact points for bilinear interpolation weights.
!
      CALL netcdf_get_dim (ng, model, NGCname,                          &
     &                     ncid = NGCid,                                &
     &                     DimName = 'nLweights',                       &
     &                     DimSize = my_nLweights)
      IF (FoundError(exit_flag, NoError, 133,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (my_nLweights.ne.4) THEN
        IF (Master) THEN
          WRITE (stdout,10) 'inconsistent parameter, nLweights = ',     &
     &                      4, my_nLweights
        END IF
        exit_flag=5
        RETURN
      END IF
!
!  Get number of contact points in input NetCDF file.  It may include
!  or not contact point over land when land/sea masking is activated.
!
      CALL netcdf_get_dim (ng, model, NGCname,                          &
     &                     ncid = NGCid,                                &
     &                     DimName = 'datum',                           &
     &                     DimSize = NCdatum)
      IF (FoundError(exit_flag, NoError, 173,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in grid dimensions and if the grid order is correct.  The order
!  of the grids is important in nesting.
!
      CALL netcdf_get_ivar (ng, model, NGCname, 'Lm',                   &
     &                      my_Lm,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 182,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      CALL netcdf_get_ivar (ng, model, NGCname, 'Mm',                   &
     &                      my_Mm,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 188,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      DO ig=1,Ngrids
        IF (my_Lm(ig).ne.Lm(ig)) THEN
          IF (Master) THEN
            WRITE (stdout,10) 'inconsistent grid order, Lm = ',         &
     &                        Lm(ig), my_Lm(ig)
          END IF
          exit_flag=5
          RETURN
        END IF
        IF (my_Mm(ig).ne.Mm(ig)) THEN
          IF (Master) THEN
            WRITE (stdout,10) 'inconsistent grid order, Mm = ',         &
     &                        Mm(ig), my_Mm(ig)
          END IF
          exit_flag=5
          RETURN
        END IF
      END DO
!
!  Read in nesting types logical switches, 1:Ngrids.
!
      CALL netcdf_get_lvar (ng, model, NGCname, 'coincident',           &
     &                      Lcoincident,                                &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 215,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      CALL netcdf_get_lvar (ng, model, NGCname, 'composite',            &
     &                      Lcomposite,                                 &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 221,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      CALL netcdf_get_lvar (ng, model, NGCname, 'mosaic',               &
     &                      Lmosaic,                                    &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 227,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      CALL netcdf_get_lvar (ng, model, NGCname, 'refinement',           &
     &                      Lrefinement,                                &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 233,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in refinement factor from donor grid, 1:Ngrids.
!
      CALL netcdf_get_ivar (ng, model, NGCname, 'refine_factor',        &
     &                      refine_factor,                              &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 241,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in vertical interpolation at contact points switch, 1:Ncontact.
!
      CALL netcdf_get_lvar (ng, model, NGCname, 'interpolate',          &
     &                      Linterpolate,                               &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 249,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in data donor and data receiver grid number, 1:Ncontact.
!
      IF (.not.allocated(donor_grid)) THEN
        allocate ( donor_grid((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'donor_grid',           &
     &                      donor_grid,                                 &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 261,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (.not.allocated(receiver_grid)) THEN
        allocate ( receiver_grid((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'receiver_grid',        &
     &                      receiver_grid,                              &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 271,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in donor grid (I,J) indices at PSI points used to extract
!  refined grid. Values have a fill value of -999 if not applicable.
!
      IF (.not.allocated(I_left)) THEN
        allocate ( I_left(Ngrids) )
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'I_left',               &
     &                      I_left,                                     &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 284,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(I_right)) THEN
        allocate ( I_right(Ngrids) )
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'I_right',              &
     &                      I_right,                                    &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 294,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(J_bottom)) THEN
        allocate ( J_bottom(Ngrids) )
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'J_bottom',             &
     &                      J_bottom,                                   &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 304,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(J_top)) THEN
        allocate ( J_top(Ngrids) )
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'J_top',                &
     &                      J_top,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 314,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in starting and ending contact points (RHO-, U-, V-) index in
!  packed data vectors for each contact region, 1:Ncontact.
!
      IF (.not.allocated(NstrR)) THEN
        allocate ( NstrR((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'NstrR',                &
     &                      NstrR,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 327,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(NendR)) THEN
        allocate ( NendR((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'NendR',                &
     &                      NendR,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 337,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(NstrU)) THEN
        allocate ( NstrU((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'NstrU',                &
     &                      NstrU,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 347,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(NendU)) THEN
        allocate ( NendU((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'NendU',                &
     &                      NendU,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 357,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(NstrV)) THEN
        allocate ( NstrV((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'NstrV',                &
     &                      NstrV,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 367,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
      IF (.not.allocated(NendV)) THEN
        allocate ( NendV((Ngrids-1)*2) )
        Dmem(ng)=Dmem(ng)+REAL((Ngrids-1)*2,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'NendV',                &
     &                      NendV,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 377,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact region number for each contact point, 1:NCdatum.
!
      IF (.not.allocated(contact_region)) THEN
        allocate ( contact_region(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'contact_region',       &
     &                      contact_region,                             &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 389,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in flag (1:NCdatum) to determine if the contact point is at
!  receiver grid interior (zero value) or on the boundary (1:western,
!  2:southern, 3:eastern, 4:northern).
!
      IF (.not.allocated(on_boundary)) THEN
        allocate ( on_boundary(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'on_boundary',          &
     &                      on_boundary,                                &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 403,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in I-left and J-bottom indices of donor cell containing contact
!  point, 1:NCdatum.
!
      IF (.not.allocated(Idg_cp)) THEN
        allocate ( Idg_cp(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'Idg',                  &
     &                      Idg_cp,                                     &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 416,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (.not.allocated(Jdg_cp)) THEN
        allocate ( Jdg_cp(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'Jdg',                  &
     &                      Jdg_cp,                                     &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 426,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in I- and J-indices of receiver grid contact point, 1:NCdatum.
!
      IF (.not.allocated(Irg_cp)) THEN
        allocate ( Irg_cp(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'Irg',                  &
     &                      Irg_cp,                                     &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 438,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (.not.allocated(Jrg_cp)) THEN
        allocate ( Jrg_cp(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_ivar (ng, model, NGCname, 'Jrg',                  &
     &                      Jrg_cp,                                     &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 448,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in X- and Y-location of receiver grid contact point, 1:NCdatum.
!
      IF (.not.allocated(Xrg)) THEN
        allocate ( Xrg(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'Xrg',                  &
     &                      Xrg,                                        &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 460,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (.not.allocated(Yrg)) THEN
        allocate ( Yrg(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'Yrg',                  &
     &                      Yrg,                                        &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 470,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact point horizontal linear interpolation weights,
!  1:NCdatum.
!
      IF (.not.allocated(Lweight)) THEN
        allocate ( Lweight(my_nLweights,NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(my_nLweights*NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'Lweight',              &
     &                      Lweight,                                    &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 483,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact point bathymetry at RHO-points, 1:NCdatum.
!
      IF (.not.allocated(h)) THEN
        allocate ( h(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'h',                    &
     &                      h,                                          &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 511,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact point Coriolis parameter at RHO-points, 1:NCdatum.
!
      IF (.not.allocated(f)) THEN
        allocate ( f(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'f',                    &
     &                      f,                                          &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 523,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact point curvilinar coordinates in XI- and ETA-direction
!  at RHO-points, 1:NCdatum.
!
      IF (.not.allocated(pm)) THEN
        allocate ( pm(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'pm',                   &
     &                      pm,                                         &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 536,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (.not.allocated(pn)) THEN
        allocate ( pn(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'pn',                   &
     &                      pn,                                         &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 546,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact point inverse metric factor in XI- and ETA-direction,
!  1:NCdatum.
!
      IF (.not.allocated(dndx)) THEN
        allocate ( dndx(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'dndx',                 &
     &                      dndx,                                       &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 559,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
      IF (.not.allocated(dmde)) THEN
        allocate ( dmde(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'dmde',                 &
     &                      dmde,                                       &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 569,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact point angle between XI-axis and EAST, 1:NCdatum.
!
      IF (.not.allocated(angle)) THEN
        allocate ( angle(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'angle',                &
     &                      angle,                                      &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 581,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Read in contact point land/sea mask, 1:NCdatum.
!
      IF (.not.allocated(mask)) THEN
        allocate ( mask(NCdatum) )
        Dmem(ng)=Dmem(ng)+REAL(NCdatum,r8)
      END IF
      CALL netcdf_get_fvar (ng, model, NGCname, 'mask',                 &
     &                      mask,                                       &
     &                      ncid = NGCid)
      IF (FoundError(exit_flag, NoError, 593,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!  Close contact points NetCDF file.
!
      CALL netcdf_close (ng, model, NGCid, NGCname, .FALSE.)
      IF (FoundError(exit_flag, NoError, 599,                           &
     &               "ROMS/Utility/set_contact.F")) RETURN
!
!-----------------------------------------------------------------------
!  Unpack contact point data into nesting structures (type T_NGC).
!-----------------------------------------------------------------------
!
!  Determine number of contact points in each contact region.
!
      IF (.not.allocated(NCpoints)) THEN
        allocate ( NCpoints(Ncontact) )
        Dmem(ng)=Dmem(ng)+REAL(Ncontact,r8)
      END IF
      DO cr=1,Ncontact
        NpointsR(cr)=NendR(cr)-NstrR(cr)+1
        NpointsU(cr)=NendU(cr)-NstrU(cr)+1
        NpointsV(cr)=NendV(cr)-NstrV(cr)+1
        NCpoints(cr)=NpointsR(cr)+NpointsU(cr)+NpointsV(cr)
      END DO
!
!  Allocate grid connectivity (type T_NGC) structures.
!
      allocate ( Rcontact(Ncontact) )
      allocate ( Ucontact(Ncontact) )
      allocate ( Vcontact(Ncontact) )
      Dmem(ng)=Dmem(ng)+3.0_r8*REAL(Ncontact,r8)
!
!  Allocate arrays in grid connectivity structure.
!
      DO cr=1,Ncontact
        dg=donor_grid(cr)
        rg=receiver_grid(cr)
        allocate ( Rcontact(cr) % Irg(NpointsR(cr)) )
        allocate ( Ucontact(cr) % Irg(NpointsU(cr)) )
        allocate ( Vcontact(cr) % Irg(NpointsV(cr)) )
        allocate ( Rcontact(cr) % Jrg(NpointsR(cr)) )
        allocate ( Ucontact(cr) % Jrg(NpointsU(cr)) )
        allocate ( Vcontact(cr) % Jrg(NpointsV(cr)) )
        allocate ( Rcontact(cr) % Idg(NpointsR(cr)) )
        allocate ( Ucontact(cr) % Idg(NpointsU(cr)) )
        allocate ( Vcontact(cr) % Idg(NpointsV(cr)) )
        allocate ( Rcontact(cr) % Jdg(NpointsR(cr)) )
        allocate ( Ucontact(cr) % Jdg(NpointsU(cr)) )
        allocate ( Vcontact(cr) % Jdg(NpointsV(cr)) )
        Dmem(dg)=Dmem(dg)+4.0_r8*REAL(NpointsR(cr),r8)
        Dmem(dg)=Dmem(dg)+4.0_r8*REAL(NpointsU(cr),r8)
        Dmem(dg)=Dmem(dg)+4.0_r8*REAL(NpointsV(cr),r8)
        allocate ( Rcontact(cr) % Kdg(N(dg),NpointsR(cr)) )
        allocate ( Ucontact(cr) % Kdg(N(dg),NpointsU(cr)) )
        allocate ( Vcontact(cr) % Kdg(N(dg),NpointsV(cr)) )
        Dmem(dg)=Dmem(dg)+REAL(N(dg)*NpointsR(cr),r8)
        Dmem(dg)=Dmem(dg)+REAL(N(dg)*NpointsU(cr),r8)
        Dmem(dg)=Dmem(dg)+REAL(N(dg)*NpointsV(cr),r8)
        allocate ( Rcontact(cr) % Lweight(4,NpointsR(cr)) )
        allocate ( Ucontact(cr) % Lweight(4,NpointsU(cr)) )
        allocate ( Vcontact(cr) % Lweight(4,NpointsV(cr)) )
        Dmem(dg)=Dmem(dg)+4.0_r8*REAL(NpointsR(cr),r8)
        Dmem(dg)=Dmem(dg)+4.0_r8*REAL(NpointsU(cr),r8)
        Dmem(dg)=Dmem(dg)+4.0_r8*REAL(NpointsV(cr),r8)
        allocate ( Rcontact(cr) % Vweight(2,N(dg),NpointsR(cr)) )
        allocate ( Ucontact(cr) % Vweight(2,N(dg),NpointsU(cr)) )
        allocate ( Vcontact(cr) % Vweight(2,N(dg),NpointsV(cr)) )
        Dmem(dg)=Dmem(dg)+2.0_r8*REAL(N(dg)*NpointsR(cr),r8)
        Dmem(dg)=Dmem(dg)+2.0_r8*REAL(N(dg)*NpointsU(cr),r8)
        Dmem(dg)=Dmem(dg)+2.0_r8*REAL(N(dg)*NpointsV(cr),r8)
      END DO
!
!  Initialize grid connectivity structure.
!
      DO cr=1,Ncontact
        dg=donor_grid(cr)
        rg=receiver_grid(cr)
        Rcontact(cr) % coincident = Lcoincident(rg)
        Ucontact(cr) % coincident = Lcoincident(rg)
        Vcontact(cr) % coincident = Lcoincident(rg)
        Rcontact(cr) % interpolate = Linterpolate(rg)
        Ucontact(cr) % interpolate = Linterpolate(rg)
        Vcontact(cr) % interpolate = Linterpolate(rg)
        Rcontact(cr) % donor_grid = dg
        Ucontact(cr) % donor_grid = dg
        Vcontact(cr) % donor_grid = dg
        Rcontact(cr) % receiver_grid = rg
        Ucontact(cr) % receiver_grid = rg
        Vcontact(cr) % receiver_grid = rg
        Rcontact(cr) % Npoints = NpointsR(cr)
        Ucontact(cr) % Npoints = NpointsU(cr)
        Vcontact(cr) % Npoints = NpointsV(cr)
        DO m=1,NpointsR(cr)
          ip=m+NstrR(cr)-1
          Rcontact(cr) % Irg(m) = Irg_cp(ip)
          Rcontact(cr) % Jrg(m) = Jrg_cp(ip)
          Rcontact(cr) % Idg(m) = Idg_cp(ip)
          Rcontact(cr) % Jdg(m) = Jdg_cp(ip)
          Rcontact(cr) % Lweight(1,m) = Lweight(1,ip)
          Rcontact(cr) % Lweight(2,m) = Lweight(2,ip)
          Rcontact(cr) % Lweight(3,m) = Lweight(3,ip)
          Rcontact(cr) % Lweight(4,m) = Lweight(4,ip)
        END DO
        DO m=1,NpointsU(cr)
          ip=m+NstrU(cr)-1
          Ucontact(cr) % Irg(m) = Irg_cp(ip)
          Ucontact(cr) % Jrg(m) = Jrg_cp(ip)
          Ucontact(cr) % Idg(m) = Idg_cp(ip)
          Ucontact(cr) % Jdg(m) = Jdg_cp(ip)
          Ucontact(cr) % Lweight(1,m) = Lweight(1,ip)
          Ucontact(cr) % Lweight(2,m) = Lweight(2,ip)
          Ucontact(cr) % Lweight(3,m) = Lweight(3,ip)
          Ucontact(cr) % Lweight(4,m) = Lweight(4,ip)
        END DO
        DO m=1,NpointsV(cr)
          ip=m+NstrV(cr)-1
          Vcontact(cr) % Irg(m) = Irg_cp(ip)
          Vcontact(cr) % Jrg(m) = Jrg_cp(ip)
          Vcontact(cr) % Idg(m) = Idg_cp(ip)
          Vcontact(cr) % Jdg(m) = Jdg_cp(ip)
          Vcontact(cr) % Lweight(1,m) = Lweight(1,ip)
          Vcontact(cr) % Lweight(2,m) = Lweight(2,ip)
          Vcontact(cr) % Lweight(3,m) = Lweight(3,ip)
          Vcontact(cr) % Lweight(4,m) = Lweight(4,ip)
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Load contact points grid metrics.
!-----------------------------------------------------------------------
!
!  Allocate contact region metrics (type T_NGM) structure.
!
      allocate ( CONTACT_METRIC(Ncontact) )
!
!  Allocate arrays in contact region metrics structure.
!
      DO cr=1,Ncontact
        allocate ( CONTACT_METRIC(cr) % angler(NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % dndx  (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % dmde  (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % f     (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % h     (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % rmask (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % umask (NpointsU(cr)) )
        allocate ( CONTACT_METRIC(cr) % vmask (NpointsV(cr)) )
        allocate ( CONTACT_METRIC(cr) % pm    (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % pn    (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % Xr    (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % Yr    (NpointsR(cr)) )
        allocate ( CONTACT_METRIC(cr) % Xu    (NpointsU(cr)) )
        allocate ( CONTACT_METRIC(cr) % Yu    (NpointsU(cr)) )
        allocate ( CONTACT_METRIC(cr) % Xv    (NpointsV(cr)) )
        allocate ( CONTACT_METRIC(cr) % Yv    (NpointsV(cr)) )
        Dmem(ng)=Dmem(ng)+10.0_r8*REAL(NpointsR(cr),r8)
        Dmem(ng)=Dmem(ng)+ 3.0_r8*REAL(NpointsU(cr),r8)
        Dmem(ng)=Dmem(ng)+ 3.0_r8*REAL(NpointsV(cr),r8)
      END DO
!
!  Initialize contact region metrics structure.
!
      DO cr=1,Ncontact
        DO m=1,NpointsR(cr)
          ip=m+NstrR(cr)-1
          CONTACT_METRIC(cr) % angler(m) = angle(ip)
          CONTACT_METRIC(cr) % dndx  (m) = dndx (ip)
          CONTACT_METRIC(cr) % dmde  (m) = dmde (ip)
          CONTACT_METRIC(cr) % f     (m) = f    (ip)
          CONTACT_METRIC(cr) % h     (m) = h    (ip)
          CONTACT_METRIC(cr) % rmask (m) = mask (ip)
          CONTACT_METRIC(cr) % pm    (m) = pm   (ip)
          CONTACT_METRIC(cr) % pn    (m) = pn   (ip)
          CONTACT_METRIC(cr) % Xr    (m) = Xrg  (ip)
          CONTACT_METRIC(cr) % Yr    (m) = Yrg  (ip)
        END DO
        DO m=1,NpointsU(cr)
          ip=m+NstrU(cr)-1
          CONTACT_METRIC(cr) % umask(m) = mask(ip)
          CONTACT_METRIC(cr) % Xu   (m) = Xrg (ip)
          CONTACT_METRIC(cr) % Yu   (m) = Yrg (ip)
        END DO
        DO m=1,NpointsV(cr)
          ip=m+NstrV(cr)-1
          CONTACT_METRIC(cr) % vmask(m) = mask(ip)
          CONTACT_METRIC(cr) % Xv   (m) = Xrg (ip)
          CONTACT_METRIC(cr) % Yv   (m) = Yrg (ip)
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Initialize various parameters.
!-----------------------------------------------------------------------
!
!  Nested grids conectivity switches.  They are used in "get_bounds.F"
!  to determenine if the global state array need to be extended by few
!  extra points at each boundary to accomodate the contact regions and
!  contact points.  These are VERY important switches in ROMS nesting.
!
      IF (.not.allocated(ContactRegion)) THEN
        allocate ( ContactRegion(4,Ngrids) )
        ContactRegion = .FALSE.
        Dmem(ng)=Dmem(ng)+4.0_r8*REAL(Ngrids,r8)
      END IF
!
      DO m=1,NCdatum
        cr=contact_region(m)
        rg=receiver_grid(cr)
        ibry=on_boundary(m)
        IF ((ibry.eq.iwest ).or.(ibry.eq.ieast ).or.                    &
     &      (ibry.eq.isouth).or.(ibry.eq.inorth)) THEN
          IF (.not.ContactRegion(ibry,rg)) THEN
            ContactRegion(ibry,rg)=.TRUE.
          END IF
        END IF
      END DO
!
!  Set composite and refinement grids switches.
!
      DO cr=1,Ncontact
        rg=receiver_grid(cr)
        DO ibry=1,4
          IF (Lcomposite(cr).and.ContactRegion(ibry,rg)) THEN
            CompositeGrid(ibry,rg)=.TRUE.
          END IF
        END DO
        RefinedGrid(rg)=Lrefinement(cr)
      END DO
!
!  Refinement grid scale factor.
!
      DO ig=1,Ngrids
        RefineScale(ig)=Refine_factor(ig)
      END DO
!
!  Coarser donor grid number to finer grid contact points.  Their
!  values are computed in "metrics.F" when the maximum grid spacing
!  is computed. The maximum grid spacing is used to distinguish
!  contact areas for more than two layers of nesting.
!
!$OMP PARALLEL
      IF (.not.allocated(CoarserDonor)) THEN
        allocate ( CoarserDonor(Ngrids) )
        CoarserDonor = 0
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
!$OMP END PARALLEL
!
!  Finer donor grid to coarser receiver grid for two-way feedback.
!  Their values are computed in "metrics.F" when the maximum grid
!  spacing is computed.
!
!$OMP PARALLEL
      IF (.not.allocated(FinerDonor)) THEN
        allocate ( FinerDonor(Ngrids) )
        FinerDonor = 0
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
!$OMP END PARALLEL
!
!  Logical switch indicating which coarser grid is a donor to a finer
!  grid external contact points. Their values are computed in
!  "metrics.F" after "CoarserDonor" is computed.
!
!$OMP PARALLEL
      IF (.not.allocated(DonorToFiner)) THEN
        allocate ( DonorToFiner(Ngrids) )
        DonorToFiner = .FALSE.
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
!$OMP END PARALLEL
!
!  Number of refined grid time-steps. Their values are initialized to
!  the optimal values based on the spatial refinement ratio.  These
!  values are over-written in "metrics.F" to use the time-step
!  size ration between donor and receiver grid.  The user is allowed
!  to take larger divisible time-step with respect to the donor grid.
!  The user is responsible to set the appropriate refined grid time-step
!  for stability.
!
!$OMP PARALLEL
      IF (.not.allocated(RefineSteps)) THEN
        allocate ( RefineSteps(Ngrids) )
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
        DO ig=1,Ngrids
          RefineSteps(ig)=Refine_factor(ig)
        END DO
      END IF
!$OMP END PARALLEL
!
!  Refine grid time-steps counter with respect the coarse grid (ng=1)
!  single time-step.
!
      IF (.not.allocated(RefineStepsCounter)) THEN
        allocate ( RefineStepsCounter(Ngrids) )
        RefineStepsCounter = 0
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
!
!  Interval used in the two-way exchange between fine and coarse grids.
!
      IF (.not.allocated(TwoWayInterval)) THEN
        allocate ( TwoWayInterval(Ngrids) )
        TwoWayInterval = 0.0_r8
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
!
!  Switch indicating which refined grid(s) include finer refined grids:
!  telescoping refinement.
!
!$OMP PARALLEL
      IF (.not.allocated(Telescoping)) THEN
        allocate ( Telescoping(Ngrids) )
        Telescoping = .FALSE.
        Dmem(ng)=Dmem(ng)+REAL(Ngrids,r8)
      END IF
!$OMP END PARALLEL
!
!  Rolling index and time (seconds) used in the temporal interpolation
!  of contact point data.
!
!$OMP PARALLEL
      IF (.not.allocated(RollingIndex)) THEN
        allocate ( RollingIndex(Ncontact) )
        RollingIndex = 0
        Dmem(ng)=Dmem(ng)+REAL(Ncontact,r8)
      END IF
      IF (.not.allocated(RollingTime)) THEN
        allocate ( RollingTime(2,Ncontact) )
        RollingTime = 0
        Dmem(ng)=Dmem(ng)+2.0_r8*REAL(Ncontact,r8)
      END IF
!$OMP END PARALLEL
!
!-----------------------------------------------------------------------
!  Allocate composite grids contact regions structure and arrays.
!-----------------------------------------------------------------------
!
      IF (ANY(CompositeGrid)) THEN
!
!  Allocate composite grid contact regions (type T_COMPOSITE) structure.
!
        allocate ( COMPOSITE(Ncontact) )
!
!  Allocate arrays in composite grids contact regions structure.
!
        DO cr=1,Ncontact
          dg=donor_grid(cr)
          allocate ( COMPOSITE(cr) % bustr(4,NpointsU(cr)) )
          allocate ( COMPOSITE(cr) % bvstr(4,NpointsV(cr)) )
          allocate ( COMPOSITE(cr) % ubar(4,NpointsU(cr),2) )
          allocate ( COMPOSITE(cr) % vbar(4,NpointsV(cr),2) )
          allocate ( COMPOSITE(cr) % zeta(4,NpointsR(cr),2) )
          allocate ( COMPOSITE(cr) % rzeta(4,NpointsR(cr)) )
          Dmem(dg)=Dmem(dg)+2.0_r8*REAL(4*NpointsR(cr),r8)
          Dmem(dg)=Dmem(dg)+2.0_r8*REAL(4*NpointsU(cr),r8)
          Dmem(dg)=Dmem(dg)+2.0_r8*REAL(4*NpointsV(cr),r8)
          allocate ( COMPOSITE(cr) % DU_avg1(4,NpointsU(cr)) )
          allocate ( COMPOSITE(cr) % DV_avg1(4,NpointsV(cr)) )
          allocate ( COMPOSITE(cr) % Zt_avg1(4,NpointsR(cr)) )
          Dmem(dg)=Dmem(dg)+REAL(4*NpointsR(cr),r8)
          Dmem(dg)=Dmem(dg)+REAL(4*NpointsU(cr),r8)
          Dmem(dg)=Dmem(dg)+REAL(4*NpointsV(cr),r8)
          allocate ( COMPOSITE(cr) % u(4,N(dg),NpointsU(cr)) )
          allocate ( COMPOSITE(cr) % v(4,N(dg),NpointsV(cr)) )
          allocate ( COMPOSITE(cr) % Huon(4,N(dg),NpointsU(cr)) )
          allocate ( COMPOSITE(cr) % Hvom(4,N(dg),NpointsV(cr)) )
          Dmem(dg)=Dmem(dg)+2.0_r8*REAL(4*N(dg)*NpointsU(cr),r8)
          Dmem(dg)=Dmem(dg)+2.0_r8*REAL(4*N(dg)*NpointsV(cr),r8)
          allocate ( COMPOSITE(cr) % t(4,N(dg),NpointsR(cr),NT(dg)) )
          Dmem(dg)=Dmem(dg)+2.0_r8*REAL(4*N(dg)*NpointsU(cr)*NT(dg),r8)
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Allocate refinement grids contact regions structure.
!-----------------------------------------------------------------------
!
      IF (ANY(RefinedGrid)) THEN
!
!  Allocate refinement grids contact region (type T_REFINED) structure.
!
        allocate ( REFINED(Ncontact) )
!
!  Allocate arrays in refinement grids contact region structure.
!
        DO cr=1,Ncontact
          rg=receiver_grid(cr)
          allocate ( REFINED(cr) % ubar(4,NpointsU(cr),2) )
          allocate ( REFINED(cr) % vbar(4,NpointsV(cr),2) )
          allocate ( REFINED(cr) % zeta(4,NpointsR(cr),2) )
          allocate ( REFINED(cr) % DU_avg2(4,NpointsU(cr),2) )
          allocate ( REFINED(cr) % DV_avg2(4,NpointsV(cr),2) )
          allocate ( REFINED(cr) % on_u(NpointsU(cr)) )
          allocate ( REFINED(cr) % om_v(NpointsV(cr)) )
          Dmem(rg)=Dmem(rg)+REAL(4*NpointsR(cr),r8)
          Dmem(rg)=Dmem(rg)+3.0_r8*REAL(4*NpointsU(cr),r8)
          Dmem(rg)=Dmem(rg)+3.0_r8*REAL(4*NpointsV(cr),r8)
          allocate ( REFINED(cr) % u(4,N(rg),NpointsU(cr),2) )
          allocate ( REFINED(cr) % v(4,N(rg),NpointsV(cr),2) )
          Dmem(rg)=Dmem(rg)+3.0_r8*REAL(4*N(rg)*NpointsU(cr),r8)
          Dmem(rg)=Dmem(rg)+3.0_r8*REAL(4*N(rg)*NpointsV(cr),r8)
          allocate ( REFINED(cr) % t(4,N(rg),NpointsR(cr),2,NT(rg)) )
          Dmem(rg)=Dmem(rg)+2.0_r8*REAL(4*N(rg)*NpointsR(cr)*NT(rg),r8)
        END DO
      END IF
      RETURN
      END SUBROUTINE set_contact
