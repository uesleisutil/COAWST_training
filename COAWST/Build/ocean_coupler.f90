      MODULE ocean_coupler_mod
!
!svn $Id: ocean_coupler.F 830 2017-01-24 21:21:11Z arango $
!==================================================== John C. Warner ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group      Hernan G. Arango   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module is used to communicate and exchange data between        !
!  ROMS/TOMS and other coupled model(s)  via the Model Coupling        !
!  Toolkit (MCT), developed at the Argonne National Laboratory.        !
!                                                                      !
!=======================================================================
!
!  Component Model Registry.
!
      USE m_MCTWorld, ONLY : MCTWorld_init => init
      USE m_MCTWorld, ONLY : MCTWorld_clean => clean
!
!  Domain Decomposition Descriptor DataType and associated methods.
!
      USE m_GlobalSegMap, ONLY : GlobalSegMap
      USE m_GlobalSegMap, ONLY : GlobalSegMap_init => init
      USE m_GlobalSegMap, ONLY : GlobalSegMap_lsize => lsize
      USE m_GlobalSegMap, ONLY : GlobalSegMap_clean => clean
      USE m_GlobalSegMap, ONLY : GlobalSegMap_Ordpnts => OrderedPoints
!
!  Field Storage DataType and associated methods.
!
      USE m_AttrVect, ONLY : AttrVect
      USE m_AttrVect, ONLY : AttrVect_init => init
      USE m_AttrVect, ONLY : AttrVect_zero => zero
      USE m_AttrVect, ONLY : AttrVect_lsize => lsize
      USE m_AttrVect, ONLY : AttrVect_clean => clean
      USE m_AttrVect, ONLY : AttrVect_copy => copy
      USE m_AttrVect, ONLY : AttrVect_importRAttr => importRAttr
      USE m_AttrVect, ONLY : AttrVect_exportRAttr => exportRAttr
!
!  Intercomponent communications scheduler.
!
      USE m_Router, ONLY : Router
      USE m_Router, ONLY : Router_init => init
      USE m_Router, ONLY : Router_clean => clean
!
!  Intercomponent transfer.
!
      USE m_Transfer, ONLY: MCT_send => send
      USE m_Transfer, ONLY: MCT_recv => recv
      USE m_Transfer, ONLY: MCT_isend => isend
      USE m_Transfer, ONLY: MCT_irecv => irecv
      USE m_Transfer, ONLY: MCT_waitr => waitrecv
      USE m_Transfer, ONLY: MCT_waits => waitsend
!
!  Sparse Matrix DataType and associated methods.
!
      USE m_SparseMatrix, ONLY : SparseMatrix
      USE m_SparseMatrix, ONLY : SparseMatrix_init => init
      USE m_SparseMatrix, ONLY : SparseMatrix_importGRowInd =>          &
     &                           importGlobalRowIndices
      USE m_SparseMatrix, ONLY : SparseMatrix_importGColInd =>          &
     &                           importGlobalColumnIndices
      USE m_SparseMatrix, ONLY : SparseMatrix_importMatrixElts =>       &
     &                           importMatrixElements
      USE m_SparseMatrix, only : SparseMatrix_lsize => lsize
      USE m_SparseMatrix, only : SparseMatrix_GNumElem =>               &
     &                           GlobalNumElements
      USE m_SparseMatrix, only : SparseMatrix_clean => clean
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_init => init
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_clean => clean
!
!  Decompose matrix by row.
!
      USE m_SparseMatrixPlus, ONLY : Xonly
!     USE m_SparseMatrixPlus, ONLY : Yonly
!
!  Matrix-Vector multiply methods.
!
      USE m_MatAttrVectMul, ONLY : MCT_MatVecMul => sMatAvMult
!
      implicit none
!
      PRIVATE
      PUBLIC :: ocean_coupling
      PUBLIC :: initialize_ocn2atm_coupling
      PUBLIC :: initialize_ocn2atm_routers
      PUBLIC :: ocn2atm_coupling
      PUBLIC :: ocnfatm_coupling
      PUBLIC :: finalize_ocn2atm_coupling
!
!  Declarations.
!
      TYPE T_GlobalSegMap_G
        TYPE(GlobalSegMap) :: GSMapROMS       ! GloabalSegMap variables
      END TYPE T_GlobalSegMap_G
      TYPE (T_GlobalSegMap_G), ALLOCATABLE :: GlobalSegMap_G(:)
      TYPE T_GSMapInterp_A
        TYPE(GlobalSegMap) :: GSMapWRF        ! GloabalSegMap variables
      END TYPE T_GSMapInterp_A
      TYPE (T_GSMapInterp_A), ALLOCATABLE :: GSMapInterp_A(:,:)
      TYPE T_AttrVect_G
      TYPE(AttrVect) :: atm2ocn_AV            ! AttrVect variables
      TYPE(AttrVect) :: ocn2atm_AV 
      END TYPE T_AttrVect_G
      TYPE (T_AttrVect_G), ALLOCATABLE :: AttrVect_G(:)
      TYPE T_Router_A
        TYPE(Router)   :: ROMStoWRF           ! Router variables
      END TYPE T_Router_A
      TYPE (T_Router_A), ALLOCATABLE :: Router_A(:,:)
      TYPE T_AV2_A
        TYPE(AttrVect) :: atm2ocn_AV2           ! AttrVect variables
        TYPE(AttrVect) :: ocn2atm_AV2
      END TYPE T_AV2_A
      TYPE (T_AV2_A), ALLOCATABLE :: AV2_A(:,:)
      TYPE(SparseMatrix) :: sMatO             ! Sparse matrix elements
      TYPE(SparseMatrix) :: sMatA             ! Sparse matrix elements
      TYPE T_SMPlus_A
        TYPE(SparseMatrixPlus) :: A2OMatPlus  ! Sparse matrix plus elements
        TYPE(SparseMatrixPlus) :: O2AMatPlus  ! Sparse matrix plus elements
      END TYPE T_SMPlus_A
      TYPE (T_SMPlus_A), ALLOCATABLE :: SMPlus_A(:,:)
      CONTAINS
      SUBROUTINE initialize_ocn2atm_coupling (ng, tile)
!
!=======================================================================
!                                                                      !
!  Initialize ocean and atmosphere models coupling stream. This is     !
!  the training phase used to constuct  MCT parallel interpolators     !
!  and stablish communication patterns.                                !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mct_coupler_params
      USE mod_kinds
      USE mod_scalars
      USE mod_iounits
      USE mod_coupler_iounits
!
!  Imported variable definitions.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Istr, Iend, Jstr, Jend
      integer :: IstrT, IendT, JstrT, JendT
      integer :: IstrR, IendR, JstrR, JendR, IstrU, JstrV
      integer :: Asize, Isize, Jsize, MyError
      integer :: i, ic, ia, j, jc, nprocs, cid, cad
      integer :: nRows, nCols, num_sparse_elems
      real(r8) :: cff
      integer, allocatable  :: length(:)
      integer, allocatable  :: start(:)
      character (len=70) :: nc_name
      character (len=20) :: to_add
      character (len=120) :: aostring
      character (len=120) :: oastring
!
!-----------------------------------------------------------------------
!  Compute lower and upper bounds over a particular domain partition or
!  tile for RHO-, U-, and V-variables. Notice that "set_bounds.h" is
!  not used here because of implementation of periodicity in other
!  models.
!-----------------------------------------------------------------------
!
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
      IstrT=BOUNDS(ng)%IstrT(tile)
      IendT=BOUNDS(ng)%IendT(tile)
      JstrT=BOUNDS(ng)%JstrT(tile)
      JendT=BOUNDS(ng)%JendT(tile)
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
        IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
        IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
        JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
        JendR=BOUNDS(ng)%Jend(tile)
      END IF
!
!-----------------------------------------------------------------------
!  Begin initialization phase.
!-----------------------------------------------------------------------
!
!  Get communicator local rank and size.
!
      CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)
      CALL mpi_comm_size (OCN_COMM_WORLD, nprocs, MyError)
!
      IF (ng.eq.1) THEN
        ALLOCATE(SMPlus_A(Nocn_grids,Natm_grids))
        ALLOCATE(AV2_A(Nocn_grids,Natm_grids))
        ALLOCATE(GSMapInterp_A(Nocn_grids,Natm_grids))
      END IF
      OCNid=ocnids(ng)
      IF (ng.eq.1) THEN
        ALLOCATE(GlobalSegMap_G(Nocn_grids))
        ALLOCATE(AttrVect_G(Nocn_grids))
      END IF
!
!  Initialize MCT coupled model registry.
!
      IF (Nocn_grids.gt.1) THEN
        CALL MCTWorld_init (N_mctmodels, MPI_COMM_WORLD,                &
     &                      OCN_COMM_WORLD,myids=ocnids)
      ELSE
        CALL MCTWorld_init (N_mctmodels, MPI_COMM_WORLD,                &
     &                      OCN_COMM_WORLD,OCNid)
      END IF
!
!  Determine start and lengths for domain decomposition.
!
      Jsize=JendR-JstrR+1
      IF (.not.allocated(start)) THEN
        allocate ( start(Jsize) )
      END IF
      IF (.not.allocated(length)) THEN
        allocate ( length(Jsize) )
      END IF
      jc=0
      DO j=JstrR,JendR
        jc=jc+1
        start (jc)=j*(Lm(ng)+2)+IstrR+1
        length(jc)=(IendR-IstrR+1)
      END DO
      CALL GlobalSegMap_init (GlobalSegMap_G(ng)%GSMapROMS,             &
     &                        start, length, 0, OCN_COMM_WORLD, OCNid)
!
!  Deallocate working arrays.
!
      IF (allocated(start)) THEN
        deallocate (start)
      END IF
      IF (allocated(length)) THEN
        deallocate (length)
      END IF
!
!  If ocean grid and atm grids are different sizes, then
!  develop sparse matrices for interpolation.
!
  35  FORMAT(a3,i1,a7,i1,a11)
      DO ia=1,Natm_grids
!
! First work on atm to ocean.
!
        IF (Myrank.eq.MyMaster) THEN
          IF (scrip_opt.eq.1) THEN
            write(nc_name,35) 'atm',ia,'_to_ocn',ng,'_weights.nc'
          ELSE
            nc_name=A2Oname(ia,ng)
          END IF
          call get_sparse_matrix (ng, nc_name, num_sparse_elems,        &
     &                            src_grid_dims, dst_grid_dims)
!
! Init the sparse matrix.
!
          nRows=dst_grid_dims(1)*dst_grid_dims(2)
          nCols=src_grid_dims(1)*src_grid_dims(2)
!
! Create sparse matrix.
!
!         Sparse rows is the dst address. Multiply the interp weights
!         by the dst masking.
!
          DO i=1,num_sparse_elems
            j=sparse_rows(i)
            cff=REAL(dst_grid_imask(j),r8)
            sparse_weights(i)=sparse_weights(i)*cff
          END DO
!
! Load the dst grid as a coupling mask.
!
          allocate(A2O_CPLMASK(ia,ng)%dst_mask(nRows))
          DO i=1,nRows
            A2O_CPLMASK(ia,ng)%dst_mask(i)=dst_grid_imask(i)
          END DO
          call SparseMatrix_init(sMatA,nRows,nCols,num_sparse_elems)
          call SparseMatrix_importGRowInd(sMatA, sparse_rows,           &
     &                                    num_sparse_elems)
          call SparseMatrix_importGColInd(sMatA, sparse_cols,           &
     &                                    num_sparse_elems)
          call SparseMatrix_importMatrixElts(sMatA, sparse_weights,     &
     &                                    num_sparse_elems)
!
! Deallocate arrays.
!
          deallocate ( sparse_rows )
          deallocate ( sparse_cols )
          deallocate ( sparse_weights )
          deallocate ( dst_grid_imask )
        END IF
!
!
        CALL mpi_bcast(dst_grid_dims, 2, MPI_INTEGER, MyMaster,         &
     &                 OCN_COMM_WORLD, MyError)
!
! scatter dst_grid_imask to be used as cpl_mask
!
        IF (Myrank.ne.MyMaster) THEN
          nRows=dst_grid_dims(1)*dst_grid_dims(2)
          allocate(A2O_CPLMASK(ia,ng)%dst_mask(nRows))
        END IF
        CALL mpi_bcast(A2O_CPLMASK(ia,ng)%dst_mask,nRows,               &
     &                 MPI_INTEGER, MyMaster,                           &
     &                 OCN_COMM_WORLD, MyError)
! Second work on ocean to atm.
!
        IF (Myrank.eq.MyMaster) THEN
          IF (scrip_opt.eq.1) THEN
            write(nc_name,35) 'ocn',ng,'_to_atm',ia,'_weights.nc'
          ELSE
            nc_name=O2Aname(ng,ia)
          END IF
          call get_sparse_matrix (ng, nc_name, num_sparse_elems,        &
     &                            src_grid_dims, dst_grid_dims)
!
! Init the sparse matrix.
!
          nRows=dst_grid_dims(1)*dst_grid_dims(2)
          nCols=src_grid_dims(1)*src_grid_dims(2)
!
! Create sparse matrix.
!
          DO i=1,num_sparse_elems
            j=sparse_rows(i)
            cff=REAL(dst_grid_imask(j),r8)
            sparse_weights(i)=sparse_weights(i)*cff
          END DO
!
! Load the dst grid as a coupling mask.
!
          allocate(O2A_CPLMASK(ng,ia)%dst_mask(nRows))
          DO i=1,nRows
            O2A_CPLMASK(ng,ia)%dst_mask(i)=dst_grid_imask(i)
          END DO
!
          call SparseMatrix_init(sMatO,nRows,nCols,num_sparse_elems)
          call SparseMatrix_importGRowInd(sMatO, sparse_rows,           &
     &                                    num_sparse_elems)
          call SparseMatrix_importGColInd(sMatO, sparse_cols,           &
     &                                    num_sparse_elems)
          call SparseMatrix_importMatrixElts(sMatO, sparse_weights,     &
     &                                    num_sparse_elems)
!
! Deallocate arrays.
!
          deallocate ( sparse_rows )
          deallocate ( sparse_cols )
          deallocate ( sparse_weights )
          deallocate ( dst_grid_imask )
        END IF
!
        CALL mpi_bcast(dst_grid_dims, 2, MPI_INTEGER, MyMaster,         &
     &                 OCN_COMM_WORLD, MyError)
!
! scatter dst_grid_imask to be used as cpl_mask
!
        IF (Myrank.ne.MyMaster) THEN
          nRows=dst_grid_dims(1)*dst_grid_dims(2)
          allocate(O2A_CPLMASK(ng,ia)%dst_mask(nRows))
        END IF
        CALL mpi_bcast(O2A_CPLMASK(ng,ia)%dst_mask,nRows,               &
     &                 MPI_INTEGER, MyMaster,                           &
     &                 OCN_COMM_WORLD, MyError)
!
!  Create Global Seg Map for atm model.
!  Determine start and lengths for domain decomposition
!  of the atm model.
!
        Isize=INT(dst_grid_dims(1)/nprocs)
        IF (MyRank.eq.nprocs-1) THEN
          Isize=dst_grid_dims(1)-Isize*(nprocs-1)
        ENDIF
        IF (.not.allocated(start)) THEN
          allocate ( start(1) )
        END IF
        IF (.not.allocated(length)) THEN
          allocate ( length(1) )
        END IF
        start=(MyRank*INT(dst_grid_dims(1)/nprocs))*dst_grid_dims(2)+1
        length=Isize*dst_grid_dims(2)
!
        CALL GlobalSegMap_init (GSMapInterp_A(ng,ia)%GSMapWRF,          &
     &                          start, length, 0, OCN_COMM_WORLD, OCNid)
!
!  Deallocate working arrays.
!
        IF (allocated(start)) THEN
          deallocate (start)
        END IF
        IF (allocated(length)) THEN
          deallocate (length)
        END IF
!
! Create ATM sparse matrix plus for interpolation.
! Specify matrix decomposition to be by row.
!
        call SparseMatrixPlus_init(SMPlus_A(ng,ia)%A2OMatPlus, sMatA,   &
     &                             GSMapInterp_A(ng,ia)%GSMapWRF,       &
     &                             GlobalSegMap_G(ng)%GSMapROMS,        &
     &                             Xonly,MyMaster,OCN_COMM_WORLD, OCNid)
        call SparseMatrix_clean(sMatA)
!
! Create Ocean sparse matrix plus for interpolation.
! Specify matrix decomposition to be by row.
!
        call SparseMatrixPlus_init(SMPlus_A(ng,ia)%O2AMatPlus, sMatO,   &
     &                             GlobalSegMap_G(ng)%GSMapROMS,        &
     &                             GSMapInterp_A(ng,ia)%GSMapWRF,       &
     &                             Xonly,MyMaster,OCN_COMM_WORLD, OCNid)
        call SparseMatrix_clean(sMatO)
      END DO
!
!  Initialize attribute vector holding the data code strings from
!  the atmosphere model.
!
      cad=LEN(aostring)
      DO i=1,cad
        aostring(i:i)=''
      END DO
      cid=1
!
      to_add='GSW'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':GLW'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':LH'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':HFX'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':USTRESS'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':VSTRESS'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':MSLP'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':RELH'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':T2'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
!
!
      to_add=':RAIN'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':EVAP'
      cad=LEN_TRIM(to_add)
      write(aostring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
!  Finalize and remove trailing spaces from the aostring
!  for the rlist.
!
      cad=LEN_TRIM(aostring)
      aostring=aostring(1:cad)
!
!  Initialize attribute vector holding the export data code strings of
!  the atmosphere model. The Asize is the number of grid point on this
!  processor.
!
      Asize=GlobalSegMap_lsize(GlobalSegMap_G(ng)%GSMapROMS,            &
     &                         OCN_COMM_WORLD)
      CALL AttrVect_init(AttrVect_G(ng)%atm2ocn_AV,                     &
     &                   rList=TRIM(aostring),lsize=Asize)
      CALL AttrVect_zero(AttrVect_G(ng)%atm2ocn_AV)
!
!  Initialize attribute vector holding the export data of
!  the atm model.
!
!
!  Initialize attribute vector that contain the data strings from
!  the ocean model.
!
      cad=LEN(oastring)
      DO i=1,cad
        oastring(i:i)=''
      END DO
      cid=1
!
      to_add='SST'
      cad=LEN_TRIM(to_add)
      write(oastring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
      to_add=':CPL_MASK'
      cad=LEN_TRIM(to_add)
      write(oastring(cid:cid+cad-1),'(a)') to_add(1:cad)
      cid=cid+cad
!
!  Finalize and remove trailing spaces from the oastring
!  for the rlist.
!
      cad=LEN_TRIM(oastring)
      oastring=oastring(1:cad)
!
      CALL AttrVect_init (AttrVect_G(ng)%ocn2atm_AV,                    &
     &                    rList=TRIM(oastring),lsize=Asize)
      CALL AttrVect_zero (AttrVect_G(ng)%ocn2atm_AV)
!
      DO ia=1,Natm_grids
        ATMid=atmids(ia)
!
!  Initialize attribute vector holding the export data of
!  the atm model. The Asize is the number of grid point on this
!  processor.
!
        Asize=GlobalSegMap_lsize(GSMapInterp_A(ng,ia)%GSMapWRF,         &
     &                           OCN_COMM_WORLD)
        CALL AttrVect_init (AV2_A(ng,ia)%atm2ocn_AV2,                   &
     &                      rList=TRIM(aostring),lsize=Asize)
        CALL AttrVect_zero (AV2_A(ng,ia)%atm2ocn_AV2)
!
!  Initialize attribute vector holding the export data code string of
!  the ocean model.
!
        CALL AttrVect_init (AV2_A(ng,ia)%ocn2atm_AV2,                   &
     &                      rList=TRIM(oastring),lsize=Asize)
        CALL AttrVect_zero (AV2_A(ng,ia)%ocn2atm_AV2)
      END DO
      RETURN
      END SUBROUTINE initialize_ocn2atm_coupling
      SUBROUTINE initialize_ocn2atm_routers (tile)
!
!=======================================================================
!                                                                      !
!  Initialize ocean and atm models coupling stream.  This is the      !
!  training phase used to constuct MCT parallel interpolators and      !
!  and stablish communication patterns.                                !
!                                                                      !
!=======================================================================
!
      USE mod_parallel
      USE mct_coupler_params
!
!  Imported variable definitions.
!
      integer, intent(in) :: tile
!
!  Local variable declarations.
!
      integer :: MyError, nprocs
      integer :: ng, ia
!
!-----------------------------------------------------------------------
!  Establish MCT router.
!-----------------------------------------------------------------------
!
      ALLOCATE(Router_A(Nocn_grids,Natm_grids))
!
!  Initialize routers to the wave model component.
!
      DO ng=1,Nocn_grids
        DO ia=1,Natm_grids
          ATMid=atmids(ia)
          CALL Router_init (ATMid, GSMapInterp_A(ng,ia)%GSMapWRF,       &
     &                      OCN_COMM_WORLD, Router_A(ng,ia)%ROMStoWRF)
        END DO
      END DO
      RETURN
      END SUBROUTINE initialize_ocn2atm_routers
      SUBROUTINE ocn2atm_coupling (ng, ia, tile)
!
!=======================================================================
!                                                                      !
!  This subroutine acquires the coupling data streams from the ocean   !
!  to the atmosphere model. Currently, the following data streams are  !
!  coded:                                                              !
!                                                                      !
!     (...) WRF  units                                                 !
!     [...] ROMS units                                                 !
!                                                                      !
!  Fields imported WRF model:                                          !
!                                                                      !
!     * GSW     Net shortwave radiation (Watts/m2), [Celsius m/s]      !
!     * GLW     Long wave raditaion (Watts/m2), [Celsius m/s]          !
!     * LH      Latent heat flux (Watts/m2), [Celsius m/s]             !
!     * HFX     Sensible heat flux (Watts/m2), [Celsius m/s]           !
!     * USTRESS Surface U-wind stress (Pa), [m2/s2]                    !
!     * VSTRESS Surface V-wind stress (Pa), [m2/s2]                    !
!     * MSLP    Mean Sea Level Pressure (Pa), [mb]                     !
!     * RELH    Surface air relative humidity (percent), [fraction]    !
!     * T2      Surface (2 m) air temperature (Celsius), [Celsius]     !
!     * U10     Surface (10 m) U-wind speed (m/s), [m/s]               !
!     * V10     Surface (10 m) V-wind speed (m/s), [m/s]               !
!     * CLDFRA  Cloud fraction (percent/100), [percent/100]            !
!     * RAIN    Precipitation (m/s), [kg/m2/s]                         !
!     * EVAP    Evaporation (m/s), [kg/m2/s]                           !
!                                                                      !
!  Fields exported to WRF model:                                       !
!                                                                      !
!     * SST     Sea surface potential temperature (Kelvin), [Celsius]  !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, ia, tile
!
!  Local variable declarations.
!
      integer :: IminS, ImaxS, JminS, JmaxS
      integer :: LBi, UBi, LBj, UBj, LBij, UBij
!
!  Set horizontal starting and ending indices for automatic private
!  storage arrays.
!
      IminS=BOUNDS(ng)%Istr(tile)-4
      ImaxS=BOUNDS(ng)%Iend(tile)+3
      JminS=BOUNDS(ng)%Jstr(tile)-4
      JmaxS=BOUNDS(ng)%Jend(tile)+3
!
!  Determine array lower and upper bounds in the I- and J-directions.
!
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)
!
!  Set array lower and upper bounds for MIN(I,J) directions and
!  MAX(I,J) directions.
!
      LBij=BOUNDS(ng)%LBij
      UBij=BOUNDS(ng)%UBij
!
      CALL wclock_on (ng, iNLM, 48)
      CALL ocn2atm_coupling_tile (ng, ia, tile,                         &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS)
      CALL wclock_off (ng, iNLM, 48)
      RETURN
      END SUBROUTINE ocn2atm_coupling
!
!***********************************************************************
      SUBROUTINE ocn2atm_coupling_tile (ng, ia, tile,                   &
     &                                  LBi, UBi, LBj, UBj,             &
     &                                  IminS, ImaxS, JminS, JmaxS)
!***********************************************************************
!
      USE mct_coupler_params
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_forces
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
      USE mod_iounits
      USE mod_grid
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
      USE exchange_2d_mod, ONLY : exchange_u2d_tile
      USE exchange_2d_mod, ONLY : exchange_v2d_tile
      USE mp_exchange_mod, ONLY : mp_exchange2d
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, ia, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
!  Local variable declarations.
!
      integer :: Asize, Iimport, Iexport, MyError, Tag
      integer :: gtype, i, id, ifield, j, ij,  status
      real(r8) :: add_offset, cff, scale
      real(r8) :: RecvTime, SendTime, buffer(2), wtime(2)
      real(r8), pointer :: A(:)
      integer, pointer :: points(:)
      integer, pointer :: indices(:)
      real(r8), pointer :: Amask(:)
      real(r8) :: BBR, cff1, cff2
      character (len=40) :: code
!
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
      integer :: IminR, ImaxR, JminR, JmaxR
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      IminR  =BOUNDS(ng) % IstrR  (-1)
      ImaxR  =BOUNDS(ng) % IendR  (-1)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
      JminR  =BOUNDS(ng) % JstrR  (-1)
      JmaxR  =BOUNDS(ng) % JendR  (-1)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
!-----------------------------------------------------------------------
!  Allocate communications array.
!-----------------------------------------------------------------------
!
      Asize=GlobalSegMap_lsize (GlobalSegMap_G(ng)%GSMapROMS,           &
     &                          OCN_COMM_WORLD)
      allocate ( A(Asize) )
      A=0.0_r8
!
!-----------------------------------------------------------------------
!  Export fields from ocean (ROMS) to atmosphere (WRF) model.
!-----------------------------------------------------------------------
!
!  Sea surface temperature       (degC)
!
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          A(ij)=OCEAN(ng)%t(i,j,N(ng),nstp(ng),itemp)
        END DO
      END DO
      CALL AttrVect_importRAttr (AttrVect_G(ng)%ocn2atm_AV, "SST", A,   &
     &                           Asize)
!
!  Send ocean fields to atmosphere model.
!
      MyError=0
      Tag=ng*100+ia*10+0
!
      CALL MCT_MatVecMul(AttrVect_G(ng)%ocn2atm_AV,                     &
     &                   SMPlus_A(ng,ia)%O2AMatPlus,                    &
     &                   AV2_A(ng,ia)%ocn2atm_AV2)
!
!  Now add in the CPL_MASK before we send it over to wrf.
!  Get the number of grid points on this processor.
!
      Asize=GlobalSegMap_lsize (GSMapInterp_A(ng,ia)%GSMapWRF,          &
     &                          OCN_COMM_WORLD)
      allocate (Amask(Asize))
      Amask=0.0_r8
!
!  Ask for points in this tile.
!
      CALL GlobalSegMap_Ordpnts (GSMapInterp_A(ng,ia)%GSMapWRF,          &
     &                           MyRank, points)
!
!  Load the dst grid cpl mask into the attr vect.
!
      DO i=1,Asize
        Amask(i)=REAL(O2A_CPLMASK(ng,ia)%dst_mask(points(i)))
      END DO
      CALL AttrVect_importRAttr (AV2_A(ng,ia)%ocn2atm_AV2, "CPL_MASK",  &
     &                           Amask, Asize)
!
      CALL MCT_isend (AV2_A(ng,ia)%ocn2atm_AV2,                         &
     &                Router_A(ng,ia)%ROMStoWRF, Tag)
      CALL MCT_waits (Router_A(ng,ia)%ROMStoWRF)
      IF (MyError.ne.0) THEN
        IF (Master) THEN
          WRITE (stdout,20) 'atmosphere model, MyError = ', MyError
        END IF
        exit_flag=2
        RETURN
      ELSE
        IF (Master) THEN
          WRITE (stdout,37) ' ## ROMS grid ',ng,                        &
     &                      ' sent data to WRF grid ',ia
 37       FORMAT (a14,i2,a24,i2)
        END IF
      END IF
!
!  Deallocate communication arrays.
!
      deallocate (A)
      deallocate (points, Amask)
      if (associated (indices)) then
        deallocate (indices)
      endif
!
 10   FORMAT (' OCN2ATM_COUPLING - error while receiving fields from ', &
     &        a, i4)
 20   FORMAT (' OCN2ATM_COUPLING - error while sending fields to ',     &
     &        a, i4)
      RETURN
      END SUBROUTINE ocn2atm_coupling_tile
      SUBROUTINE ocnfatm_coupling (ng, ia, tile)
!
!=======================================================================
!                                                                      !
!  This subroutine acquires the coupling data streams between ocean    !
!  and atmosphere models. Currently, the following data streams are    !
!  coded:                                                              !
!                                                                      !
!     (...) WRF  units                                                 !
!     [...] ROMS units                                                 !
!                                                                      !
!  Fields imported WRF model:                                          !
!                                                                      !
!     * GSW     Net shortwave radiation (Watts/m2), [Celsius m/s]      !
!     * GLW     Long wave raditaion (Watts/m2), [Celsius m/s]          !
!     * LH      Latent heat flux (Watts/m2), [Celsius m/s]             !
!     * HFX     Sensible heat flux (Watts/m2), [Celsius m/s]           !
!     * USTRESS Surface U-wind stress (Pa), [m2/s2]                    !
!     * VSTRESS Surface V-wind stress (Pa), [m2/s2]                    !
!     * MSLP    Mean Sea Level Pressure (Pa), [mb]                     !
!     * RELH    Surface air relative humidity (percent), [fraction]    !
!     * T2      Surface (2 m) air temperature (Celsius), [Celsius]     !
!     * U10     Surface (10 m) U-wind speed (m/s), [m/s]               !
!     * V10     Surface (10 m) V-wind speed (m/s), [m/s]               !
!     * CLDFRA  Cloud fraction (percent/100), [percent/100]            !
!     * RAIN    Precipitation (m/s), [kg/m2/s]                         !
!     * EVAP    Evaporation (m/s), [kg/m2/s]                           !
!                                                                      !
!  Fields exported to WRF model:                                       !
!                                                                      !
!     * SST     Sea surface potential temperature (Kelvin), [Celsius]  !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, ia, tile
!
!  Local variable declarations.
!
      integer :: IminS, ImaxS, JminS, JmaxS
      integer :: LBi, UBi, LBj, UBj, LBij, UBij
!
!  Set horizontal starting and ending indices for automatic private
!  storage arrays.
!
      IminS=BOUNDS(ng)%Istr(tile)-4
      ImaxS=BOUNDS(ng)%Iend(tile)+3
      JminS=BOUNDS(ng)%Jstr(tile)-4
      JmaxS=BOUNDS(ng)%Jend(tile)+3
!
!  Determine array lower and upper bounds in the I- and J-directions.
!
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)
!
!  Set array lower and upper bounds for MIN(I,J) directions and
!  MAX(I,J) directions.
!
      LBij=BOUNDS(ng)%LBij
      UBij=BOUNDS(ng)%UBij
!
      CALL wclock_on (ng, iNLM, 48)
      CALL ocnfatm_coupling_tile (ng, ia, tile,                         &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS)
      CALL wclock_off (ng, iNLM, 48)
      RETURN
      END SUBROUTINE ocnfatm_coupling
!
!***********************************************************************
      SUBROUTINE ocnfatm_coupling_tile (ng, ia, tile,                   &
     &                                  LBi, UBi, LBj, UBj,             &
     &                                  IminS, ImaxS, JminS, JmaxS)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_forces
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
      USE mod_iounits
      USE mct_coupler_params
      USE mod_grid
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
      USE exchange_2d_mod, ONLY : exchange_u2d_tile
      USE exchange_2d_mod, ONLY : exchange_v2d_tile
      USE distribute_mod,  ONLY : mp_reduce
      USE mp_exchange_mod, ONLY : mp_exchange2d
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, ia, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
!  Local variable declarations.
!
      integer :: Asize, Iimport, Iexport, MyError, Tag
      integer :: gtype, i, id, ifield, j, ij,  status
      integer, pointer :: points(:)
      integer, pointer :: indices(:)
      real(r8) :: add_offset, cff, fac, scale
      real(r8) :: RecvTime, SendTime, buffer(2), wtime(2)
      real(r8) :: BBR, cff1, cff2
!     real(r8), parameter ::  Large = 1.0E+20_r8
      real(r8), pointer :: A(:)
      real(r8), dimension(2) :: range
      character (len=40) :: code
      character (len=3), dimension(2) :: op_handle
!
!-----------------------------------------------------------------------
!  Compute lower and upper bounds over a particular domain partition or
!  tile for RHO-, U-, and V-variables. Notice that "set_bounds.h" is
!  not used here because of implementation of periodicity in other
!  models.
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
      integer :: IminR, ImaxR, JminR, JmaxR
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      IminR  =BOUNDS(ng) % IstrR  (-1)
      ImaxR  =BOUNDS(ng) % IendR  (-1)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
      JminR  =BOUNDS(ng) % JstrR  (-1)
      JmaxR  =BOUNDS(ng) % JendR  (-1)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
      op_handle(1)='MIN'
      op_handle(2)='MAX'
!
!-----------------------------------------------------------------------
!  Allocate communications array.
!-----------------------------------------------------------------------
!
      Asize=GlobalSegMap_lsize (GlobalSegMap_G(ng)%GSMapROMS,           &
     &                          OCN_COMM_WORLD)
      allocate ( A(Asize) )
      A=0.0_r8
!
!  Ask for points in this tile.
!
      CALL GlobalSegMap_Ordpnts (GlobalSegMap_G(ng)%GSMapROMS,          &
     &                           MyRank, points)
!
!-----------------------------------------------------------------------
!  Import fields from atmosphere model (WRF) to ocean model (ROMS).
!-----------------------------------------------------------------------
!
!  Receive fields from atmosphere model.
!
      MyError=0
      CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)
      Tag=ng*100+ia*10+0
      CALL MCT_irecv (AV2_A(ng,ia)%atm2ocn_AV2,                         &
     &                Router_A(ng,ia)%ROMStoWRF, Tag)
!     Wait to make sure the WRF data has arrived.
      CALL MCT_waitr (AV2_A(ng,ia)%atm2ocn_AV2,                         &
     &                Router_A(ng,ia)%ROMStoWRF)
      CALL MCT_MatVecMul(AV2_A(ng,ia)%atm2ocn_AV2,                      &
     &                   SMPlus_A(ng,ia)%A2OMatPlus,                    &
     &                   AttrVect_G(ng)%atm2ocn_AV)
      IF (MyError.ne.0) THEN
        IF (Master) THEN
          WRITE (stdout,10) 'atmosphere model, MyError = ', MyError
        END IF
        CALL finalize_ocn2atm_coupling
      ELSE
        IF (Master) THEN
          WRITE (stdout,38) ' ## ROMS grid ',ng,                        &
     &                      ' recv data from WRF grid ',ia
 38       FORMAT (a14,i2,a25,i2)
        END IF
      END IF
!
!  Receive fields from atmosphere model.
 40         FORMAT (a36,1x,2(1pe14.6))
!
!  Short wave radiation          (from W/m^2 to Celsius m/s)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "GSW",      &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=1.0_r8/(rho0*Cp)
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)
          IF (ia.eq.1) THEN
            FORCES(ng)%srflx(i,j)=cff*fac
          ELSE
            FORCES(ng)%srflx(i,j)=FORCES(ng)%srflx(i,j)+cff*fac
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  GSW     (Wm-2):  ',        &
     &                    range(1),range(2)
      END IF
!
!  Long wave radiation          (from W/m^2 to Celsius m/s)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "GLW",      &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=1.0_r8/(rho0*Cp)
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
!         BBR=OCEAN(ng)%t(i,j,N(ng),nstp(ng),itemp)+273.16_r8
!         BBR=BBR*BBR*BBR*BBR
!         BBR=0.97_r8*5.67E-8_r8*BBR
!         A(ij)=A(ij)-BBR
          cff=A(ij)*REAL(A2O_CPLMASK(ia,ng)%dst_mask(points(ij)))
          IF (ia.eq.1) THEN
            FORCES(ng)%lrflx(i,j)=cff*fac
          ELSE
            FORCES(ng)%lrflx(i,j)=FORCES(ng)%lrflx(i,j)+cff*fac
          END IF
          IF (ia.eq.Natm_grids) THEN
            BBR=OCEAN(ng)%t(i,j,N(ng),nstp(ng),itemp)+273.16_r8
            BBR=BBR*BBR*BBR*BBR
            BBR=0.97_r8*5.67E-8_r8*BBR
            FORCES(ng)%lrflx(i,j)=FORCES(ng)%lrflx(i,j)-BBR*fac
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  GLW     (Wm-2):  ',        &
     &                    range(1),range(2)
      END IF
!
!  Latent heat flux            (from W/m^2 to Celsius m/s)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "LH",       &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=-1.0_r8/(rho0*Cp)
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)
          IF (ia.eq.1) THEN
            FORCES(ng)%lhflx(i,j)=cff*fac
          ELSE
            FORCES(ng)%lhflx(i,j)=FORCES(ng)%lhflx(i,j)+cff*fac
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  LH      (Wm-2):  ',        &
     &                    range(1),range(2)
      END IF
!
!  Sensible heat flux            (from W/m^2 to Celsius m/s)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "HFX",      &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=-1.0_r8/(rho0*Cp)
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)
          IF (ia.eq.1) THEN
            FORCES(ng)%shflx(i,j)=cff*fac
          ELSE
            FORCES(ng)%shflx(i,j)=FORCES(ng)%shflx(i,j)+cff*fac
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  HFX     (Wm-2):  ',        &
     &                    range(1),range(2)
      END IF
!
!  Surface u-stress              (m2/s2)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "USTRESS",  &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=1.0_r8/rho0
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)
          IF (ia.eq.1) THEN
            FORCES(ng)%Taux(i,j)=cff*fac
          ELSE
            FORCES(ng)%Taux(i,j)=FORCES(ng)%Taux(i,j)+cff*fac
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  USTRESS (Nm-2):  ',        &
     &                    range(1),range(2)
      END IF
!
!  Surface v-stress              (m2/s2)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "VSTRESS",  &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=1.0_r8/rho0
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)
          IF (ia.eq.1) THEN
            FORCES(ng)%Tauy(i,j)=cff*fac
          ELSE
            FORCES(ng)%Tauy(i,j)=FORCES(ng)%Tauy(i,j)+cff*fac
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  VSTRESS (Nm-2):  ',        &
     &                    range(1),range(2)
      END IF
!
!  Rotate to curvilinear grid.
!
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          cff1=FORCES(ng)%Taux(i,j)*GRID(ng)%CosAngler(i,j)+            &
     &         FORCES(ng)%Tauy(i,j)*GRID(ng)%SinAngler(i,j)
          cff2=FORCES(ng)%Tauy(i,j)*GRID(ng)%CosAngler(i,j)-            &
     &         FORCES(ng)%Taux(i,j)*GRID(ng)%SinAngler(i,j)
          FORCES(ng)%Taux(i,j)=cff1
          FORCES(ng)%Tauy(i,j)=cff2
        END DO
      END DO
!
!  Mean seal level pressure, convert from Pa to mb.
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "MSLP",     &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      ij=0
      fac=0.01_r8
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)*fac
          IF (ia.eq.1) THEN
            FORCES(ng)%Pair(i,j)=cff
          ELSE
            FORCES(ng)%Pair(i,j)=FORCES(ng)%Pair(i,j)+cff
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  MSLP    (mb):    ',        &
     &                    range(1),range(2)
      END IF
!
!  Surface air relative humidity (-)
!  Convert RELH from percent to fraction.
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "RELH",     &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      ij=0
      fac=0.01_r8
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)*fac
          IF (ia.eq.1) THEN
            FORCES(ng)%Hair(i,j)=cff
          ELSE
            FORCES(ng)%Hair(i,j)=FORCES(ng)%Hair(i,j)+cff
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  RELH    (-):     ',        &
     &                    range(1),range(2)
      END IF
!
!  Surface 2m air temperature    (degC)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "T2",       &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)
          IF (ia.eq.1) THEN
            FORCES(ng)%Tair(i,j)=cff
          ELSE
            FORCES(ng)%Tair(i,j)=FORCES(ng)%Tair(i,j)+cff
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  T2      (C):     ',        &
     &                    range(1),range(2)
      END IF
!
!  Precipitation                 (convert to kg/m2/s)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "RAIN",     &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=rho0
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)*fac
          IF (ia.eq.1) THEN
            FORCES(ng)%rain(i,j)=cff
          ELSE
            FORCES(ng)%rain(i,j)=FORCES(ng)%rain(i,j)+cff
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  RAIN  (kgm-2s-1):',        &
     &                    range(1),range(2)
      END IF
!
!  Evaporation                 (convert to kg/m2/s)
!
      CALL AttrVect_exportRAttr (AttrVect_G(ng)%atm2ocn_AV, "EVAP",     &
     &                           A, Asize)
      range(1)= Large
      range(2)=-Large
      fac=rho0
      ij=0
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          ij=ij+1
          cff=A(ij)*fac
          IF (ia.eq.1) THEN
            FORCES(ng)%evap(i,j)=cff
          ELSE
            FORCES(ng)%evap(i,j)=FORCES(ng)%evap(i,j)+cff
          END IF
          range(1)=MIN(range(1),cff)
          range(2)=MAX(range(2),cff)
        END DO
      END DO
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      IF (Myrank.eq.MyMaster) THEN
        write(stdout,40) 'WRFtoROMS Min/Max  EVAP  (kgm-2s-1):',        &
     &                    range(1),range(2)
      END IF
!
!  Apply boundary conditions.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
!
!-----------------------------------------------------------------------
!  Apply periodic boundary conditions.
!-----------------------------------------------------------------------
!
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%srflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%lrflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%lhflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%shflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Taux)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Tauy)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Pair)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Hair)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Tair)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%rain)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%evap)
      END IF
!
!-----------------------------------------------------------------------
!  Exchange tile boundaries.
!-----------------------------------------------------------------------
!
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%srflx, FORCES(ng)%lrflx)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%lhflx, FORCES(ng)%shflx)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &

     &                    LBi, UBi, LBj, UBj,                           &

     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Taux, FORCES(ng)%Tauy)
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Pair)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Hair, FORCES(ng)%Tair)
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%rain)
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%evap)
!
!  Deallocate communication arrays.
!
      deallocate (A)
      deallocate (points)
      if (associated (indices)) then
        deallocate (indices)
      endif
!
 10   FORMAT (' OCNFATM_COUPLING - error while receiving fields from ', &
     &        a, i4)
 20   FORMAT (' OCNFATM_COUPLING - error while sending fields to ',     &
     &        a, i4)
      RETURN
      END SUBROUTINE ocnfatm_coupling_tile
      SUBROUTINE finalize_ocn2atm_coupling
!
!========================================================================
!                                                                       !
!  This routine finalizes ocean and atmosphere models coupling data     !
!  streams.                                                             !
!                                                                       !
!========================================================================
      USE mod_scalars
      USE mct_coupler_params
!
!  Local variable declarations.
!
      integer :: ng, ia, MyError
!
!-----------------------------------------------------------------------
!  Deallocate MCT environment.
!-----------------------------------------------------------------------
!
      deallocate ( atmids )
      deallocate ( ocnids )
      DO ng=1,Nocn_grids
        CALL AttrVect_clean (AttrVect_G(ng)%ocn2atm_AV, MyError)
        CALL GlobalSegMap_clean (GlobalSegMap_G(ng)%GSMapROMS, MyError)
        DO ia=1,Natm_grids
          CALL Router_clean (Router_A(ng,ia)%ROMStoWRF, MyError)
        END DO
      END DO
      RETURN
      END SUBROUTINE finalize_ocn2atm_coupling
      SUBROUTINE ocean_coupling (nl)
!
!=======================================================================
!                                                                      !
!  Determine which roms grids are going to exchange data to otehr      !
!  model grids and call those exchange.                                !
!                                                                      !
!=======================================================================
!
      USE mod_parallel
      USE mct_coupler_params
      USE mod_scalars
!
!  Imported variable definitions.
!
      integer, intent(in) :: nl
!
!  Local variable declarations.
!
      integer :: MyError, nprocs, tile
      integer :: ng, iw, ia, ig, nlay, offset
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!  Couple ocean to atmosphere model every nOCN_ATM timesteps.
!-----------------------------------------------------------------------
!
      IF (nl.eq.1) THEN
        DO nlay=1,NestLayers
          DO ig=1,GridsInLayer(nlay)
            ng=GridNumber(ig,nlay)
            DO ia=1,Natm_grids
              offset=-1 !nlay-NestLayers
              IF (MOD(iic(1)+offset,nOCNFATM(1,1)).eq.0) THEN
                DO tile=first_tile(ng),last_tile(ng),+1
                  CALL ocnfatm_coupling (ng, ia, tile)
                END DO
              END IF
            END DO
          END DO
        END DO
      END IF
!
      IF (nl.eq.1) THEN
        DO nlay=1,NestLayers
          DO ig=1,GridsInLayer(nlay)
            ng=GridNumber(ig,nlay)
            DO ia=1,Natm_grids
              offset=-1 !nlay-NestLayers
              IF (MOD(iic(1)+offset,nOCN2ATM(1,1)).eq.0) THEN
                DO tile=first_tile(ng),last_tile(ng),+1
                  CALL ocn2atm_coupling (ng, ia, tile)
                END DO
              END IF
            END DO
          END DO
        END DO
      END IF
      RETURN
      END SUBROUTINE ocean_coupling
      END MODULE ocean_coupler_mod
