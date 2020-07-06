









































































































































































































































      MODULE mct_wrf_coupler_params



      USE mod_wrf_coupler_kinds

      implicit none



      integer :: N_mctmodels




      integer, dimension(:), pointer :: sparse_rows
      integer, dimension(:), pointer :: sparse_cols
      integer, dimension(:), pointer :: dst_grid_imask
      integer, dimension(2) :: src_grid_dims, dst_grid_dims
      real(m8), dimension(:), pointer :: sparse_weights

      TYPE T_DST_GRID
        integer, pointer :: dst_mask(:)
      END TYPE T_DST_GRID
      TYPE (T_DST_GRID), allocatable :: O2A_CPLMASK(:,:)
      TYPE (T_DST_GRID), allocatable :: A2O_CPLMASK(:,:)
      TYPE (T_DST_GRID), allocatable :: W2A_CPLMASK(:,:)





      integer :: NnodesATM
      integer :: NnodesWAV
      integer :: NnodesOCN




        integer :: peATM_frst          
        integer :: peATM_last          



        integer :: peOCN_frst          
        integer :: peOCN_last          
        integer, dimension(:), pointer :: roms_fwcoup
        integer, dimension(:), pointer :: roms_2wcoup
        integer, dimension(:), pointer :: roms_facoup
        integer, dimension(:), pointer :: roms_2acoup



      real(m8) :: TI_ATM2WAV           
      real(m8) :: TI_ATM2OCN           
      real(m8) :: TI_WAV2ATM           
      real(m8) :: TI_WAV2OCN           
      real(m8) :: TI_OCN2WAV           
      real(m8) :: TI_OCN2ATM           



      integer :: Natm_grids
      integer :: Nocn_grids
      integer :: Nwav_grids

      real(m8), dimension(:), pointer :: dtocn
      real(m8), dimension(:), pointer :: dtwav
      real(m8), dimension(:), pointer :: dtatm

      integer, dimension(:,:), pointer :: nOCN2ATM
      integer, dimension(:,:), pointer :: nATM2OCN
      integer, dimension(:,:), pointer :: nOCNFATM
      integer, dimension(:,:), pointer :: nATMFOCN



      integer, dimension(:), pointer :: ocnids
      integer, dimension(:), pointer :: wavids
      integer, dimension(:), pointer :: atmids
      integer :: OCNid
      integer :: WAVid
      integer :: ATMid


      CONTAINS

      SUBROUTINE allocate_coupler_params






      integer :: i

      allocate (nOCN2ATM(Nocn_grids,Natm_grids))
      allocate (nATM2OCN(Natm_grids,Nocn_grids))
      allocate (nOCNFATM(Nocn_grids,Natm_grids))
      allocate (nATMFOCN(Natm_grids,Nocn_grids))

      allocate(O2A_CPLMASK(Nocn_grids,Natm_grids))
      allocate(A2O_CPLMASK(Natm_grids,Nocn_grids))

      RETURN
      END SUBROUTINE allocate_coupler_params

      END MODULE mct_wrf_coupler_params
