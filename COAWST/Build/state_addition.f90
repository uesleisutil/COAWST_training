      MODULE state_addition_mod
!
!svn $Id: state_addition.F 889 2018-02-10 03:32:52Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the following model state addition:           !
!                                                                      !
!      s1_var(...,Lout) = fac1 * s1_var(...,Lin1) +                    !
!                         fac2 * s2_var(...,Lin2)                      !
!                                                                      !
!  where fac1 and fac2 are scalars.                                    !
!                                                                      !
!=======================================================================
!
      implicit none
      PUBLIC  :: state_addition
      CONTAINS
!
!***********************************************************************
      SUBROUTINE state_addition (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj, LBij, UBij,        &
     &                           Lin1, Lin2, Lout,                      &
     &                           fac1, fac2,                            &
     &                           rmask, umask, vmask,                   &
     &                           s1_t, s2_t,                            &
     &                           s1_u, s2_u,                            &
     &                           s1_v, s2_v,                            &
     &                           s1_zeta, s2_zeta)
!***********************************************************************
!
      USE mod_param
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBij, UBij
      integer, intent(in) :: Lin1, Lin2, Lout
!
      real(r8), intent(in) :: fac1, fac2
!
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
      real(r8), intent(in) :: s2_t(LBi:,LBj:,:,:,:)
      real(r8), intent(in) :: s2_u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: s2_v(LBi:,LBj:,:,:)
      real(r8), intent(in) :: s2_zeta(LBi:,LBj:,:)
      real(r8), intent(inout) :: s1_t(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: s1_u(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: s1_v(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: s1_zeta(LBi:,LBj:,:)
!
!  Local variable declarations.
!
      integer :: i, j, k
      integer :: ib, ir, it
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
!  Compute the following operation between S1 and S2 model state
!  trajectories:
!                 S1(Lout) = fac1 * S1(Lin1) + fac2 * S2(Lin2)
!-----------------------------------------------------------------------
!
!  Free-surface.
!
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          s1_zeta(i,j,Lout)=fac1*s1_zeta(i,j,Lin1)+                     &
     &                      fac2*s2_zeta(i,j,Lin2)
          s1_zeta(i,j,Lout)=s1_zeta(i,j,Lout)*rmask(i,j)
        END DO
      END DO
!
!  3D U-momentum component.
!
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrP,IendT
            s1_u(i,j,k,Lout)=fac1*s1_u(i,j,k,Lin1)+                     &
     &                       fac2*s2_u(i,j,k,Lin2)
            s1_u(i,j,k,Lout)=s1_u(i,j,k,Lout)*umask(i,j)
          END DO
        END DO
      END DO
!
!  3D V-momentum component.
!
      DO k=1,N(ng)
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            s1_v(i,j,k,Lout)=fac1*s1_v(i,j,k,Lin1)+                     &
     &                       fac2*s2_v(i,j,k,Lin2)
            s1_v(i,j,k,Lout)=s1_v(i,j,k,Lout)*vmask(i,j)
          END DO
        END DO
      END DO
!
!  Tracers.
!
      DO it=1,NT(ng)
        DO k=1,N(ng)
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              s1_t(i,j,k,Lout,it)=fac1*s1_t(i,j,k,Lin1,it)+             &
     &                            fac2*s2_t(i,j,k,Lin2,it)
              s1_t(i,j,k,Lout,it)=s1_t(i,j,k,Lout,it)*rmask(i,j)
            END DO
          END DO
        END DO
      END DO
      RETURN
      END SUBROUTINE state_addition
      END MODULE state_addition_mod
