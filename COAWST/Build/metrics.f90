      MODULE metrics_mod
!
!svn $Id: metrics.F 927 2018-10-16 03:51:56Z arango $
!=======================================================================
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This routine computes various horizontal metric terms.              !
!                                                                      !
!=======================================================================
!
      implicit none
      PRIVATE
      PUBLIC  :: metrics
      CONTAINS
!
!***********************************************************************
      SUBROUTINE metrics (ng, tile, model)
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_ocean
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
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
      CALL metrics_tile (ng, tile, model,                               &
     &                   LBi, UBi, LBj, UBj,                            &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp(ng), nnew(ng),                            &
     &                   GRID(ng) % f,                                  &
     &                   GRID(ng) % h,                                  &
     &                   GRID(ng) % pm,                                 &
     &                   GRID(ng) % pn,                                 &
     &                   GRID(ng) % pmask,                              &
     &                   GRID(ng) % rmask,                              &
     &                   GRID(ng) % angler,                             &
     &                   GRID(ng) % CosAngler,                          &
     &                   GRID(ng) % SinAngler,                          &
     &                   GRID(ng) % Hz,                                 &
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
     &                   GRID(ng) % om_p,                               &
     &                   GRID(ng) % om_r,                               &
     &                   GRID(ng) % om_u,                               &
     &                   GRID(ng) % om_v,                               &
     &                   GRID(ng) % on_p,                               &
     &                   GRID(ng) % on_r,                               &
     &                   GRID(ng) % on_u,                               &
     &                   GRID(ng) % on_v,                               &
     &                   GRID(ng) % fomn,                               &
     &                   GRID(ng) % omn,                                &
     &                   GRID(ng) % pnom_p,                             &
     &                   GRID(ng) % pnom_r,                             &
     &                   GRID(ng) % pnom_u,                             &
     &                   GRID(ng) % pnom_v,                             &
     &                   GRID(ng) % pmon_p,                             &
     &                   GRID(ng) % pmon_r,                             &
     &                   GRID(ng) % pmon_u,                             &
     &                   GRID(ng) % pmon_v)
      RETURN
      END SUBROUTINE metrics
!
!***********************************************************************
      SUBROUTINE metrics_tile (ng, tile, model,                         &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
     &                         f, h, pm, pn,                            &
     &                         pmask, rmask,                            &
     &                         angler, CosAngler, SinAngler,            &
     &                         Hz, z_r, z_w,                            &
     &                         om_p, om_r, om_u, om_v,                  &
     &                         on_p, on_r, on_u, on_v,                  &
     &                         fomn, omn,                               &
     &                         pnom_p, pnom_r, pnom_u, pnom_v,          &
     &                         pmon_p, pmon_r, pmon_u, pmon_v)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_nesting
      USE mod_scalars
      USE mod_iounits
!
      USE distribute_mod, ONLY : mp_reduce
      USE exchange_2d_mod
      USE mp_exchange_mod, ONLY : mp_exchange2d
      USE set_depth_mod, ONLY : set_depth_tile
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew
      real(r8), intent(in) :: f(LBi:,LBj:)
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(inout) :: h(LBi:,LBj:)
      real(r8), intent(inout) :: pmask(LBi:,LBj:)
      real(r8), intent(inout) :: rmask(LBi:,LBj:)
      real(r8), intent(inout) :: angler(LBi:,LBj:)
      real(r8), intent(out) :: om_p(LBi:,LBj:)
      real(r8), intent(out) :: om_r(LBi:,LBj:)
      real(r8), intent(out) :: om_u(LBi:,LBj:)
      real(r8), intent(out) :: om_v(LBi:,LBj:)
      real(r8), intent(out) :: on_p(LBi:,LBj:)
      real(r8), intent(out) :: on_r(LBi:,LBj:)
      real(r8), intent(out) :: on_u(LBi:,LBj:)
      real(r8), intent(out) :: on_v(LBi:,LBj:)
      real(r8), intent(out) :: fomn(LBi:,LBj:)
      real(r8), intent(out) :: omn(LBi:,LBj:)
      real(r8), intent(out) :: pnom_p(LBi:,LBj:)
      real(r8), intent(out) :: pnom_r(LBi:,LBj:)
      real(r8), intent(out) :: pnom_u(LBi:,LBj:)
      real(r8), intent(out) :: pnom_v(LBi:,LBj:)
      real(r8), intent(out) :: pmon_p(LBi:,LBj:)
      real(r8), intent(out) :: pmon_r(LBi:,LBj:)
      real(r8), intent(out) :: pmon_u(LBi:,LBj:)
      real(r8), intent(out) :: pmon_v(LBi:,LBj:)
      real(r8), intent(out) :: CosAngler(LBi:,LBj:)
      real(r8), intent(out) :: SinAngler(LBi:,LBj:)
      real(r8), intent(out) :: Hz(LBi:,LBj:,:)
      real(r8), intent(out) :: z_r(LBi:,LBj:,:)
      real(r8), intent(out) :: z_w(LBi:,LBj:,0:)
!
!  Local variable declarations.
!
      integer :: NSUB, i, ibry, is, j, k, rec
      integer :: cr, dg, ig, rg
      real(r8) :: cff, cff1, cff2
      real(r8) :: SecScale
      real(dp) :: my_DXmax, my_DXmin, my_DYmax, my_DYmin
      real(dp) :: my_DZmax, my_DZmin
      real(dp) :: my_Cg_Cor, my_Cg_max, my_Cg_min, my_grdmax
      real(dp), dimension(14) :: buffer
      character (len=3), dimension(14) :: op_handle
      real(r8), dimension(LBi:UBi,LBj:UBj) :: A2d
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
!  Compute 1/m, 1/n, 1/mn, and f/mn at horizontal RHO-points.
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          om_r(i,j)=1.0_r8/pm(i,j)
          on_r(i,j)=1.0_r8/pn(i,j)
          omn(i,j)=1.0_r8/(pm(i,j)*pn(i,j))
          fomn(i,j)=f(i,j)*omn(i,j)
        END DO
      END DO
!
!  Exchange boundary information.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          om_r)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          on_r)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          omn)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          fomn)
      END IF
      CALL mp_exchange2d (ng, tile, model, 4,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    om_r, on_r, omn, fomn)
!
!-----------------------------------------------------------------------
!  Compute n/m, and m/n at horizontal RHO-points.
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          pnom_r(i,j)=pn(i,j)/pm(i,j)
          pmon_r(i,j)=pm(i,j)/pn(i,j)
        END DO
      END DO
!
!  Exchange boundary information.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pnom_r)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pmon_r)
      END IF
      CALL mp_exchange2d (ng, tile, model, 2,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    pnom_r, pmon_r)
!
!-----------------------------------------------------------------------
!  Compute m/n, 1/m, and 1/n at horizontal U-points.
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          pmon_u(i,j)=(pm(i-1,j)+pm(i,j))/(pn(i-1,j)+pn(i,j))
          pnom_u(i,j)=(pn(i-1,j)+pn(i,j))/(pm(i-1,j)+pm(i,j))
          om_u(i,j)=2.0_r8/(pm(i-1,j)+pm(i,j))
          on_u(i,j)=2.0_r8/(pn(i-1,j)+pn(i,j))
        END DO
      END DO
!
!  Exchange boundary information.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pmon_u)
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pnom_u)
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          om_u)
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          on_u)
      END IF
      CALL mp_exchange2d (ng, tile, model, 4,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    pmon_u, pnom_u, om_u, on_u)
!
!-----------------------------------------------------------------------
!  Compute n/m, 1/m, and 1/m at horizontal V-points.
!-----------------------------------------------------------------------
!
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          pmon_v(i,j)=(pm(i,j-1)+pm(i,j))/(pn(i,j-1)+pn(i,j))
          pnom_v(i,j)=(pn(i,j-1)+pn(i,j))/(pm(i,j-1)+pm(i,j))
          om_v(i,j)=2.0_r8/(pm(i,j-1)+pm(i,j))
          on_v(i,j)=2.0_r8/(pn(i,j-1)+pn(i,j))
        END DO
      END DO
!
!  Exchange boundary information.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pmon_v)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pnom_v)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          om_v)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          on_v)
      END IF
      CALL mp_exchange2d (ng, tile, model, 4,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    pmon_v, pnom_v, om_v, on_v)
!
!-----------------------------------------------------------------------
!  Compute n/m and m/n at horizontal PSI-points.
!-----------------------------------------------------------------------
!
      DO j=JstrP,JendT
        DO i=IstrP,IendT
          pnom_p(i,j)=(pn(i-1,j-1)+pn(i-1,j)+pn(i,j-1)+pn(i,j))/        &
     &                (pm(i-1,j-1)+pm(i-1,j)+pm(i,j-1)+pm(i,j))
          pmon_p(i,j)=(pm(i-1,j-1)+pm(i-1,j)+pm(i,j-1)+pm(i,j))/        &
     &                (pn(i-1,j-1)+pn(i-1,j)+pn(i,j-1)+pn(i,j))
          om_p(i,j)=4.0_r8/(pm(i-1,j-1)+pm(i-1,j)+pm(i,j-1)+pm(i,j))
          on_p(i,j)=4.0_r8/(pn(i-1,j-1)+pn(i-1,j)+pn(i,j-1)+pn(i,j))
        END DO
      END DO
!
!  Exchange boundary information.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_p2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pnom_p)
        CALL exchange_p2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pmon_p)
        CALL exchange_p2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          om_p)
        CALL exchange_p2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          on_p)
      END IF
      CALL mp_exchange2d (ng, tile, model, 4,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    pnom_p, pmon_p, om_p, on_p)
!
!-----------------------------------------------------------------------
!  Set slipperiness (no-slip) mask at PSI-points.
!-----------------------------------------------------------------------
!
! Set no-slip boundary conditions on land-mask boundaries regardless of
! supplied value of gamma2.
!
      cff1=1.0_r8       ! computation of off-diagonal nonlinear terms
      cff2=2.0_r8
      DO j=JstrP,JendP
        DO i=IstrP,IendP
          IF ((rmask(i-1,j  ).gt.0.5_r8).and.                           &
     &        (rmask(i  ,j  ).gt.0.5_r8).and.                           &
     &        (rmask(i-1,j-1).gt.0.5_r8).and.                           &
     &        (rmask(i  ,j-1).gt.0.5_r8)) THEN
            pmask(i,j)=1.0_r8
          ELSE IF ((rmask(i-1,j  ).lt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).gt.0.5_r8)) THEN
            pmask(i,j)=cff1
          ELSE IF ((rmask(i-1,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).lt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).gt.0.5_r8)) THEN
            pmask(i,j)=cff1
          ELSE IF ((rmask(i-1,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).lt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).gt.0.5_r8)) THEN
            pmask(i,j)=cff1
          ELSE IF ((rmask(i-1,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).lt.0.5_r8)) THEN
            pmask(i,j)=cff1
          ELSE IF ((rmask(i-1,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).lt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).lt.0.5_r8)) THEN
            pmask(i,j)=cff2
          ELSE IF ((rmask(i-1,j  ).lt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).lt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).gt.0.5_r8)) THEN
            pmask(i,j)=cff2
          ELSE IF ((rmask(i-1,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).gt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).lt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).lt.0.5_r8)) THEN
            pmask(i,j)=cff2
          ELSE IF ((rmask(i-1,j  ).lt.0.5_r8).and.                      &
     &             (rmask(i  ,j  ).lt.0.5_r8).and.                      &
     &             (rmask(i-1,j-1).gt.0.5_r8).and.                      &
     &             (rmask(i  ,j-1).gt.0.5_r8)) THEN
            pmask(i,j)=cff2
          ELSE
            pmask(i,j)=0.0_r8
          END IF
        END DO
      END DO
!
!  Exchange boundary information.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_p2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          pmask)
      END IF
      CALL mp_exchange2d (ng, tile, model, 1,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    pmask)
!
!-----------------------------------------------------------------------
! Compute cosine and sine of grid rotation angle.
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          CosAngler(i,j)=COS(angler(i,j))
          SinAngler(i,j)=SIN(angler(i,j))
        END DO
      END DO
!
!  Exchange boundary information.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          CosAngler)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          SinAngler)
      END IF
      CALL mp_exchange2d (ng, tile, model, 2,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    CosAngler, SinAngler)
!
!-----------------------------------------------------------------------
!  Compute minimum and maximum grid spacing.
!-----------------------------------------------------------------------
!
!  Compute time invariant depths (use zero free-surface).
!
      DO i=LBi,UBi
        DO j=LBj,UBj
          A2d(i,j)=0.0_r8
        END DO
      END DO
      CALL set_depth_tile (ng, tile, iNLM,                              &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     nstp, nnew,                                  &
     &                     h,                                           &
     &                     A2d,                                         &
     &                     Hz, z_r, z_w)
!
!  Compute grid spacing range.
!
      my_DXmin= Large
      my_DXmax=-Large
      my_DYmin= Large
      my_DYmax=-Large
      my_DZmin= Large
      my_DZmax=-Large
      my_grdmax=-Large
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          cff=SQRT(om_r(i,j)*on_r(i,j))
          my_DXmin=MIN(my_DXmin,om_r(i,j))
          my_DXmax=MAX(my_DXmax,om_r(i,j))
          my_DYmin=MIN(my_DYmin,on_r(i,j))
          my_DYmax=MAX(my_DYmax,on_r(i,j))
          my_grdmax=MAX(my_grdmax,cff)
          DO k=1,N(ng)
            my_DZmin=MIN(my_DZmin,Hz(i,j,k))
            my_DZmax=MAX(my_DZmax,Hz(i,j,k))
          END DO
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Compute gravity waves Courant number.
!-----------------------------------------------------------------------
!
!  The 2D Courant number is defined as:
!
!     Cg = c * dt * SQRT (1/dx^2 + 1/dy^2)
!
!  where c=SQRT(g*h) is gravity wave speed, and dx, dy are grid spacing
!  in each direction.
!
      my_Cg_min= Large
      my_Cg_max=-Large
      my_Cg_Cor=-Large
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          IF (rmask(i,j).gt.0.0_r8) THEN
            cff=dtfast(ng)*                                             &
     &          SQRT(g*ABS(h(i,j))*(pm(i,j)*pm(i,j)+pn(i,j)*pn(i,j)))
            my_Cg_min=MIN(my_Cg_min,cff)
            my_Cg_max=MAX(my_Cg_max,cff)
            cff=dt(ng)*ABS(f(i,j))
            my_Cg_Cor=MAX(my_Cg_Cor,cff)
          END IF
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Perform global reductions.
!-----------------------------------------------------------------------
!
      NSUB=1                             ! distributed-memory
!$OMP CRITICAL (REDUCTIONS)
      Cg_min(ng)=MIN(Cg_min(ng),my_Cg_min)
      Cg_max(ng)=MAX(Cg_max(ng),my_Cg_max)
      Cg_Cor(ng)=MAX(Cg_Cor(ng),my_Cg_Cor)
      grdmax(ng)=MAX(grdmax(ng),my_grdmax)
      DXmin(ng)=MIN(DXmin(ng),my_DXmin)
      DXmax(ng)=MAX(DXmax(ng),my_DXmax)
      DYmin(ng)=MIN(DYmin(ng),my_DYmin)
      DYmax(ng)=MAX(DYmax(ng),my_DYmax)
      DZmin(ng)=MIN(DZmin(ng),my_DZmin)
      DZmax(ng)=MAX(DZmax(ng),my_DZmax)
      tile_count=tile_count+1
      IF (tile_count.eq.NSUB) THEN
        tile_count=0
        buffer(1)=Cg_min(ng)
        op_handle(1)='MIN'
        buffer(2)=Cg_max(ng)
        op_handle(2)='MAX'
        buffer(3)=Cg_Cor(ng)
        op_handle(3)='MAX'
        buffer(4)=grdmax(ng)
        op_handle(4)='MAX'
        buffer(5)=DXmin(ng)
        op_handle(5)='MIN'
        buffer(6)=DXmax(ng)
        op_handle(6)='MAX'
        buffer(7)=DYmin(ng)
        op_handle(7)='MIN'
        buffer(8)=DYmax(ng)
        op_handle(8)='MAX'
        buffer(9)=DZmin(ng)
        op_handle(9)='MIN'
        buffer(10)=DZmax(ng)
        op_handle(10)='MAX'
        buffer(11)=0.0_r8
        op_handle(11)='MIN'
        buffer(12)=0.0_r8
        op_handle(12)='MAX'
        buffer(13)=0.0_r8
        op_handle(13)='MIN'
        buffer(14)=0.0_r8
        op_handle(14)='MAX'
        CALL mp_reduce (ng, model, 14, buffer, op_handle)
        Cg_min(ng)=buffer(1)
        Cg_max(ng)=buffer(2)
        Cg_Cor(ng)=buffer(3)
        grdmax(ng)=buffer(4)
        DXmin(ng)=buffer(5)
        DXmax(ng)=buffer(6)
        DYmin(ng)=buffer(7)
        DYmax(ng)=buffer(8)
        DZmin(ng)=buffer(9)
        DZmax(ng)=buffer(10)
        IF (Master.and.LwrtInfo(ng)) THEN
          WRITE(stdout,10) ng,                                          &
     &                     DXmin(ng)/1000.0_r8, DXmax(ng)/1000.0_r8,    &
     &                     DYmin(ng)/1000.0_r8, DYmax(ng)/1000.0_r8
  10      FORMAT (/,' Metrics information for Grid ',i2.2,':',          &
     &            /,' ===============================',/,               &
     &            /,' Minimum X-grid spacing, DXmin = ',1pe15.8,' km',  &
     &            /,' Maximum X-grid spacing, DXmax = ',1pe15.8,' km',  &
     &            /,' Minimum Y-grid spacing, DYmin = ',1pe15.8,' km',  &
     &            /,' Maximum Y-grid spacing, DYmax = ',1pe15.8,' km')
          WRITE(stdout,20) DZmin(ng), DZmax(ng)
  20      FORMAT (' Minimum Z-grid spacing, DZmin = ',1pe15.8,' m',/,   &
     &            ' Maximum Z-grid spacing, DZmax = ',1pe15.8,' m')
          WRITE (stdout,30) Cg_min(ng), Cg_max(ng), Cg_Cor(ng)
  30      FORMAT (/,' Minimum barotropic Courant Number = ', 1pe15.8,/, &
     &              ' Maximum barotropic Courant Number = ', 1pe15.8,/, &
     &              ' Maximum Coriolis   Courant Number = ', 1pe15.8,/)
        END IF
      END IF
!$OMP END CRITICAL (REDUCTIONS)
!
!-----------------------------------------------------------------------
!  If grid refinement, set various important parameters.
!-----------------------------------------------------------------------
!
      IF (ng.eq.Ngrids) THEN
!
!  Set coarser donor grid number to finer grid external contact points.
!  The reciver grid is always finer than the donor grid, DXmax(dg) >
!  DXmax(rg). It is done in terms of the reciver grid.
!
        DO cr=1,Ncontact
          dg=donor_grid(cr)
          rg=receiver_grid(cr)
          IF (RefinedGrid(rg).and.                                        &
     &        (RefineScale(rg).gt.0).and.                                 &
     &         DXmax(dg).gt.DXmax(rg)) THEN
            CoarserDonor(rg)=dg
          END IF
        END DO
!
!  Set donor finer grid number to receiver coarser grid. This is use for
!  two-way feeback between grids. The donor grid is always finer than the
!  receiver grid, DXmax(dg) < DXmax(rg). It is done in terms of the donor
!  grid.
!
        DO cr=1,Ncontact
          dg=donor_grid(cr)
          rg=receiver_grid(cr)
          IF (RefinedGrid(dg).and.                                        &
     &        (RefineScale(dg).gt.0).and.                                 &
     &        DXmax(dg).gt.DXmax(rg)) THEN
            FinerDonor(dg)=rg
          END IF
        END DO
!
!  Set logical indicating which coarser grid is a donor to a finer
!  grid external contact points.
!
        DO dg=1,Ngrids
          DO ig=1,Ngrids
            IF (CoarserDonor(ig).eq.dg) THEN
              DonorToFiner(dg)=.TRUE.
            END IF
          END DO
        END DO
!
!  Set switch indicating which refined grid(s), with RefineScale(dg) > 0,
!  include finer refined grids inside (telescoping refinement).
!
        DO cr=1,Ncontact
          dg=donor_grid(cr)
          rg=receiver_grid(cr)
          IF ((RefineScale(dg).gt.0).and.(CoarserDonor(rg).eq.dg)) THEN
            Telescoping(dg)=.TRUE.
          END IF
        END DO
!
!  Set Number of refined grid time-steps using the ratio between donor
!  and receiver grids.  Optimally, thes values should be based on the
!  spatial refinement ratio for stability. However, the user is allowed
!  to take larger divisible time-step with respect to the donor grid.
!  The user is responsible to set the appropriate refined grid time-step
!  for stability.
!
        DO cr=1,Ncontact
          dg=donor_grid(cr)
          rg=receiver_grid(cr)
          IF (RefinedGrid(rg).and.(CoarserDonor(rg).eq.dg)) THEN
            RefineSteps(rg)=NINT(dt(dg)/dt(rg))
          END IF
        END DO
!
!  Report information.
!
        IF (DOMAIN(ng)%NorthEast_Test(tile)) THEN
          IF (Master.and.ANY(RefinedGrid)) THEN
            WRITE (stdout,70)
  70        FORMAT (/,' Refined Nested Grid(s) Information: ',          &
     &              /,' ==================================',/,/,        &
     &              ' Refined    Donor   Refined   Timestep   Refined', &
     &              /,'  Grid      Grid    Scale      Ratio   ',        &
     &              'Timesteps',/)
            DO ig=1,Ngrids
              IF (RefineScale(ig).gt.0) THEN
                dg=CoarserDonor(ig)
                WRITE (stdout,80) ig, dg, RefineScale(ig),              &
     &                            dt(dg)/dt(ig), RefineSteps(ig)
  80            FORMAT(4x,i2.2,8x,i2.2,7x,i2.2,4x,f9.5,6x,i2.2)
              END IF
            END DO
            WRITE (stdout,90)
  90        FORMAT (/,' WARNING: Usually the number of Refined ',       &
     &              'Timesteps must be the same',/,10x,                 &
     &              'as the Refined Scale for numerical stability.',/)
          END IF
        END IF
!
!  Check refined grid time-step.  The time-step size for refined grids
!  needs to be an exact mode multiple of its coarser donor grid.  In
!  principle, it can be a "RefineScale" factor smaller.  However, other
!  integer smaller or larger factor is allowed such that:
!
!    MOD(dt(dg)*SecScale, dt(rg)*SecScale) = 0   dg:  donor  coarse grid
!                                                rg:  receiver fine grid
!
!  Notice that SecScale is used to avoid roundoff when the timestep
!  between donor and receiver grids are small and less than one.
!
        SecScale=1000.0_dp              ! seconds to milliseconds
        DO ig=1,Ngrids
          IF (RefinedGrid(ig).and.(RefineScale(ig).gt.0)) THEN
            dg=CoarserDonor(ig)
            IF (MOD(dt(dg)*SecScale,dt(ig)*SecScale).ne.0.0_dp) THEN
              IF (DOMAIN(ng)%SouthWest_Test(tile)) THEN
                IF (Master) THEN
                  WRITE (stdout,100) ig, dt(ig), dg, dt(dg),            &
     &                               MOD(dt(dg),dt(ig))
  100             FORMAT (/,' METRICS - illegal timestep size for ',    &
     &                    ' finer reciever grid (rg),',/,11x,           &
     &                    'It must be an exact divisible factor from ', &
     &                    'donor grid (dg).',/,/,11x,'rg = ',i2.2,      &
     &                    ', dt(rg) = ',f12.4,/,11x,'dg = ',i2.2,       &
     &                    ', dt(dg) = ',f12.4,4x,                       &
     &                    'MOD[dt(dg), dt(rg)] = ',f10.4,/)
                END IF
              END IF
              exit_flag=5
              RETURN
            END IF
          END IF
        END DO
      END IF
      RETURN
      END SUBROUTINE metrics_tile
      END MODULE metrics_mod
