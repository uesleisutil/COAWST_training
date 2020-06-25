<<<<<<< HEAD
subroutine SwanVertlist
=======
subroutine SwanVertlist ( compda )
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
<<<<<<< HEAD
!     Copyright (C) 1993-2017  Delft University of Technology
=======
!     Copyright (C) 1993-2019  Delft University of Technology
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!   Authors
!
!   40.80: Marcel Zijlema
!   41.07: Casey Dietrich
!   41.48: Marcel Zijlema
<<<<<<< HEAD
!
!   Updates
!
!   40.80,  July 2007: New subroutine
!   41.07,  July 2009: small fix (assign ref.point to deepest point in case of no b.c.)
!   41.48, March 2013: including order along a user-given direction
!
!   Purpose
!
!   Makes vertex list with specific order
!
!   Method
!
!   Sorting based on distance with respect to a reference point
!   This reference point can be either a vertex with boundary condition or a deepest point
=======
!   41.68: Marcel Zijlema
!
!   Updates
!
!   40.80,   July 2007: New subroutine
!   41.07,   July 2009: small fix (assign ref.point to deepest point in case of no b.c.)
!   41.48,  March 2013: including order along a user-given direction
!   41.68, August 2015: introduction of a fixed number of sweeps per iteration
!   41.68,  April 2018: removal of the wavefront approach (based on reference point)
!
!   Purpose
!
!   Makes vertex list in line with sweep direction
!   Note: first sweep direction always equals user-given/wave/wind direction
!
!   Method
!
!   Sorting based on increasing distance along sweep direction
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!   Modules used
!
    use ocpcomm4
<<<<<<< HEAD
    use m_genarr, only: DEPTH
    use SwanGriddata
    use SwanGridobjects
    use SwanCompdata
!
    implicit none
!
!   Local variables
!
    integer, save                   :: ient = 0   ! number of entries in this subroutine
    integer                         :: istat      ! indicate status of allocation
    integer                         :: itmp       ! temporary stored integer for swapping
    integer                         :: j          ! loop counter over vertices
    integer                         :: k          ! counter
    integer, dimension(1)           :: kd         ! location of minimum value in array dist
    integer, dimension(1)           :: kx         ! location of minimum value in array of x-coordinates of boundary vertices
    integer, dimension(1)           :: ky         ! location of minimum value in array of y-coordinates of boundary vertices
    !
    real                            :: d1         ! distance of a point to origin
    real                            :: d2         ! distance of another point to origin
    real                            :: depmax     ! maximum depth found
    real                            :: rtmp       ! temporary stored real for swapping
    real                            :: x0         ! x-coordinate of reference point
    real                            :: y0         ! y-coordinate of reference point
    !
    real, dimension(:), allocatable :: dist       ! distance of each point with respect to reference point
    !
    type(verttype), dimension(:), pointer :: vert ! datastructure for vertices with their attributes
=======
    use swcomm2, only: COSWC, SINWC, VARWI
    use swcomm3, only: MCMVAR, JWX2, JWY2, JWX3, JWY3
    use m_genarr
    use SwanGriddata
    use SwanGridobjects
    use SwanCompdata
!PUN    use SIZES, only: MNPROC
!
    implicit none
!
!   Argument variables
!
    real, dimension(nverts,MCMVAR), intent(in) :: compda ! array containing space-dependent info (e.g. wind)
!
!   Local variables
!
    integer, save                     :: ient = 0   ! number of entries in this subroutine
    integer                           :: ierr       ! error indicator: ierr=0: no error, otherwise error
    integer                           :: istat      ! indicate status of allocation
    integer                           :: itmp       ! temporary stored integer for swapping
    integer                           :: j          ! loop counter over vertices
    integer                           :: k          ! counter
    integer, dimension(1)             :: kd         ! location of minimum value in array dist
    integer                           :: swpdir     ! sweep counter
    !
    real                              :: rtmp       ! temporary stored real for swapping
    real                              :: sdir       ! sweep direction
    real                              :: wdsum      ! total sum of wind direction
    real                              :: wx         ! wind velocity in x-direction
    real                              :: wy         ! wind velocity in y-direction
    !
    real, dimension(:,:), allocatable :: dist       ! distance of each point with respect to reference point
    !
    type(verttype), dimension(:), pointer :: vert   ! datastructure for vertices with their attributes
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
    if (ltrace) call strace (ient,'SwanVertlist')
    !
    ! point to vertex object
    !
    vert => gridobject%vert_grid
    !
    istat = 0
<<<<<<< HEAD
    if(.not.allocated(vlist)) allocate (vlist(nverts), stat = istat)
=======
    if(.not.allocated(vlist)) allocate (vlist(nverts,nsweep), stat = istat)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanVertlist: array vlist ' )
       return
    endif
    !
<<<<<<< HEAD
    allocate (dist(nverts))
    !
    if ( asort > -999. ) then
       !
       ! order direction asort
       !
       do j = 1, nverts
          dist(j) = vert(j)%attr(VERTX) * cos(asort) + vert(j)%attr(VERTY) * sin(asort)
       enddo
       !
    else
       !
       ! determine reference point
       !
       if ( all(mask=vert(:)%atti(VBC)==0) ) then
          !
          ! if no boundary condition is given then find the vertex with the maximum depth
          !
          depmax = -999.
          !
          do j = 1, nverts
             !
             if ( DEPTH(j) > depmax ) then
                !
                depmax = DEPTH(j)
                kx(1)  = j
                ky(1)  = j
                !
             endif
             !
          enddo
          !
       else
          !
          ! reference point is one of the vertices nearest to the origin where boundary condition is given
          !
          kx = minloc(vert(:)%attr(VERTX), vert(:)%atti(VBC)/=0)
          ky = minloc(vert(:)%attr(VERTY), vert(:)%atti(VBC)/=0)
          !
       endif
       !
       if ( kx(1) == ky(1) ) then
          x0 = vert(kx(1))%attr(VERTX)
          y0 = vert(ky(1))%attr(VERTY)
       else
          !
          d1 = sqrt((vert(kx(1))%attr(VERTX))**2+(vert(kx(1))%attr(VERTY))**2)
          d2 = sqrt((vert(ky(1))%attr(VERTX))**2+(vert(ky(1))%attr(VERTY))**2)
          !
          if ( d1 < d2 ) then
             x0 = vert(kx(1))%attr(VERTX)
             y0 = vert(kx(1))%attr(VERTY)
          else
             x0 = vert(ky(1))%attr(VERTX)
             y0 = vert(ky(1))%attr(VERTY)
          endif
          !
       endif
       !
       ! calculate distance of each point with respect to reference point
       !
       do j = 1, nverts
          dist(j) = sqrt((vert(j)%attr(VERTX)-x0)**2 + (vert(j)%attr(VERTY)-y0)**2)
       enddo
       !
    endif
    !
    ! sort vertex list in order of increasing distance
    !
    vlist=(/ (j, j=1, nverts) /)
    !
    do j = 1, nverts-1
       !
       kd = minloc(dist(j:nverts))
       k  = kd(1) + j-1
       !
       if ( k /= j ) then
          !
          rtmp     = dist(j)
          dist(j)  = dist(k)
          dist(k)  = rtmp
          !
          itmp     = vlist(j)
          vlist(j) = vlist(k)
          vlist(k) = itmp
          !
       endif
=======
    allocate (dist(nverts,nsweep))
    !
    ! check first sweep direction
    !
    if ( .not. asort > -999. ) then
!      if asort still does not have a value, try space-varying wind and take the mean of wind direction
       if ( VARWI ) then
!         retrieve current wind field
          if ( NSTATM == 1 ) call FLFILE ( 5, 6, WXI, WYI, 0, JWX2, JWX3, 0, JWY2, JWY3, COSWC, SINWC, compda, XCGRID, YCGRID, KGRPNT, ierr )
          k     = 0
          wdsum = 0.
          do j = 1, nverts
!            internal vertices and ghost vertices only
             if ( vmark(j) == 0 .or. vmark(j) == 999 ) then
                wx = compda(j,JWX2)
                wy = compda(j,JWY2)
                if ( wx /= 0. .or. wy /= 0. ) then
                   k = k + 1
                   wdsum = wdsum + atan2(wy,wx)
                endif
             endif
          enddo
          asort = wdsum / real(k)
!PUN          call SwanSumOverNodes ( asort )
!PUN          asort = asort / real(MNPROC)
       else
!         final attempt: set sweep direction to zero
          asort = 0.
       endif
    endif
    if ( ITEST >= 40 ) write (PRINTF,10) nsweep, 180.*asort/PI
    asort = asort - PI/real(nsweep)
    !
    ! order vertices according to sweep direction; base vector is user-given/wave/wind direction
    !
    sdir = asort + PI/real(nsweep)
    do swpdir = 1, nsweep
       do j = 1, nverts
          dist(j,swpdir) = vert(j)%attr(VERTX) * cos(sdir) + vert(j)%attr(VERTY) * sin(sdir)
       enddo
       sdir = sdir + PI2/real(nsweep)
    enddo
    !
    ! sort vertex list in order of increasing distance
    !
    do swpdir = 1, nsweep
       !
       do j = 1, nverts
          vlist(j,swpdir) = j
       enddo
       !
       do j = 1, nverts-1
          !
          kd = minloc(dist(j:nverts,swpdir))
          k  = kd(1) + j-1
          !
          if ( k /= j ) then
             !
             rtmp            = dist(j,swpdir)
             dist(j,swpdir)  = dist(k,swpdir)
             dist(k,swpdir)  = rtmp
             !
             itmp            = vlist(j,swpdir)
             vlist(j,swpdir) = vlist(k,swpdir)
             vlist(k,swpdir) = itmp
             !
          endif
          !
       enddo
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
       !
    enddo
    !
    deallocate(dist)
    !
<<<<<<< HEAD
=======
 10 format (' Number of sweeps = ',i2,'; chosen wave direction for sweeping: ',f7.2,' degrees')
    !
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
end subroutine SwanVertlist
