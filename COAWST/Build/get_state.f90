      SUBROUTINE get_state (ng, model, msg, ncname, IniRec, Tindex)
!
!svn $Id: get_state.F 930 2018-11-14 16:58:13Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2019 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in requested model state from specified NetCDF   !
!  file. It is usually used to read initial conditions.                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number.                                   !
!     model      Calling model identifier.                             !
!     msg        Message index for StateMsg.                           !
!     ncname     Nonlinear initial conditions NetCDF file name.        !
!     IniRec     Nonlinear initial conditions time record to read.     !
!     Tindex     State variable time index to intialize.               !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
      USE mod_strings
      USE dateclock_mod,      ONLY : time_string
      USE mp_exchange_mod,    ONLY : mp_exchange2d
      USE mp_exchange_mod,    ONLY : mp_exchange3d
      USE nf_fread2d_mod,     ONLY : nf_fread2d
      USE nf_fread3d_mod,     ONLY : nf_fread3d
      USE nf_fread4d_mod,     ONLY : nf_fread4d
      USE strings_mod,        ONLY : find_string
      USE strings_mod,        ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, msg, Tindex
      integer, intent(inout) :: IniRec
      character (len=*), intent(in) :: ncname
!
!  Local variable declarations.
!
      logical :: Perfect2D, Perfect3D, foundit, read_ice
      logical, dimension(NV) :: get_var, have_var
      integer :: LBi, UBi, LBj, UBj
      integer :: IDmod, InpRec, gtype, i, ifield, itrc, lstr, lend
      integer :: Nrec, mySize, ncINPid, nvatts, nvdim, status, varid
      integer :: Vsize(4), start(4), total(4)
      real(dp), parameter :: Fscl = 1.0_r8
      real(dp) :: INPtime, Tmax, scale, time_scale
      real(r8) :: Fmax, Fmin
      real(dp), allocatable :: TimeVar(:)
      character (len=6 ) :: string
      character (len=15) :: Tstring, attnam, tvarnam
      character (len=22) :: t_code
      character (len=40) :: tunits
!
      SourceFile="ROMS/Utility/get_state.F"
!
!-----------------------------------------------------------------------
!  Determine variables to read and their availability.
!-----------------------------------------------------------------------
!
!  Set model identification string.
!
      IF (model.eq.iNLM.or.(model.eq.0)) THEN
        string=' NLM: '                    ! nonlinear model, restart
        IDmod=iNLM
      ELSE IF (model.eq.iTLM) THEN
        string=' TLM: '                    ! tangent linear model
        IDmod=iTLM
      ELSE IF (model.eq.iRPM) THEN
        string=' RPM: '                    ! representer model
        IDmod=iRPM
      ELSE IF (model.eq.iADM) THEN
        string=' ADM: '                    ! adjoint model
        IDmod=iADM
      ELSE IF (model.eq.5) THEN
        string=' NLM: '                    ! surface forcing and
        IDmod=iNLM                         ! OBC increments
      ELSE IF (model.eq.6) THEN
        string=' TLM: '                    ! tangent linear error
        IDmod=iTLM                         ! forcing (time covariance)
      ELSE IF (model.eq.7) THEN
        string=' FRC: '                    ! impulse forcing
        IDmod=iNLM
      ELSE IF (model.eq.8) THEN
        string=' TLM: '                    ! v-space increments
        IDmod=iTLM                         ! I4D-Var
      ELSE IF (model.eq.9) THEN
        string=' NLM: '                    ! nonlinear model
        IDmod=iNLM                         ! background state
      ELSE IF (model.eq.10) THEN
        string=' STD: '                    ! standard deviation
        IDmod=iNLM                         ! initial conditions
      ELSE IF (model.eq.11) THEN
        string=' STD: '                    ! standard deviation
        IDmod=iNLM                         ! model error
      ELSE IF (model.eq.12) THEN
        string=' STD: '                    ! standard deviation
        IDmod=iNLM                         ! boundary conditions
      ELSE IF (model.eq.13) THEN
        string=' STD: '                    ! standard deviation
        IDmod=iNLM                         ! surface forcing
      ELSE IF (model.eq.14) THEN
        string=' NRM: '                    ! normalization factors
        IDmod=iNLM                         ! initial conditions
      ELSE IF (model.eq.15) THEN
        string=' NRM: '                    ! normalization factors
        IDmod=iNLM                         ! model error
      ELSE IF (model.eq.16) THEN
        string=' NRM: '                    ! normalization factor
        IDmod=iNLM                         ! boundary conditions
      ELSE IF (model.eq.17) THEN
        string=' NRM: '                    ! normalization factor
        IDmod=iNLM                         ! surface forcing
      END IF
!
!  Turn on time wall clock.
!
      CALL wclock_on (ng, IDmod, 42, 203, "ROMS/Utility/get_state.F")
!
!  Set switch to process variables for nonlinear model perfect restart.
!
      Perfect2D=.FALSE.
      Perfect3D=.FALSE.
      PerfectRST(ng)=Perfect2D.or.Perfect3D
!
!  Set Vsize to zero to deactivate interpolation of input data to model
!  grid in "nf_fread2d" and "nf_fread3d".
!
      DO i=1,4
        Vsize(i)=0
      END DO
      SourceFile="ROMS/Utility/get_state.F"
!
!-----------------------------------------------------------------------
!  Open input NetCDF file and check time variable.
!-----------------------------------------------------------------------
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!  Open input NetCDF file.
!
      CALL netcdf_open (ng, IDmod, ncname, 0, ncINPid)
      IF (FoundError(exit_flag, NoError, 238,                           &
     &               "ROMS/Utility/get_state.F")) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(ncname)
        RETURN
      END IF
!
!  Determine variables to read.
!
      CALL checkvars (ng, model, ncname, ncINPid, string, Nrec, NV,     &
     &                tvarnam, get_var, have_var)
      IF (FoundError(exit_flag, NoError, 248,                           &
     &               "ROMS/Utility/get_state.F")) RETURN
!
!  If restart, read in lateral boundary conditions global attribute
!  from restart file and check keyword strings with structure vlues
!  for consistency.
!
      IF (((model.eq.0).or.(model.eq.iNLM)).and.(nrrec(ng).ne.0)) THEN
        CALL lbc_getatt (ng, model, ncINPid, ncname, 'NLM_LBC', LBC)
        IF (FoundError(exit_flag, NoError, 266,                         &
     &                 "ROMS/Utility/get_state.F")) RETURN
      END IF
!
!  Inquire about the input time variable.
!
      CALL netcdf_inq_var (ng, IDmod, ncname,                           &
     &                     ncid = ncINPid,                              &
     &                     MyVarName = TRIM(tvarnam),                   &
     &                     VarID = varid,                               &
     &                     nVarDim =  nvdim,                            &
     &                     nVarAtt = nvatts)
      IF (FoundError(exit_flag, NoError, 279,                           &
     &               "ROMS/Utility/get_state.F")) RETURN
!
!  Allocate input time variable and read its value(s).  Recall that
!  input time variable is a one-dimensional array with one or several
!  values.
!
      mySize=var_Dsize(1)
      IF (.not.allocated(TimeVar)) allocate (TimeVar(mySize))
      CALL netcdf_get_time (ng, IDmod, ncname, TRIM(tvarnam),           &
     &                      Rclock%DateNumber, TimeVar,                 &
     &                      ncid = ncINPid)
      IF (FoundError(exit_flag, NoError, 291,                           &
     &               "ROMS/Utility/get_state.F")) RETURN
!
!  If using the latest time record from input NetCDF file as the
!  initialization record, assign input time.
!
      IF (LastRec(ng)) THEN
        Tmax=-1.0_r8
        DO i=1,mySize
          IF (TimeVar(i).gt.Tmax) THEN
            Tmax=TimeVar(i)
            IniRec=i
          END IF
        END DO
        INPtime=Tmax
        InpRec=IniRec
      ELSE
        IF ((IniRec.ne.0).and.(IniRec.gt.mySize)) THEN
          IF (Master)  WRITE (stdout,30) string, IniRec, TRIM(ncname),  &
     &                                   mySize
          exit_flag=2
          RETURN
        END IF
        IF (IniRec.ne.0) THEN
          InpRec=IniRec
        ELSE
          InpRec=1
        END IF
        INPtime=TimeVar(InpRec)
      END IF
      IF (allocated(TimeVar)) deallocate ( TimeVar )
!
!  Set input time scale by looking at the "units" attribute.
!
      time_scale=0.0_dp
      DO i=1,nvatts
        IF (TRIM(var_Aname(i)).eq.'units') THEN
          IF (INDEX(TRIM(var_Achar(i)),'day').ne.0) THEN
            time_scale=day2sec
          ELSE IF (INDEX(TRIM(var_Achar(i)),'second').ne.0) THEN
            time_scale=1.0_dp
          END IF
        END IF
      END DO
      IF (time_scale.gt.0.0_r8) THEN
        INPtime=INPtime*time_scale
      END IF
!
!  Set starting time index and time clock in days.  Notice that the
!  global time variables and indices are only over-written when
!  processing initial conditions (msg = 1).
!
      IF ((model.eq.0).or.(model.eq.iNLM).or.                           &
     &    (model.eq.iTLM).or.(model.eq.iRPM)) THEN
        IF (((model.eq.iTLM).or.(model.eq.iRPM)).and.(msg.eq.1).and.    &
     &      (INPtime.ne.(dstart*day2sec))) THEN
          INPtime=dstart*day2sec
        END IF
        IF (msg.eq.1) THEN            ! processing initial conditions
          time(ng)=INPtime
          tdays(ng)=time(ng)*sec2day
          ntstart(ng)=NINT((time(ng)-dstart*day2sec)/dt(ng))+1
          IF (ntstart(ng).lt.1) ntstart(ng)=1
          ntend(ng)=ntstart(ng)+ntimes(ng)-1
          IF (PerfectRST(ng)) THEN
            ntfirst(ng)=1
          ELSE
            ntfirst(ng)=ntstart(ng)
          END IF
        END IF
      ELSE IF (model.eq.iADM) THEN
        IF ((msg.eq.1).and.(INPtime.eq.0.0_r8)) THEN
          INPtime=time(ng)
        ELSE IF (msg.ne.1) THEN
          time(ng)=INPtime
          tdays(ng)=time(ng)*sec2day
        END IF
        ntstart(ng)=ntimes(ng)+1
        ntend(ng)=1
        ntfirst(ng)=ntend(ng)
      END IF
      CALL time_string (time(ng), time_code(ng))
!
!  Over-write "IniRec" to the actual initial record processed.
!
      IF (model.eq.iNLM) THEN
        IniRec=InpRec
      END IF
!
!  Set current input time, io_time .  Notice that the model time,
!  time(ng), is reset above.  This is a THREADPRIVATE variable in
!  shared-memory and this routine is only processed by the MASTER
!  thread since it is an I/O routine. Therefore, we need to update
!  time(ng) somewhere else in a parallel region. This will be done
!  with io_time variable.
!
      io_time=INPtime
!
!  Report information.
!
      lstr=SCAN(ncname,'/',BACK=.TRUE.)+1
      lend=LEN_TRIM(ncname)
      IF (Master) THEN
        CALL time_string (INPtime, t_code)
        IF ((10.le.model).and.(model.le.17)) THEN
          t_code=' '              ! time is meaningless for these fields
        END IF
        WRITE (Tstring,'(f15.4)') tdays(ng)
        IF (ERend.gt.ERstr) THEN
          WRITE (stdout,40) string, 'Reading '//TRIM(StateMsg(msg)),    &
     &                      t_code, ng, Nrun, TRIM(ADJUSTL(Tstring)),   &
     &                      ncname(lstr:lend), InpRec, Tindex
        ELSE
          WRITE (stdout,50) string, 'Reading '//TRIM(StateMsg(msg)),    &
     &                      t_code, ng, TRIM(ADJUSTL(Tstring)),         &
     &                      ncname(lstr:lend), InpRec, Tindex
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Read in nonlinear state variables. If applicable, read in perfect
!  restart variables.
!-----------------------------------------------------------------------
!
      NLM_STATE: IF ((model.eq.iNLM).or.(model.eq.0)) THEN
!
!  Read in nonlinear free-surface (m).
!
        IF (get_var(idFsur)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idFsur)),   &
     &                        varid)
          IF (foundit) THEN
            IF (Perfect2D) THEN
              gtype=var_flag(varid)*r3dvar
            ELSE
              gtype=var_flag(varid)*r2dvar
            END IF
            IF (Perfect2D) THEN
              status=nf_fread3d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idFsur), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, 3,               &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % rmask,                       &
     &                          OCEAN(ng) % zeta)
            ELSE
              status=nf_fread2d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idFsur), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % rmask,                       &
     &                          OCEAN(ng) % zeta(:,:,Tindex))
            END IF
            IF (FoundError(status, nf90_noerr, 610,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idFsur)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idFsur)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idFsur)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 630,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear RHS of free-surface.
!
        IF (get_var(idRzet).and.Perfect2D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idRzet)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*r3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idRzet), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 1, 2,                 &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        OCEAN(ng) % rzeta)
            IF (FoundError(status, nf90_noerr, 653,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idRzet)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idRzet)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idRzet)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 673,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear 2D U-momentum component (m/s).
!
        IF (get_var(idUbar)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idUbar)),   &
     &                        varid)
          IF (foundit) THEN
            IF (Perfect2D) THEN
              gtype=var_flag(varid)*u3dvar
            ELSE
              gtype=var_flag(varid)*u2dvar
            END IF
            IF (Perfect2D) THEN
              status=nf_fread3d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idUbar), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, 3,               &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % umask,                       &
     &                          OCEAN(ng) % ubar)
            ELSE
              status=nf_fread2d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idUbar), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % umask,                       &
     &                          OCEAN(ng) % ubar(:,:,Tindex))
            END IF
            IF (FoundError(status, nf90_noerr, 712,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idUbar)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idUbar)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idUbar)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 732,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear RHS of 2D U-momentum component.
!
        IF (get_var(idRu2d).and.Perfect2D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idRu2d)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*u3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idRu2d), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 1, 2,                 &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask,                         &
     &                        OCEAN(ng) % rubar)
            IF (FoundError(status, nf90_noerr, 755,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idRu2d)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idRu2d)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idRu2d)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 775,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear 2D U-momentum component (m/s).
!
        IF (get_var(idVbar)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idVbar)),   &
     &                        varid)
          IF (foundit) THEN
            IF (Perfect2D) THEN
              gtype=var_flag(varid)*v3dvar
            ELSE
              gtype=var_flag(varid)*v2dvar
            END IF
            IF (Perfect2D) THEN
              status=nf_fread3d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idVbar), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, 3,               &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % vmask,                       &
     &                          OCEAN(ng) % vbar)
            ELSE
              status=nf_fread2d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idVbar), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % vmask,                       &
     &                          OCEAN(ng) % vbar(:,:,Tindex))
            END IF
            IF (FoundError(status, nf90_noerr, 814,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idVbar)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idVbar)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idVbar)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 834,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear RHS 2D U-momentum component.
!
        IF (get_var(idRv2d).and.Perfect2D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idRv2d)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*v3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idRv2d), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 1, 2,                 &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask,                         &
     &                        OCEAN(ng) % rvbar)
            IF (FoundError(status, nf90_noerr, 857,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idRv2d)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idRv2d)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idRv2d)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 877,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear 3D U-momentum component (m/s).
!
        IF (get_var(idUvel)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idUvel)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*u3dvar
            IF (Perfect3D) THEN
              status=nf_fread4d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idUvel), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, N(ng), 1, 2,     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % umask,                       &
     &                          OCEAN(ng) % u)
            ELSE
              status=nf_fread3d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idUvel), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % umask,                       &
     &                          OCEAN(ng) % u(:,:,:,Tindex))
            END IF
            IF (FoundError(status, nf90_noerr, 914,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idUvel)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idUvel)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idUvel)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 934,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear RHS of 3D U-momentum component.
!
        IF (get_var(idRu3d).and.Perfect3D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idRu3d)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*u3dvar
            status=nf_fread4d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idRu3d), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng), 1, 2,       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask,                         &
     &                        OCEAN(ng) % ru)
            IF (FoundError(status, nf90_noerr, 957,                     &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idRu3d)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idRu3d)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idRu3d)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 977,                  &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear 3D V-momentum component (m/s).
!
        IF (get_var(idVvel)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idVvel)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*v3dvar
            IF (Perfect3D) THEN
              status=nf_fread4d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idVvel), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, N(ng), 1, 2,     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % vmask,                       &
     &                          OCEAN(ng) % v)
            ELSE
              status=nf_fread3d(ng, IDmod, ncname, ncINPid,             &
     &                          Vname(1,idVvel), varid,                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % vmask,                       &
     &                          OCEAN(ng) % v(:,:,:,Tindex))
            END IF
            IF (FoundError(status, nf90_noerr, 1012,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idVvel)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idVvel)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idVvel)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1032,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear RHS of 3D V-momentum component.
!
        IF (get_var(idRv3d).and.Perfect3D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idRv3d)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*v3dvar
            status=nf_fread4d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idRv3d), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng), 1, 2,       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask,                         &
     &                        OCEAN(ng) % rv)
            IF (FoundError(status, nf90_noerr, 1055,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idRv3d)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idRv3d)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idRv3d)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1075,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in nonlinear tracer type variables.
!
        DO itrc=1,NT(ng)
          IF (get_var(idTvar(itrc))) THEN
            foundit=find_string(var_name, n_var,                        &
     &                          TRIM(Vname(1,idTvar(itrc))), varid)
            IF (foundit) THEN
              gtype=var_flag(varid)*r3dvar
              IF (Perfect3D) THEN
                status=nf_fread4d(ng, IDmod, ncname, ncINPid,           &
     &                            Vname(1,idTvar(itrc)), varid,         &
     &                            InpRec, gtype, Vsize,                 &
     &                            LBi, UBi, LBj, UBj, 1, N(ng), 1, 2,   &
     &                            Fscl, Fmin, Fmax,                     &
     &                            GRID(ng) % rmask,                     &
     &                            OCEAN(ng) % t(:,:,:,:,itrc))
              ELSE
                status=nf_fread3d(ng, IDmod, ncname, ncINPid,           &
     &                            Vname(1,idTvar(itrc)), varid,         &
     &                            InpRec, gtype, Vsize,                 &
     &                            LBi, UBi, LBj, UBj, 1, N(ng),         &
     &                            Fscl, Fmin, Fmax,                     &
     &                            GRID(ng) % rmask,                     &
     &                            OCEAN(ng) % t(:,:,:,Tindex,itrc))
              END IF
              IF (FoundError(status, nf90_noerr, 1111,                  &
     &                       "ROMS/Utility/get_state.F")) THEN
                IF (Master) THEN
                  WRITE (stdout,60) string, TRIM(Vname(1,idTvar(itrc))),&
     &                              InpRec, TRIM(ncname)
                END IF
                exit_flag=2
                ioerror=status
                RETURN
              ELSE
                IF (Master) THEN
                  WRITE (stdout,70) TRIM(Vname(2,idTvar(itrc))),        &
     &                              Fmin, Fmax
                END IF
              END IF
            ELSE
              IF (Master) THEN
                WRITE (stdout,80) string, TRIM(Vname(1,idTvar(itrc))),  &
     &                            TRIM(ncname)
              END IF
              exit_flag=4
              IF (FoundError(exit_flag, nf90_noerr, 1132,               &
     &                       "ROMS/Utility/get_state.F")) THEN
                RETURN
              END IF
            END IF
          END IF
        END DO
!
!  Read in vertical viscosity.
!
        IF (have_var(idVvis)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idVvis)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idVvis), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng),             &
     &                        Fscl, Fmin,Fmax,                          &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % AKv)
            IF (FoundError(status, nf90_noerr, 1313,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idVvis)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idVvis)), Fmin, Fmax
              END IF
            END IF
            CALL mp_exchange3d (ng, MyRank, IDmod, 1,                   &
     &                          LBi, UBi, LBj, UBj, 0, N(ng),           &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          MIXING(ng) % AKv)
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idVvis)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1340,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in temperature vertical diffusion.
!
        IF (have_var(idTdif)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idTdif)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idTdif), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng),             &
     &                        Fscl, Fmin,Fmax,                          &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % AKt(:,:,:,itemp))
            IF (FoundError(status, nf90_noerr, 1363,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idTdif)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idTdif)), Fmin, Fmax
              END IF
            END IF
            CALL mp_exchange3d (ng, MyRank, IDmod, 1,                   &
     &                          LBi, UBi, LBj, UBj, 0, N(ng),           &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          MIXING(ng) % AKt(:,:,:,itemp))
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idTdif)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1390,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in salinity vertical diffusion.
!
        IF (have_var(idSdif)) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idSdif)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idSdif), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng),             &
     &                        Fscl, Fmin,Fmax,                          &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % AKt(:,:,:,isalt))
            IF (FoundError(status, nf90_noerr, 1414,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idSdif)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idSdif)), Fmin, Fmax
              END IF
            END IF
            CALL mp_exchange3d (ng, MyRank, IDmod, 1,                   &
     &                          LBi, UBi, LBj, UBj, 0, N(ng),           &
     &                          NghostPoints,                           &
     &                          EWperiodic(ng), NSperiodic(ng),         &
     &                          MIXING(ng) % AKt(:,:,:,isalt))
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idSdif)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1441,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in turbulent kinetic energy.
!
        IF (get_var(idMtke).and.Perfect3D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idMtke)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread4d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idMtke), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng), 1, 2,       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % tke)
            IF (FoundError(status, nf90_noerr, 1605,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idMtke)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idMtke)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idMtke)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1625,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in turbulent kinetic energy time length scale.
!
        IF (get_var(idMtls).and.Perfect3D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idMtls)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread4d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idMtls), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng), 1, 2,       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % gls)
            IF (FoundError(status, nf90_noerr, 1648,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idMtls)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
             RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idMtls)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idMtls)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1668,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in vertical mixing turbulent length scale.
!
        IF (get_var(idVmLS).and.Perfect3D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idVmLS)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idVmLS), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng),             &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % Lscale)
            IF (FoundError(status, nf90_noerr, 1691,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idVmLS)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idVmLS)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idVmLS)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1711,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in turbulent kinetic energy vertical diffusion coefficient.
!
        IF (get_var(idVmKK).and.Perfect3D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idVmKK)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idVmKK), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng),             &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % Akk)
            IF (FoundError(status, nf90_noerr, 1734,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idVmKK)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idVmKK)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idVmKK)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1754,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
!  Read in turbulent length scale vertical diffusion coefficient.
!
        IF (get_var(idVmKP).and.Perfect3D) THEN
          foundit=find_string(var_name, n_var, TRIM(Vname(1,idVmKP)),   &
     &                        varid)
          IF (foundit) THEN
            gtype=var_flag(varid)*w3dvar
            status=nf_fread3d(ng, IDmod, ncname, ncINPid,               &
     &                        Vname(1,idVmKP), varid,                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 0, N(ng),             &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        MIXING(ng) % Akp)
            IF (FoundError(status, nf90_noerr, 1778,                    &
     &                     "ROMS/Utility/get_state.F")) THEN
              IF (Master) THEN
                WRITE (stdout,60) string, TRIM(Vname(1,idVmKP)),        &
     &                            InpRec, TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,70) TRIM(Vname(2,idVmKP)), Fmin, Fmax
              END IF
            END IF
          ELSE
            IF (Master) THEN
              WRITE (stdout,80) string, TRIM(Vname(1,idVmKP)),          &
     &                          TRIM(ncname)
            END IF
            exit_flag=4
            IF (FoundError(exit_flag, nf90_noerr, 1798,                 &
     &                     "ROMS/Utility/get_state.F")) THEN
              RETURN
            END IF
          END IF
        END IF
!
      END IF NLM_STATE
!
!
!-----------------------------------------------------------------------
!  Close input NetCDF file.
!-----------------------------------------------------------------------
!
      CALL netcdf_close (ng, IDmod, ncINPid, ncname, .FALSE.)
!
!  Turn off time wall clock.
!
      CALL wclock_off (ng, IDmod, 42, 7613, "ROMS/Utility/get_state.F")
!
  10  FORMAT (/,a,'GET_STATE - unable to open input NetCDF file: ',a)
  20  FORMAT (/,a,'GET_STATE - Warning - NetCDF global attribute: ',a,  &
     &        /,18x,'for lateral boundary conditions not checked',      &
     &        /,18x,'in restart file: ',a)
  30  FORMAT (/,a,'GET_STATE - requested input time record = ',i3,/,    &
     &        18x,'not found in input NetCDF: ',a,/,                    &
     &        18x,'number of available records = ',i3)
  40  FORMAT (/,a,'GET_STATE - ',a,t75,a,                               &
     &        /,19x,'(Grid ',i2.2,', Iter=',i4.4, ', t = ',a,           &
     &        ', File: ',a, ', Rec=',i4.4,', Index=',i1,')')
  50  FORMAT (/,a,'GET_STATE - ',a,t75,a,                               &
     &        /,19x,'(Grid ',i2.2, ', t = ',a,                          &
     &        ', File: ',a,', Rec=',i4.4, ', Index=',i1,')')
  60  FORMAT (/,a,'GET_STATE - error while reading variable: ',a,2x,    &
     &        'at time record = ',i3,/,18x,'in input NetCDF file: ',a)
  70  FORMAT (16x,'- ',a,/,19x,'(Min = ',1p,e15.8,                      &
     &        ' Max = ',1p,e15.8,')')
  80  FORMAT (/,a,'GET_STATE - cannot find variable: ',a,               &
     &        /,18x,'in input NetCDF file: ',a)
      RETURN
      END SUBROUTINE get_state
