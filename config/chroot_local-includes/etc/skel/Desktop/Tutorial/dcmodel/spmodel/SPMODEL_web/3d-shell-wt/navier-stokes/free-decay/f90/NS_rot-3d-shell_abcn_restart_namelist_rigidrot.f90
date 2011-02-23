!--------------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!ɽ��  ��ž��̥ʥӥ������ȡ�����ή�Υ�ǥ� : 
!      2nd order Adams-Bashforth + Crank Nicholson scheme
!      ���β�ž���������
!
!����  2005/02/11  �ݹ� ����   mhd-boussinesq ��ǥ븵�˴�ά��
!      2005/02/19  �ݹ� ����   ���β�ž������Ѥ˲�¤
!      2008/08/10  �ݹ� ����   ����ź�����ѹ� im -> 0:im-1
!
program NS_rot_3d_shell_abcn_restart_namelist

  use lumatrix
  use wt_module
  use dc_message, only : MessageNotify
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  ! T10 N8
  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)
  ! T10 N16
  !integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)
  !! T21 N16
  !integer,parameter  :: im=64, jm=32, km=16  ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=21, lm=16         ! �����ȿ�������(��ʿ, ư��)
  !! T21 N24
  !integer,parameter  :: im=64, jm=32, km=24  ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=21, lm=24         ! �����ȿ�������(��ʿ, ư��)
  !! T21 N32
  !integer,parameter  :: im=64, jm=32, km=32  ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=21, lm=32         ! �����ȿ�������(��ʿ, ư��)
  !! T42 N32
  !integer,parameter  :: im=128, jm=64, km=32 ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=42, lm=32         ! �����ȿ�������(��ʿ, ư��)
  !! T42 N48
  !integer,parameter  :: im=128, jm=64, km=48 ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=42, lm=48         ! �����ȿ�������(��ʿ, ư��)
  !  T85 N32
  !integer,parameter :: im=256, jm=128, km=32 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=85, lm=32          ! �����ȿ�������(��ʿ, ư��)
  !  T85 N48
  !integer,parameter :: im=256, jm=128, km=48 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=85, lm=48          ! �����ȿ�������(��ʿ, ư��)
  !  T85 N64
  !integer,parameter :: im=256, jm=128, km=64 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=85, lm=64          ! �����ȿ�������(��ʿ, ư��)
  !  T170 N48
  !integer,parameter :: im=512, jm=256, km=48 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=170, lm=48         ! �����ȿ�������(��ʿ, ư��)
  !  T170 N64
  !integer,parameter :: im=512, jm=256, km=64 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=170, lm=64         ! �����ȿ�������(��ʿ, ư��)
  !  T341 N80
  !integer,parameter :: im=1024, jm=512, km=80 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=341, lm=80          ! �����ȿ�������(��ʿ, ư��)

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Torvel      ! �ȥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Torvel       ! �ȥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polvel      ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Polvel       ! �ݥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_LaplaPolvel ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaPolvel  ! ��^2�ݥ�����®��

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Vlon       ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Vlat       ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Vrad       ! ®��(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Zlon       ! ����(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Zlat       ! ����(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Zrad       ! ����(ư��)

 !---- ������ʬ���ѿ� ----

 ! �ȥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTorvel1, wt_DtDTorvel0
 ! ��^2�ݥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDLaplaPolvel1, wt_DtDLaplaPolvel0

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(®�پ�)
  real(8), dimension((nm+1)*(nm+1),0:lm,0:lm)   :: DifLUMT_Vel
  integer, dimension((nm+1)*(nm+1),0:lm)        :: kpivot_Vel     ! �ԥܥå�

 !---- ����¾ ----
  real(8), parameter :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time

 !---- NAMELIST �ѿ� ----
  logical            :: verbose=.false.          ! ���ϥ�å�������٥�
  namelist /message/  verbose                    !
                                                 !-- ��ɸ�ѿ��ʤ� --
  real(8)            :: ri=0.0D0                 ! ��Ⱦ��
  real(8)            :: ro=0.0D00                ! ��Ⱦ��
  real(8)            :: eta=0.0D0                ! �⳰Ⱦ����
  namelist /radius/  ri, ro, eta
                                                 ! -- ����� --
  character(len=100) :: initial_file=''          ! ����ͥǡ����ե�����̾
  real               :: initial_time=0.0         ! �������
  namelist /initial/ initial_file, initial_time  ! 
                                                 ! -- ������ʬ --
  real(8) :: delta_t=1.0e-7                      ! ������ʬ���
  integer :: nstep=2000                          ! ������ʬ���ƥå׿�
  namelist /tint/    delta_t, nstep              ! 
                                                 ! -- ���� --
  character(len=100) :: output_file=''           ! ���ϥե�����̾
  character(len=100) :: title = &                ! �����ȥ�
            'Navier-Stokes equation in a 3-dim spherical shell'!
  integer :: ndisp=200                           ! ���ϴֳ֥��ƥå׿�
  namelist /output/    output_file, title, ndisp ! 
                                                 ! -- ʪ���ѥ�᥿�� --
  real(8)            :: Rs=0.0D0                 ! ���ӡ���
  real(8)            :: Re=0.0D0                 ! �쥤�Υ륺��
  real(8)            :: RsInv=0.0D0              ! 1/���ӡ���
  real(8)            :: ReInv=0.0D0              ! 1/�쥤�Υ륺��
  namelist /physics/  Rs, Re                     ! 
                                                 ! -- ������� --
  character(len=2)   :: VelBC='FF'               ! ®�ٶ������(RR/RF/FR/FF)
  namelist /boundary/ VelBC                      !
                                                 ! -- ������ --
  real(8)            :: Omega=1.0D0              ! ��ž��®��
  real(8)            :: LonOmega=0.0D0           ! ��ž������
  real(8)            :: LatOmega=90.0D0          ! ��ž������
  namelist /rotation/ Omega, LonOmega, LatOmega  !

 !---------------- NAMELIST �ɤ߹��� ---------------------
  write(6,nml=message)  ; read(5,nml=message) ; write(6,nml=message)

  if (verbose) write(6,nml=radius) ; read(5,nml=radius)
   !
   ! ri,ro �� eta �Τɤ��餫�� NAMELIST radius ��Ϳ����. eta �����꤬ͥ�褵���
   !
  if ( eta /= 0.0D0 ) then
     ri=eta/(1.0D0-eta) ; ro=1.0D0/(1.0D0-eta)
     call MessageNotify('M','main', &
             'The inner/outer radii were calculatied with their ratio, eta.')
  else if ( ro /= 0.0D0 .and. ro /= 0.0D0 ) then
     eta = ri/ro
     call MessageNotify('M','main', &
             'The ratio of inner/outer radii were calculatied with ro and ri.')
  else
     call MessageNotify('E','main', &
                 'The inner/outer radii or their ratio should be given')
  endif
  write(6,nml=radius)

  if(verbose)write(6,nml=initial) ; read(5,nml=initial) ;write(6,nml=initial)
  if(verbose)write(6,nml=tint)    ; read(5,nml=tint)    ;write(6,nml=tint)
  if(verbose)write(6,nml=output)  ; read(5,nml=output)  ;write(6,nml=output)

  if(verbose)write(6,nml=physics) ; read(5,nml=physics)
  if ( Rs == 0.0D0 ) then
     RsInv = 0.0D0
     call MessageNotify('M','main', &
             'Inverse of Rossby Number is set to zero.')
  else
     RsInv = 1.0D0/Rs
  endif
  if ( Re == 0.0D0 ) then
     ReInv = 0.0D0
     call MessageNotify('M','main', &
             'Inverse of Reynolds Number is set to zero.')
  else
     ReInv = 1.0D0/Re
  endif
  write(6,nml=physics)

  if(verbose)write(6,nml=boundary); read(5,nml=boundary);write(6,nml=boundary)
  if(verbose)write(6,nml=rotation); read(5,nml=rotation);write(6,nml=rotation)

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !------------------- ��������� ----------------------
  if ( initial_file == "") then
     call set_initial_values( xyz_Torvel, xyz_Polvel )

  else
     call initial_read_gtool4(initial_file, initial_time, &
          xyz_Torvel, xyz_Polvel, wt_DtDTorvel1, wt_DtDLaplaPolvel1 )
  endif

  time = initial_time
  
  wt_Torvel = wt_xyz(xyz_Torvel) 
  wt_Polvel = wt_xyz(xyz_Polvel)
  wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
  xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)

  call wt_Potential2Vector(xyz_Vlon, xyz_Vlat, xyz_Vrad, wt_Torvel, wt_Polvel)

  if ( initial_file == "") then
     call TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          wt_DtDTorvel1, wt_DtDLaplaPolvel1 )
  endif

  call CNDiffusionMatrix( ReInv, delta_t, DifLUMT_Vel, kpivot_Vel )

 !------------------- ������ʬ(Adams-Bashforth ˡ) --------------------
  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! ������Ϳ��������ͤϽ���

  do it=1,nstep
     time = initial_time + it * delta_t

     ! �Τλ����Ѳ������¸
     wt_DtDTorvel0 = wt_DtDTorvel1
     wt_DtDLaplaPolvel0 = wt_DtDLaplaPolvel1

     call TimeDerivetives_noDiffusion( &
       wt_Torvel, wt_Polvel, wt_LaplaPolvel, &
       xyz_Vlon, xyz_Vlat, xyz_Vrad, &
       wt_DtDTorvel1, wt_DtDLaplaPolvel1 )

     ! �ȥ�����®�پ�
     wt_Torvel = wt_Torvel + delta_t *( 3/2.0*wt_DtDTorvel1 - 1/2.0*wt_DtDTorvel0 ) &
                           + delta_t/2.0 * ReInv * wt_Lapla_wt(wt_Torvel) 
     wt_Torvel = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_Torvel)

     ! �ݥ�����®�پ�
     wt_LaplaPolvel = wt_LaplaPolvel &
           + delta_t *(3/2.0*wt_DtDLaplaPolvel1 - 1/2.0*wt_DtDLaplaPolvel0 ) &
           + delta_t/2.0 * ReInv * wt_Lapla_wt(wt_LaplaPolvel) 
     wt_LaplaPolvel = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_LaplaPolvel)

     ! �������
     call wt_TorBoundariesGrid(wt_Torvel,cond=velBC)
     wt_Polvel = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=velBC))
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)

     ! ����å��ͷ׻�
     xyz_Torvel = xyz_wt(wt_Torvel)
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)

     call wt_Potential2Vector(xyz_Vlon,xyz_Vlat,xyz_Vrad,wt_Torvel,wt_Polvel)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

! �ʾ� �ᥤ��ץ���� 
!--------------------------------------------------------------------------------
! �ʲ� ���֥롼����
  !------------------- �����Ѳ���(�Ȼ���ʳ�) ----------------------
  subroutine TimeDerivetives_noDiffusion( &
       wt_Torvel, wt_Polvel, wt_LaplaPolvel, &
       xyz_Vlon, xyz_Vlat, xyz_Vrad, &
       wt_DtDTorvel, wt_DtDLaplaPolvel )

   ! �ȥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Torvel
   ! �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Polvel
   ! ��^2 �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_LaplaPolvel
   ! ����

    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Vlon   ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Vlat   ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Vrad   ! ®��(ư��)

   ! �ȥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDTorvel
   ! ��^2 �ݥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDLaplaPolvel

    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradxyz_Vlon   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradxyz_Vlat   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradxyz_Vrad   ! v����v(ư��)

   ! v����v ��������׻�
    call wt_VGradV(xyz_VGradxyz_Vlon,xyz_VGradxyz_Vlat,xyz_VGradxyz_Vrad, &
                   xyz_Vlon, xyz_Vlat, xyz_Vrad )

  ! �ȥ���������Ѳ�
    wt_DtDTorvel = &
           wt_L2Inv_wt(  RsInv * wt_KxRGrad_wt(wt_Torvel)   &
                       - RsInv * wt_Qoperator_wt(wt_Polvel) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_VGradxyz_Vlon,xyz_VGradxyz_Vlat )) 
  ! �ݥ���������Ѳ�
    wt_DtDLaplaPolvel = &
           wt_L2Inv_wt(  RsInv * wt_KxRGrad_wt(wt_LaplaPolvel) &
                       + RsInv * wt_Qoperator_wt(wt_Torvel) )  &
         + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(                 &
               xyz_VGradxyz_Vlon, xyz_VGradxyz_Vlat, xyz_VGradxyz_Vrad ))

  end subroutine TimeDerivetives_noDiffusion

 !------------------- �Ȼ��� ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
    real(8), dimension((nm+1)*(nm+1),0:lm,0:lm), intent(OUT)  :: DiffLUMatrix
    ! �ԥܥåȾ���
    integer, dimension((nm+1)*(nm+1),0:lm), intent(OUT)       :: kpivot

    ! ������ѿ�
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wt_I

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       DiffLUMatrix(:,:,l) = wt_I - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- ���������(�ե�����ʤ�) ----------------------
  subroutine set_initial_values(xyz_Torvel, xyz_Polvel )

    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Torvel ! �ȥ�����®��
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Polvel ! �ݥ�����®��

    real(8), parameter  :: pi=3.1415926535897932385D0

    ! ®�پ�

   ! ���β�ž��
    xyz_Torvel =  Omega * xyz_Rad                                &
                   * ( cos(LatOmega/180.0*pi)*cos(xyz_Lat)       &
                         *cos(LonOmega/180.0D0*pi-xyz_Lon)       &
                       + sin(LatOmega/180.0D0*pi)*sin(xyz_Lat) )
    xyz_Polvel =  0.0D0

  end subroutine set_initial_values

 !------------------- ���������(�ե����뤫��) ----------------------  
  subroutine initial_read_gtool4( filename, time, &
       xyz_Torvel, xyz_Polvel, wt_DtDTorvel, wt_DtDLaplaPolvel )


    character(len=*), intent(in)   :: filename
    real, intent(in)               :: time
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Torvel  ! �ȥ�����®��
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Polvel  ! �ݥ�����®��
 ! �ȥ�����®�ٻ����Ѳ� 
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDTorvel
 ! ��^2�ݥ�����®�ٻ����Ѳ�
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDLaplaPolvel

 ! �ɤ߹���������
    real(8), dimension(0:im-1,jm,0:km) :: xyz_DtDVar

    call HistoryGet( trim(filename), 'torvel', xyz_Torvel, time )
    call HistoryGet( trim(filename), 'polvel', xyz_Polvel, time )

    call HistoryGet( trim(filename), 'DtDTorvel', xyz_DtDVar, time )
      wt_DtDTorvel = wt_xyz(xyz_DtDVar)
    call HistoryGet( trim(filename), 'DtDLaplaPolvel', xyz_DtDVar, time )
      wt_DtDLaplaPolvel = wt_xyz(xyz_DtDVar)

  end subroutine initial_read_gtool4

 !------------------- ���� ----------------------
  subroutine output_gtool4_init

   !---- �ҥ��ȥ꡼�ե�������� ----
    call HistoryCreate( &
           file=trim(output_file), &
           title=trim(title), &
           source='NS_rot-3d-shell_abcn_restart_namelist.f90 (2008/08/10)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t), &
           xtypes=(/'double','double','double','double'/))

   !---- ��ɸ�ѿ����, ���� ----
    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryAddattr('lon','topology','circular')          ! ����°��
    call HistoryAddattr('lon','modulo',360.0)                 ! ����°��
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����

   !---- ʪ���ѿ���� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='torvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='laplapolvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='laplacian poloidal velocity potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')

   !---- �����Ѳ������ ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDTorvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of velocity toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDLaplaPolvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of laplacian of velocity poloidal potential', &
           units='1', xtype='double')

   !---- ����¾��������� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='zlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='zlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='zrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-voticity', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='ek', dims=(/'t  '/), & 
           longname='mean kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='hel', dims=(/'lon','lat','rad','t  '/), & 
           longname='helicity', units='1', xtype='double')

   !---- �¸��ѥ�᥿����°���Ȥ������, ����(���� Global °��) ----
    call HistoryAddAttr('lon','+Radius', (/ri,ro,eta/) )
    call HistoryAddAttr('lon','+delta_t', delta_t  )

    call HistoryAddAttr('lon', '+Rossby',   Rs    )
    call HistoryAddAttr('lon', '+Reynolds', Re    )

    call HistoryAddAttr('lon', '+VelBoundary',  VelBC  )

   end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- ʪ���ѿ����� ----
    call HistoryPut('torvel',xyz_Torvel)
    call HistoryPut('polvel',xyz_Polvel)
    call HistoryPut('laplapolvel',xyz_LaplaPolvel)
    call HistoryPut('vlon',xyz_Vlon)
    call HistoryPut('vlat',xyz_Vlat)
    call HistoryPut('vrad',xyz_Vrad)

   !---- �����Ѳ������ ----
    call HistoryPut('DtDTorvel',xyz_wt(wt_DtDTorvel1))
    call HistoryPut('DtDLaplaPolvel',xyz_wt(wt_DtDLaplaPolvel1))

   !---- �����̽��� ----
    xyz_Zlon = xyz_RotLon_wt_wt(wt_xyz(xyz_Vrad),wt_xyz(xyz_Vlat))
    xyz_Zlat = xyz_RotLat_wt_wt(wt_xyz(xyz_Vlon),wt_xyz(xyz_Vrad))
    xyz_Zrad = xyz_wt(wt_RotRad_xyz_xyz(xyz_Vlat,xyz_Vlon))

    call HistoryPut('zlon',xyz_Zlon)
    call HistoryPut('zlat',xyz_Zlat)
    call HistoryPut('zrad',xyz_Zrad)

    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_Vlon**2+xyz_Vlat**2+xyz_Vrad**2)/2))
    call HistoryPut('um',yz_avrLon_xyz(xyz_Vlon))
    call HistoryPut('hel',xyz_Vlon*xyz_Zlon + xyz_Vlat*xyz_Zlat + xyz_Vrad*xyz_Zrad)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program NS_rot_3d_shell_abcn_restart_namelist
