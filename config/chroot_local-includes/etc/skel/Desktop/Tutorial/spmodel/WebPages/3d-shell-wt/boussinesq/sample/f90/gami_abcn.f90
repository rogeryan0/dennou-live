!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ��ž��̥֥��ͥ���ή�Υ�ǥ� : Adams-Bashforth + Crank Nicholson scheme
!
!����  2002/09/04  �ݹ�����
!      2004/02/16  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2008/08/13  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!
program gami_abcn

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  !real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ�� \eta=0.3
  real(8),parameter  :: ri=1.0, ro=2.0       ! �⳰Ⱦ�� \eta=0.5

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Torvel      ! �ȥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Torvel       ! �ȥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polvel      ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Polvel       ! �ݥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_LaplaPolvel ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaPolvel  ! ��^2�ݥ�����®��

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp   ! ����
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp    ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_U      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_V      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_W      ! ®��(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Q      ! Ǯ��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Q       ! Ǯ��

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! ������
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! ������

 !---- ������ʬ���ѿ� ----

 ! �ȥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTorvel1, wt_DtDTorvel0
 ! ��^2�ݥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDLaplaPolvel1, wt_DtDLaplaPolvel0
 ! ���ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTemp1, wt_DtDTemp0

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(®�پ�)
  real(8), dimension((nm+1)*(nm+1),0:lm,0:lm)   :: DifLUMT_Vel
  integer, dimension((nm+1)*(nm+1),0:lm)        :: kpivot_Vel     ! �ԥܥå�
 ! Crank Nicholson ��Ū�׻��ѳȻ�����(����)
  real(8), dimension((nm+1)*(nm+1),0:lm,0:lm)   :: DifLUMT_Temp
  integer, dimension((nm+1)*(nm+1),0:lm)        :: kpivot_Temp    ! �ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=2e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=250, ndisp=50        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: velbc='FF'     ! ®�ٶ������(RR/RF/FR/FF)
  character(len=2), parameter :: Tempbc='DD'    ! ���ٶ������(DD/DN/ND/NN)

  real(8), parameter :: Ra=1e3                  ! �쥤�꡼��
  real(8), parameter :: Pr=1.0                  ! �ץ��ɥ��
  real(8), parameter :: Ta=1.0d4                ! �ƥ��顼��

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: Lon1=pi                 ! ���ʬ�۰��ٺ�ɸ
  real(8), parameter :: Lat1=0.0                ! ���ʬ�۷��ٺ�ɸ
  real(8), parameter :: Rad1=(ri+ro)/2.0        ! ���ʬ��ư�º�ɸ
  real(8), parameter :: Sigma=0.05               ! ���ʬ�ۤ��礭��

 !---- ����¾ ----
  integer :: it
  real(8) :: Tau                                ! ��ƥ��顼��

  Tau = sqrt(Ta)

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_initial(im,jm,km,nm,lm,ri,ro)

 !------------------- ��������� ----------------------
  xyz_Temp =  exp(-((xyz_Lon-Lon1)**2+(xyz_Lat-Lat1)**2+(xyz_Rad-Rad1)**2)&
                 /(2*Sigma**2))
  wt_Temp = wt_xyz(xyz_Temp)

  xyz_Q = 0.0
  wt_Q = wt_xyz(xyz_Q)

  xyz_Torvel =  0
  xyz_Polvel =  0
  wt_Torvel = wt_xyz(xyz_Torvel)
  wt_Polvel = wt_xyz(xyz_Polvel)
  wt_LaplaPolvel = wt_Lapla_Wt(wt_Polvel)
  xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)

  call wt_Potential2Vector(xyz_U,xyz_V,xyz_W,wt_Torvel,wt_Polvel)

  call TimeDerivetives_noDiffusion( &
       wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, wt_Q, &
       xyz_U, xyz_V, xyz_W, xyz_Temp, &
       wt_DtDTorvel1, wt_DtDLaplaPolvel1, wt_DtDTemp1 )

  call CNDiffusionMatrix( Pr, dt, DifLUMT_Vel, kpivot_Vel )
  call CNDiffusionMatrix( 1.0D0, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- ���������� ----------------------
  xy_TempBndry(:,:,1) = 0
  xy_TempBndry(:,:,2) = 0

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- ������ʬ(Adams-Bashforth ˡ) --------------------
  call output_gtool4_init
  call output_gtool4

  do it=1,nt
     wt_DtDTorvel0 = wt_DtDTorvel1
     wt_DtDLaplaPolvel0 = wt_DtDLaplaPolvel1
     wt_DtDTemp0   = wt_DtDTemp1

     call TimeDerivetives_noDiffusion( &
         wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, wt_Q, &
         xyz_U, xyz_V, xyz_W, xyz_Temp, &
         wt_DtDTorvel1, wt_DtDLaplaPolvel1, wt_DtDTemp1 )

     ! �ȥ�����®�پ�
     wt_Torvel = wt_Torvel + dt *( 3/2.0*wt_DtDTorvel1 - 1/2.0*wt_DtDTorvel0 ) &
                           + dt/2.0 * Pr * wt_Lapla_wt(wt_Torvel) 
     wt_Torvel = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_Torvel)

     ! �ݥ�����®�پ�
     wt_LaplaPolvel = wt_LaplaPolvel &
           + dt *(3/2.0*wt_DtDLaplaPolvel1 - 1/2.0*wt_DtDLaplaPolvel0 ) &
           + dt/2.0 * Pr * wt_Lapla_wt(wt_LaplaPolvel) 
     wt_LaplaPolvel = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_LaplaPolvel)

     ! ���پ�
     wt_Temp = wt_Temp + dt *( 3/2.0*wt_DtDTemp1 - 1/2.0*wt_DtDTemp0 ) &
                       + dt/2.0 * wt_Lapla_wt(wt_Temp) 
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wt_Temp)

     ! �������
     call wt_TorBoundaries(wt_Torvel,cond=velbc)
     wt_Polvel = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=velbc))
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
     call wt_Boundaries(wt_Temp,w_TempBndry,cond=Tempbc)

     ! ����å��ͷ׻�
     xyz_Torvel = xyz_wt(wt_Torvel)
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
     xyz_Temp = xyz_wt(wt_Temp)

     call wt_Potential2Vector(xyz_U,xyz_V,xyz_W,wt_Torvel,wt_Polvel)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- �����Ѳ��� ----------------------
  subroutine TimeDerivetives_noDiffusion( &
       wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, wt_Q, &
       xyz_U, xyz_V, xyz_W, xyz_Temp, &
       wt_DtDTorvel, wt_DtDLaplaPolvel, wt_DtDTemp )

   ! �ȥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Torvel
   ! �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Polvel
   ! ��^2 �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_LaplaPolvel
   ! ����
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Temp
   ! Ǯ��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Q

    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_U      ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_V      ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_W      ! ®��(ư��)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Temp   ! ����

   ! �ȥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDTorvel
   ! �ݥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDLaplaPolvel
   ! ���ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDTemp

    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lon   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lat   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Rad   ! v����v(ư��)


   ! v����v ��������׻�
    call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                   xyz_U,xyz_V,xyz_W )

  ! �ȥ���������Ѳ�
    wt_DtDTorvel = &
           wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_Torvel)   &
                       - Tau * wt_Qoperator_wt(wt_Polvel) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_VGradV_Lon,xyz_VGradV_Lat)) 

  ! �ݥ���������Ѳ�
    wt_DtDLaplaPolvel = &
         - Ra*Pr*wt_Temp                    &
         + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_LaplaPolvel) &
                       + Tau * wt_Qoperator_wt(wt_Torvel) )  &
         + wt_l2inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
               xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad ))

  ! ���ٻ����Ѳ�
    wt_DtDTemp = &
         - wt_Div_xyz_xyz_xyz(  &
               xyz_U*xyz_Temp,xyz_v*xyz_Temp,xyz_w*xyz_Temp) &
         + wt_L2_wt(wt_Polvel)  &
         + wt_Q

  end subroutine TimeDerivetives_noDiffusion

 !------------------- �����Ѳ��� ----------------------
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

 !------------------- ���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='gami_abcn.nc', &
           title='convection in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryAddattr('lon','topology','circular')          ! ����°��
    call HistoryAddattr('lon','modulo',360.0)                 ! ����°��
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����

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
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
   call HistoryAddVariable( &                                ! �ѿ����
           varname='u', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
   call HistoryAddVariable( &                                ! �ѿ����
           varname='v', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
   call HistoryAddVariable( &                                ! �ѿ����
           varname='w', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')

   call HistoryAddVariable( &                                ! �ѿ����
           varname='ke', dims=(/'t  '/), & 
           longname='kinetic energy', units='1', xtype='double')
   call HistoryAddVariable( &                                ! �ѿ����
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
   end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('torvel',xyz_Torvel)
    call HistoryPut('polvel',xyz_Polvel)
    call HistoryPut('laplapolvel',xyz_LaplaPolvel)
    call HistoryPut('temp',xyz_Temp)
    call HistoryPut('u',xyz_U)
    call HistoryPut('v',xyz_V)
    call HistoryPut('w',xyz_W)
    call HistoryPut('ke',IntLonLatRad_xyz((xyz_U**2+xyz_V**2+xyz_W**2)/2))
    call HistoryPut('um',yz_avrLon_xyz(xyz_U))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_abcn

