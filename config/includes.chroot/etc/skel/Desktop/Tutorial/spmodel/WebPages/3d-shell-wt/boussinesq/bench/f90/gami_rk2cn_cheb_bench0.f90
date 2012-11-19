!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ��ž��̥֥��ͥ���ή�Υ�ǥ�
!      2nd Order Rung-Kutta + Crank Nicholson scheme
!
!����  2002/11/03  �ݹ�����  �٥���ޡ��� Case 0
!      2002/11/20  �ݹ�����  �������׻��ѹ�
!      2002/11/26  �ݹ�����  ����򸺤餷�ѿ�̾�ѹ�
!      2004/02/16  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2008/08/13  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!
program gami_rk2cn_cheb_bench0

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)
  !integer,parameter  :: im=64, jm=32, km=16  ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=21, lm=16         ! �����ȿ�������(��ʿ, ư��)
  !integer,parameter  :: im=128, jm=64, km=32 ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=42, lm=32         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: Ri=7.0D0/13.0D0     ! �⳰Ⱦ�� \eta=0.35
  real(8),parameter  :: Ro=20.0D0/13.0D0    ! �⳰Ⱦ�� \eta=0.35

 !---- �ѿ� ----
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor       ! �ȥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor1      ! �ȥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol1      ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ��^2�ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol1 ! ��^2�ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp       ! ����
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp1      ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp      ! ����
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! ®��(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Q      ! Ǯ��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Q       ! Ǯ��

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! ������
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! ������

 !---- ������ʬ���ѿ� ----

 ! �ȥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDVTor
 ! ��^2�ݥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDLaplaVPol
 ! ���ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTemp

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
  character(len=2), parameter :: VelBC='RR'     ! ®�ٶ������(RR/RF/FR/FF)
  character(len=2), parameter :: TempBC='DD'    ! ���ٶ������(DD/DN/ND/NN)

  real(8), parameter :: Ra=100.0D0              ! �쥤�꡼��
  real(8), parameter :: Pr=1.0D0                ! �ץ��ɥ��
  real(8), parameter :: Ekman=1.0d-3            ! �����ޥ��

  real(8), parameter :: A=0.1D0                 ! ���ʬ�ۿ���
  real(8), parameter :: Temptop=0.0D0           ! ��������(��ü)
  real(8), parameter :: Tempbottom=1.0D0        ! ��������(��ü)

 !---- ����¾ ----
  real(8), parameter :: pi=3.1415926535897932385D0
  integer :: it
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_X   ! x=2r-r_i-r_o

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_Initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- ��������� ----------------------
  xyz_X = 2*xyz_Rad - Ri - Ro

  xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_X**2 + 3*xyz_X**4 -xyz_X**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

  wt_Temp = wt_xyz(xyz_Temp)

  wt_VTor = 0     ! �Ż߾���
  wt_VPol = 0
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)

  call CNDiffusionMatrix( 1.0D0, dt, DifLUMT_Vel, kpivot_Vel )
  call CNDiffusionMatrix( 1/Pr, dt, DifLUMT_Temp, kpivot_Temp )

  xyz_Q = 0.0 ; wt_Q = wt_xyz(xyz_Q)    ! Ǯ���ʤ�

 !------------------- ���������� ----------------------
  xy_Tempbndry(:,:,1) = Temptop
  xy_Tempbndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- ������ʬ(Adams-Bashforth ˡ) --------------------
  call output_gtool4_init
  call output_gtool4

  do it=1,nt
    !---- Runge-Kutta 1 ����
     call TimeDerivetives_noDiffusion( &
         wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
         xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
         wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

     ! �ȥ�����®�پ�
     wt_VTor1 = wt_VTor + 1/2.0 * dt * wt_DtDVTor
     call wt_TorBoundariesGrid(wt_VTor1,cond=velBC)

     ! �ݥ�����®�پ�
     wt_LaplaVPol1 = wt_LaplaVPol + 1/2.0 * dt * wt_DtDLaplaVPol
     wt_VPol1 = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol1),cond=velBC))
     wt_LaplaVPol1 = wt_Lapla_wt(wt_VPol1)

     ! ���پ�
     wt_Temp1 = wt_Temp + 1/2.0 * dt * wt_DtDTemp
     call wt_BoundariesGrid(wt_Temp1,w_TempBndry,cond=TempBC)

     ! ����å��ͷ׻�
     xyz_Temp = xyz_wt(wt_Temp1)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor1,wt_VPol1)

    !---- Runge-Kutta 2 ����
     call TimeDerivetives_noDiffusion( &
         wt_VTor1, wt_VPol1, wt_LaplaVPol1, wt_Temp1, wt_Q, &
         xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
         wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

     ! �ȥ�����®�پ�
     wt_VTor = wt_VTor + dt * wt_DtDVTor
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)

     ! �ݥ�����®�پ�
     wt_LaplaVPol = wt_LaplaVPol + dt * wt_DtDLaplaVPol
     wt_VPol = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol),cond=velBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     ! ���پ�
     wt_Temp = wt_Temp + dt * wt_DtDTemp
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

    !---- Crank-Nicolson 
     wt_VTor = wt_VTor+dt/2.0 * wt_Lapla_wt(wt_VTor)
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)
     wt_VTor = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_VTor)
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)

     wt_LaplaVPol = wt_LaplaVPol + dt/2.0 * wt_Lapla_wt(wt_LaplaVPol)
     wt_VPol = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol),cond=velBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     wt_LaplaVPol = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_LaplaVPol)
     wt_VPol = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol),cond=velBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     wt_Temp = wt_Temp+ dt/2.0/Pr * wt_Lapla_wt(wt_Temp)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wt_Temp)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

     ! ����å��ͷ׻�
     xyz_Temp = xyz_wt(wt_Temp)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- �����Ѳ��� ----------------------
  subroutine TimeDerivetives_noDiffusion( &
       wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
       xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
       wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

   ! �ȥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_VTor
   ! �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_VPol
   ! ��^2 �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_LaplaVPol
   ! ����
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Temp
   ! Ǯ��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Q

    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VLon   ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VLat   ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VRad   ! ®��(ư��)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Temp   ! ����

   ! �ȥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDVTor
   ! �ݥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDLaplaVPol
   ! ���ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDTemp

    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lon   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lat   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Rad   ! v����v(ư��)


   ! v����v ��������׻�
    call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                   xyz_VLon,xyz_VLat,xyz_VRad )

  ! �ȥ���������Ѳ�
    wt_DtDVTor = &
           wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_VTor)   &
                       - 2/Ekman * wt_Qoperator_wt(wt_VPol) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_VGradV_Lon,xyz_VGradV_Lat)) 

  ! �ݥ���������Ѳ�
    wt_DtDLaplaVPol = &
         - Ra/(Ekman*RO)*wt_Temp                    &
         + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_LaplaVPol) &
                       + 2/Ekman * wt_Qoperator_wt(wt_VTor) )  &
         + wt_L2inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
               xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad ))

  ! ���ٻ����Ѳ�
    wt_DtDTemp = &
         - wt_Div_xyz_xyz_xyz(  &
               xyz_VLon*xyz_Temp,xyz_VLat*xyz_Temp,xyz_VRad*xyz_Temp)

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

 !------------------- ���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='gami_rk2cn_cheb_bench0.nc', &
           title='Benchmark of dynamo model (case0)', &
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
           varname='vtor', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vpol', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='laplapolvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='laplacian poloidal velocity potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='ek', dims=(/'t  '/), & 
           longname='kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('vtor',xyz_wt(wt_VTor))
    call HistoryPut('vpol',xyz_wt(wt_VPol))
    call HistoryPut('temp',xyz_Temp)
    call HistoryPut('vlon',xyz_VLon)
    call HistoryPut('vlat',xyz_VLat)
    call HistoryPut('vrad',xyz_VRad)
    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2))
    call HistoryPut('um',yz_avrLon_xyz(xyz_VLon))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_rk2cn_cheb_bench0



