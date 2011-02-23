!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  GAMIRAS ��ǥ� : Euler scheme
!      �ݥ�����®�پ�ϱ�ľ�ʻ������֤Ǥ�����ˡ�ˤ�붭�����Ŭ��
!      ����¾�� Chebyshev-Tau method
!
!����  2002/08/06  �ݹ�����  �٥���ޡ��� Case 0
!      2002/11/19  �ݹ�����  �ץ����̾�ѹ�
!      2002/11/26  �ݹ�����  ����򸺤餷�ѿ�̾�ѹ�
!      2004/02/16  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2008/08/13  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!
program gami_euler_tau_bench

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
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ��^2�ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp       ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp      ! ����
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! ®��(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lon   ! v����v (����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lat   ! v����v(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Rad   ! v����v(ư��)

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! ������
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! ������

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-4                    ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=20        ! ������ʬ��, ɽ�����ƥå�

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
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_x   ! x=2r-r_i-r_o

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_Initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- ��������� ----------------------
  xyz_x = 2*xyz_Rad - Ri - Ro

  xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_x**2 + 3*xyz_x**4 -xyz_x**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

  wt_Temp = wt_xyz(xyz_Temp)

  wt_VTor = 0     ! �Ż߾���
  wt_VPol = 0
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
  call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                 xyz_VLon,xyz_VLat,xyz_VRad )

 !------------------- ���������� ----------------------
  xy_TempBndry(:,:,1) = Temptop
  xy_TempBndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- ������ʬ(ñ�� Euler ˡ) --------------------
  call output_gtool4_init
  call output_gtool4

  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ

     ! �ȥ�����®�پ�
     wt_VTor = wt_VTor + dt *( &
                    wt_Lapla_wt(wt_VTor) &
                  + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_VTor)   &
                                - 2/Ekman * wt_Qoperator_wt(wt_VPol) ) &
                  - wt_L2Inv_wt(wt_RadRot_xyz_xyz(      &
                                   xyz_VGradV_Lon,xyz_VGradV_Lat)) &
                  )
          

     ! �ݥ�����®�پ�
     wt_LaplaVPol = wt_LaplaVPol + dt *(    &
               - Ra/(Ekman*Ro)*wt_Temp          &
               + wt_Lapla_wt(wt_LaplaVPol) &
               + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_LaplaVPol) &
                             + 2/Ekman * wt_Qoperator_wt(wt_VTor) )  &
               + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
                     xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad )) &
             )

     ! ���پ�
     wt_Temp = wt_Temp + dt *(       &
              - wt_Div_xyz_xyz_xyz(  &
                    xyz_VLon*xyz_Temp,xyz_VLat*xyz_Temp,xyz_VRad*xyz_Temp) &
              + 1/Pr * wt_Lapla_wt(wt_Temp) &
            )

     ! �������
     call wt_TorBoundariesTau(wt_VTor,cond=VelBC)
     wt_VPol = wt_wz(wz_Laplapol2pol_wz(wz_wt(wt_LaplaVPol),cond=VelBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)
     call wt_BoundariesTau(wt_Temp,w_TempBndry,cond=TempBC)

     ! ����å��ͷ׻�
     xyz_Temp = xyz_wt(wt_Temp)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
     call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                    xyz_VLon,xyz_VLat,xyz_VRad )

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- ���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='gami_euler_tau_bench0.nc', &
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
           longname='mean kinetic energy', units='1', xtype='double')
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
    call HistoryPut('ek',avrlonlatrad_xyz((xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_euler_tau_bench

