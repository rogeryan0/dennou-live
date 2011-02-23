!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ��ž��̥֥��ͥ���ή�Υ�ǥ� : ���β�ž�����
!
!����  2002/05/25  �ݹ�����
!      2002/11/27  �ݹ�����
!      2002/11/28  �ݹ����� ���������ѹ�
!      2004/02/16  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2008/08/13  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!
program gami_vrotv_rigidrot

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: Ri=0.5, Ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor       ! �ȥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ��^2�ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp    ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VTor      ! �ȥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VPol      ! �ݥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp   ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! ®��(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZLon      ! ����(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZLat      ! ����(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZRad      ! ����(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZxVLon   ! (��xv)xv (����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZxVLat   ! (��xv)xv(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZxVRad   ! (��xv)xv(ư��)

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! ������
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! ������

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=2e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=1000, ndisp=100      ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: VelBC='FF'     ! ®�ٶ������(RR/RF/FR/FF)
  character(len=2), parameter :: TempBC='DD'    ! ���ٶ������(DD/DN/ND/NN)

  real(8), parameter :: Ra=0                    ! �쥤�꡼��
  real(8), parameter :: Pr=1.0                  ! �ץ��ɥ��
  !real(8), parameter :: Ta=1.0d2                ! �ƥ��顼��
  real(8), parameter :: Ta=0.0                  ! �ƥ��顼��

  real(8), parameter :: pi=3.1415926535897932385D0

 !---- ����¾ ----
  integer :: it
  real(8) :: Tau                                ! ��ƥ��顼��

  Tau = sqrt(Ta)

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- ��������� ----------------------
  xyz_Temp =  0.0
  wt_Temp = wt_xyz(xyz_Temp)

  ! ����ή���β�ž(�Ϥβ�ž�� 0)
  !xyz_VTor =  xyz_Rad*sin(xyz_Lat) ; xyz_VPol =  0 

  ! ����ή���β�ž(�Ϥβ�ž 0)
  xyz_VTor =  xyz_Rad*cos(xyz_Lat)*sin(xyz_Lon) ; xyz_VPol =  0 

  wt_VTor = wt_xyz(xyz_VTor) ; wt_VPol = wt_xyz(xyz_VPol)
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
  call wt_Potential2Rotation(xyz_ZLon,xyz_ZLat,xyz_ZRad,wt_VTor,wt_VPol)

  xyz_ZxVLon = xyz_VRad * xyz_ZLat - xyz_VLat * xyz_ZRad
  xyz_ZxVLat = xyz_VLon * xyz_ZRad - xyz_VRad * xyz_ZLon
  xyz_ZxVRad = xyz_VLat * xyz_ZLon - xyz_VLon * xyz_ZLat

 !------------------- ���������� ----------------------
  xy_TempBndry(:,:,1) = 0
  xy_TempBndry(:,:,2) = 0

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- ������ʬ(ñ�� Euler ˡ) --------------------
  call output_gtool4_init
  call output_gtool4

  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ

     ! �ȥ�����®�پ�
     wt_VTor = wt_VTor + dt *( &
                    Pr * wt_Lapla_wt(wt_VTor) &
                  + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_VTor)   &
                                - Tau * wt_QOperator_wt(wt_VPol) ) &
                  - wt_L2Inv_wt(wt_RadRot_xyz_xyz(      &
                                   xyz_ZxVLon,xyz_ZxVLat)) &
                  )
          

     ! �ݥ�����®�پ�
     wt_LaplaVPol = wt_LaplaVPol + dt *(    &
               - Ra*Pr*wt_Temp                  &
               + Pr * wt_Lapla_wt(wt_LaplaVPol) &
               + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_LaplaVPol) &
                             + Tau * wt_QOperator_wt(wt_VTor) )  &
               + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
                     xyz_ZxVLon,xyz_ZxVLat,xyz_ZxVRad )) &
             )

     ! ���پ�
     wt_Temp = wt_Temp + dt *(       &
              - wt_Div_xyz_xyz_xyz(  &
                    xyz_VLon*xyz_Temp,xyz_VLat*xyz_Temp,xyz_VRad*xyz_Temp) &
              + wt_L2_wt(wt_VPol)    &
              + wt_Lapla_wt(wt_Temp) &
            )

     ! �������
     call wt_TorBoundariesGrid(wt_VTor,cond=VelBC)
     wt_VPol = wt_LaplaPol2PolGrid_wt(wt_LaplaVPol,cond=VelBC)
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

     ! ����å��ͷ׻�
     xyz_Temp = xyz_wt(wt_Temp)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
     call wt_Potential2Rotation(xyz_ZLon,xyz_ZLat,xyz_ZRad,wt_VTor,wt_VPol)



     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- ���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='gami_vrotv_rigidrot.nc', title='convection in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
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
           longname='kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='am', dims=(/'t  '/), & 
           longname='relative angular momentum', units='1', xtype='double')
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
    call HistoryPut('am',IntLonLatRad_xyz(xyz_VLon*xyz_Rad*cos(xyz_Lat)))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_vrotv_rigidrot
