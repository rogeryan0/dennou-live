!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ��ž��̥֥��ͥ���ή�Υ�ǥ� : ���β�ž�����
!
!����  2002/05/25  �ݹ�����
!      2002/11/27  �ݹ�����
!      2004/02/16  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2008/08/13  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!
program gami_rigidrot

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
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp       ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VTor      ! �ȥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VPol      ! �ݥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp   ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! ®��(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lon   ! v����v (����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lat   ! v����v(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Rad   ! v����v(ư��)

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! ������
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! ������

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=2e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=1000, ndisp=100        ! ������ʬ��, ɽ�����ƥå�

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
  call wt_VGradV1(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                 xyz_VLon,xyz_VLat,xyz_VRad )

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
                                   xyz_VGradV_Lon,xyz_VGradV_Lat)) &
                  )
          

     ! �ݥ�����®�پ�
     wt_LaplaVPol = wt_LaplaVPol + dt *(    &
               - Ra*Pr*wt_Temp                  &
               + Pr * wt_Lapla_wt(wt_LaplaVPol) &
               + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_LaplaVPol) &
                             + Tau * wt_QOperator_wt(wt_VTor) )  &
               + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
                     xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad )) &
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
     call wt_VGradV1(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
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
           file='gami_rigidrot.nc', title='convection in a spherical shell', &
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

 !------- VGradV �׻�(����1) -------
  subroutine wt_VGradV1(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                       xyz_VLON,xyz_VLAT,xyz_VRAD )

    real(8), dimension(0:im-1,jm,0:km),intent(out)   :: xyz_VGRADV_LON
    real(8), dimension(0:im-1,jm,0:km),intent(out)   :: xyz_VGRADV_LAT
    real(8), dimension(0:im-1,jm,0:km),intent(out)   :: xyz_VGRADV_RAD
    real(8), dimension(0:im-1,jm,0:km),intent(in)    :: xyz_VLON
    real(8), dimension(0:im-1,jm,0:km),intent(in)    :: xyz_VLAT
    real(8), dimension(0:im-1,jm,0:km),intent(in)    :: xyz_VRAD

    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_VLon
    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_VLat
    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_VRad

    wt_VLon=wt_xyz(xyz_Vlon)
    wt_VLat=wt_xyz(xyz_Vlat)
    wt_VRad=wt_xyz(xyz_VRad)

    xyz_VGRADV_LON = xyz_VLon*xyz_GradLon_wt(wt_VLon)     &
                   + xyz_VLat*xyz_GradLat_wt(wt_VLon)     &
                   + xyz_VRad*xyz_wt(wt_Drad_wt(wt_VLon)) &
                   + xyz_VLON*xyz_VRAD/xyz_RAD            &
                   - xyz_VLON*xyz_VLAT*tan(xyz_LAT)/xyz_Rad 

    xyz_VGRADV_LAT = xyz_VLon*xyz_GradLon_wt(wt_VLat)     &
                   + xyz_VLat*xyz_GradLat_wt(wt_VLat)     &
                   + xyz_VRad*xyz_wt(wt_Drad_wt(wt_VLat)) &
                   + xyz_VLAT*xyz_VRAD/xyz_RAD            &
                   + xyz_VLON**2*tan(xyz_LAT)/xyz_RAD 

    xyz_VGRADV_RAD = xyz_VLon*xyz_GradLon_wt(wt_VRad)     &
                   + xyz_VLat*xyz_GradLat_wt(wt_VRad)     &
                   + xyz_VRad*xyz_wt(wt_Drad_wt(wt_VLat)) &
                   - (xyz_VLON**2 + xyz_VLAT**2)/xyz_RAD 

  end subroutine wt_VGradV1

 !------- VGradV �׻�(����2) -------
  subroutine wt_VGradV2(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                       xyz_VLON,xyz_VLAT,xyz_VRAD )
   ! V����V =  ��(1/2V**2) - Vx��xV

    real(8), dimension(0:im-1,jm,0:km),intent(out) :: xyz_VGRADV_LON
    real(8), dimension(0:im-1,jm,0:km),intent(out) :: xyz_VGRADV_LAT
    real(8), dimension(0:im-1,jm,0:km),intent(out) :: xyz_VGRADV_RAD
    real(8), dimension(0:im-1,jm,0:km),intent(in)  :: xyz_VLON
    real(8), dimension(0:im-1,jm,0:km),intent(in)  :: xyz_VLAT
    real(8), dimension(0:im-1,jm,0:km),intent(in)  :: xyz_VRAD

    real(8), dimension(0:im-1,jm,0:km)      :: xyz_RotVlon
    real(8), dimension(0:im-1,jm,0:km)      :: xyz_RotVlat
    real(8), dimension(0:im-1,jm,0:km)      :: xyz_RotVrad
    real(8), dimension((nm+1)*(nm+1),0:lm)  :: wt_Ke

    wt_Ke = wt_xyz(1/2.0*(xyz_VLon**2 + xyz_VLat**2 + xyz_VRad**2))

    xyz_RotVLon=xyz_GradLat_wt(wt_xyz(xyz_VRad))-xyz_wt(wt_RotRad_wt(wt_xyz(xyz_VLat)))
    xyz_RotVLat=xyz_wt(wt_RotRad_wt(wt_xyz(xyz_VLon)-wt_DivLon_xyz(xyz_VRad)))

    xyz_RotVRad=xyz_wt(wt_DivLon_xyz(xyz_VLat)-wt_DivLat_xyz(xyz_VLon))

    xyz_VGRADV_LON = xyz_GradLon_wt(wt_Ke) &
                     - (xyz_VLat*xyz_RotVRad- xyz_VRad*xyz_RotVLat)

    xyz_VGRADV_LAT = xyz_GradLat_wt(wt_Ke) &
                     - (xyz_VRad*xyz_RotVLon- xyz_VLon*xyz_RotVRad)

    xyz_VGRADV_RAD = xyz_wt(wt_DRad_wt(wt_Ke)) &
                     - (xyz_VLon*xyz_RotVLat- xyz_VLat*xyz_RotVLon)

  end subroutine wt_VGradV2

 !------- VGradV �׻�(����3) -------
  subroutine wt_VGradV3(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                        xyz_VLON,xyz_VLAT,xyz_VRAD )

    real(8), dimension(0:im-1,jm,0:km),intent(out)   :: xyz_VGRADV_LON
    real(8), dimension(0:im-1,jm,0:km),intent(out)   :: xyz_VGRADV_LAT
    real(8), dimension(0:im-1,jm,0:km),intent(out)   :: xyz_VGRADV_RAD
    real(8), dimension(0:im-1,jm,0:km),intent(in)    :: xyz_VLON
    real(8), dimension(0:im-1,jm,0:km),intent(in)    :: xyz_VLAT
    real(8), dimension(0:im-1,jm,0:km),intent(in)    :: xyz_VRAD

    xyz_VGRADV_LON = &
            xyz_Div_xyz_xyz_xyz( &
                xyz_VLON * xyz_VLON, xyz_VLON*xyz_VLAT, xyz_VLON*xyz_VRAD ) &
          + xyz_VLON*xyz_VRAD/xyz_RAD              &
          - xyz_VLON*xyz_VLAT*tan(xyz_LAT)/xyz_RAD 

    xyz_VGRADV_LAT = &
            xyz_Div_xyz_xyz_xyz( &
                xyz_VLAT*xyz_VLON, xyz_VLAT*xyz_VLAT, xyz_VLAT*xyz_VRAD ) &
          + xyz_VLAT*xyz_VRAD/xyz_RAD        &
          + xyz_VLON**2*tan(xyz_LAT)/xyz_RAD 

    xyz_VGRADV_RAD = &
            xyz_Div_xyz_xyz_xyz( &
                xyz_VRAD*xyz_VLON, xyz_VRAD*xyz_VLAT, xyz_VRAD*xyz_VRAD ) &
          - (xyz_VLON**2 + xyz_VLAT**2)/xyz_RAD 


    end subroutine wt_VGradV3

end program gami_rigidrot
