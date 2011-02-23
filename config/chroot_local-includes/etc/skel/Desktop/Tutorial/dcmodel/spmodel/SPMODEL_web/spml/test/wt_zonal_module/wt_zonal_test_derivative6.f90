!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_zonal_module �ƥ��ȥץ����
!
!      ���֥롼����Υƥ���
!        wt_VGradV
!  
!����  2008/12/30  �ݹ�����
!
program wt_zonal_test_derivative6

  use dc_message, only : MessageNotify
  use wt_zonal_module
  implicit none

  integer,parameter  :: im=1, jm=16, km=8    ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8          ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=7.0D0/13.0D0      ! �⳰Ⱦ�� \eta=0.35
  real(8),parameter  :: ro=20.0D0/13.0D0     ! �⳰Ⱦ�� \eta=0.35

  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VLon       ! ®��(����)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VLat       ! ®��(����)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_Vrad       ! ®��(ư��)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VGradVLon  ! ������(����)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VGradVLat  ! ������(����)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VGradVrad  ! ������(ư��)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VGradV0Lon ! ������(����, ����)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VGradV0Lat ! ������(����, ����)
  real(8), dimension(0:im-1,1:jm,0:km)  :: xyz_VGradV0rad ! ������(����,ư��)

  real(8), parameter :: eps = 1D-10
  real(8), parameter :: pi=3.1415926535897932385D0

  integer :: i,j,k

  call MessageNotify('M','wt_test_derivative6', &
       'wt_module derivative subroutine test #6')

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  write( 6,* ) 'Test for VGradV'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- �� 1 --------------------
  ! ���β�ž��
  xyz_Vlon = xyz_Rad * cos(xyz_Lat)
  xyz_Vlat = 0.0D0
  xyz_Vrad = 0.0D0

  xyz_VGradV0lon = 0.0D0
  xyz_VGradV0lat = xyz_Rad*sin(xyz_LAT)*cos(xyz_LAT)
  xyz_VGradV0rad = -xyz_Rad*cos(xyz_LAT)**2

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xyz_Vlon = xyz_Rad * cos(xyz_Lat)'
  write(6,*)'    xyz_Vlat = 0.0'
  write(6,*)'    xyz_VRad = 0.0'

  call checkresult

! ----------------- �� 2 --------------------
!!$  ! ���β�ž��(����ή)
!!$  xyz_VLon = -xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)
!!$  xyz_VLat = -xyz_Rad*cos(xyz_Lon)
!!$  xyz_VRad = 0.0D0
!!$
!!$  xyz_VGradV0Lon = xyz_Rad*cos(xyz_Lat)*sin(xyz_Lon)*cos(xyz_Lon)
!!$  xyz_VGradV0Lat = -xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)*sin(xyz_Lon)**2
!!$  xyz_VGradV0Rad = -xyz_Rad*(sin(xyz_Lat)**2*sin(xyz_Lon)**2 + cos(xyz_lon)**2)
!!$
!!$  write(6,*)
!!$  write(6,*)
!!$  write(6,*)'Example 2 : rigid rotation'
!!$  write(6,*)'    xyz_Vlon=xyz_Rad*xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)'
!!$  write(6,*)'    xyz_Vlat=-xyz_Rad*cos(xyz_Lon)'
!!$  write(6,*)'    xyz_VRad = 0.0'
!!$
!!$  call checkresult

!!$! ----------------- �� 3 --------------------
!!$ ! �٥���ޡ��� case1 �ν������
!!$  xyz_Vrad = 5.0D0/8.0D0 * ( 8*ro - 6*xyz_Rad - 2*ri**4/xyz_Rad**3 ) &
!!$               * cos(pi/2-xyz_Lat)
!!$  xyz_Vlat = - 5.0D0/8.0D0 * ( 9*xyz_Rad - 8*ro - ri**4/xyz_Rad**3 ) &
!!$               * sin(pi/2-xyz_Lat)
!!$  xyz_Vlon = 5 * sin(pi*(xyz_Rad-ri)) * sin(2*(pi/2-xyz_Lat))

  call MessageNotify('M','wt_test_derivative6', &
       'wt_module derivative function test #6 succeeded!')
  stop
contains

 !------- ������ -------
  subroutine checkresult

!!$    call wt_VGradV3(xyz_VGradVLon,xyz_VGradVLat,xyz_VGradVRad, &
!!$                    xyz_Vlon,xyz_Vlat,xyz_Vrad )
    call wt_VGradV(xyz_VGradVLon,xyz_VGradVLat,xyz_VGradVRad, &
                   xyz_Vlon,xyz_Vlat,xyz_Vrad )

    write(6,*)
    write(6,*)'Checking VGradV_Lon '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_VGradVLon(i,j,k)-xyz_VGradV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_VGradVLon(i,j,k),xyz_VGradV0Lon(i,j,k)
                call MessageNotify('E','wt_test_derivative6', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking VGradV_Lat '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_VGradVLat(i,j,k)-xyz_VGradV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_VGradVLat(i,j,k),xyz_VGradV0Lat(i,j,k)
                call MessageNotify('E','wt_test_derivative6', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking VGradV_Rad '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_VGradVRad(i,j,k)-xyz_VGradV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_VGradVRad(i,j,k),xyz_VGradV0rad(i,j,k)
                call MessageNotify('E','wt_test_derivative6', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do


  end subroutine checkresult

 !------- VGradV �׻�(����0) -------
  subroutine wt_VGradV0(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                       xyz_VLON,xyz_VLAT,xyz_VRAD )

    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LON
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_RAD
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLON
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VRAD

    xyz_VGRADV_LON = xyz_wt(    &
            wt_Div_xyz_xyz_xyz( &
                xyz_VLON * xyz_VLON, xyz_VLON*xyz_VLAT, xyz_VLON*xyz_VRAD )) &
          + xyz_VLON*xyz_VRAD/xyz_RAD              &
          - xyz_VLON*xyz_VLAT*tan(xyz_LAT)/xyz_RAD 

    xyz_VGRADV_LAT = xyz_wt(    &
            wt_Div_xyz_xyz_xyz( &
                xyz_VLAT*xyz_VLON, xyz_VLAT*xyz_VLAT, xyz_VLAT*xyz_VRAD )) &
          + xyz_VLAT*xyz_VRAD/xyz_RAD        &
          + xyz_VLON**2*tan(xyz_LAT)/xyz_RAD 

    xyz_VGRADV_RAD = xyz_wt(    &
            wt_Div_xyz_xyz_xyz( &
                xyz_VRAD*xyz_VLON, xyz_VRAD*xyz_VLAT, xyz_VRAD*xyz_VRAD )) &
          - (xyz_VLON**2 + xyz_VLAT**2)/xyz_RAD 

  end subroutine wt_VGradV0

 !------- VGradV �׻�(����1) -------
  subroutine wt_VGradV1(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                       xyz_VLON,xyz_VLAT,xyz_VRAD )

    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LON
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_RAD
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLON
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VRAD

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

    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LON
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_RAD
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLON
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VRAD

    real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_RotVlon
    real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_RotVlat
    real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_RotVrad
    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_Ke

    wt_Ke = wt_xyz(1/2.0*(xyz_VLon**2 + xyz_VLat**2 + xyz_VRad**2))

    xyz_RotVLon=xyz_GradLat_wt(wt_xyz(xyz_VRad))-xyz_wt(wt_RotRad_wt(wt_xyz(xyz_VLat)))
    xyz_RotVLat=xyz_wt(wt_RotRad_wt(wt_xyz(xyz_VLon))-wt_DivLon_xyz(xyz_VRad))

    xyz_RotVRad=xyz_wt(wt_DivLon_xyz(xyz_VLat)-wt_DivLat_xyz(xyz_VLon))

    xyz_VGRADV_LON = xyz_GradLon_wt(wt_Ke) &
                     - (xyz_VLat*xyz_RotVRad- xyz_VRad*xyz_RotVLat)

    xyz_VGRADV_LAT = xyz_GradLat_wt(wt_Ke) &
                     - (xyz_VRad*xyz_RotVLon- xyz_VLon*xyz_RotVRad)

    xyz_VGRADV_RAD = xyz_wt(wt_DRad_wt(wt_Ke)) &
                     - (xyz_VLon*xyz_RotVLat- xyz_VLat*xyz_RotVLon)

  end subroutine wt_VGradV2

  subroutine wt_VGradV3(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                        xyz_VLON,xyz_VLAT,xyz_VRAD )

    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LON
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_RAD
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLON
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLAT
    real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VRAD

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

end program wt_zonal_test_derivative6

