!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! ���֥롼����Υƥ���
!    wt_VGradV
!  
program wttest14

  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=7.0D0/13.0D0      ! �⳰Ⱦ�� \eta=0.35
  integer,parameter  :: np=8                ! OPENMP ���祹��åɿ�
  real(8),parameter  :: ro=20.0D0/13.0D0     ! �⳰Ⱦ�� \eta=0.35

  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VTor    ! �ȥ�����ݥƥ󥷥��
  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VPol    ! �ݥ�����ݥƥ󥷥��

  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_Ke      ! ��ư���ͥ륮��

  real(8), dimension(im,jm,0:km)     :: xyz_VLon       ! ®��(����)
  real(8), dimension(im,jm,0:km)     :: xyz_VLat       ! ®��(����)
  real(8), dimension(im,jm,0:km)     :: xyz_Vrad       ! ®��(ư��)
  real(8), dimension(im,jm,0:km)     :: xyz_RotVLon  ! ������(����)
  real(8), dimension(im,jm,0:km)     :: xyz_RotVLat  ! ������(����)
  real(8), dimension(im,jm,0:km)     :: xyz_RotVrad  ! ������(ư��)
  real(8), dimension(im,jm,0:km)     :: xyz_VGradVLon  ! ������(����)
  real(8), dimension(im,jm,0:km)     :: xyz_VGradVLat  ! ������(����)
  real(8), dimension(im,jm,0:km)     :: xyz_VGradVrad  ! ������(ư��)
  real(8), dimension(im,jm,0:km)     :: xyz_VGradV0Lon  ! ������(����, ����)
  real(8), dimension(im,jm,0:km)     :: xyz_VGradV0Lat  ! ������(����, ����)
  real(8), dimension(im,jm,0:km)     :: xyz_VGradV0rad  ! ������(����,ư��)

  real(8), parameter :: eps = 1D-10
  real(8), parameter :: pi=3.1415926535897932385D0

  integer :: i,j,k

  call wt_Initial(im,jm,km,nm,lm,ri,ro,np)

  write( 6,* ) 'Test for VGradV'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- �� 1 --------------------
  ! ���β�ž��
  wt_VTor = wt_xyz(xyz_Rad * sin(xyz_Lat))
  wt_VPol = 0.0

  ! xyz_Vlon = xyz_Rad * cos(xyz_Lat)
  ! xyz_Vlat = 0.0
  ! xyz_Vrad = 0.0

  xyz_VGradV0lon = 0.0
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
  ! ���β�ž��(����ή)
  wt_VTor = wt_xyz(xyz_Rad * cos(xyz_Lat) * sin(xyz_Lon))
  wt_VPol = 0.0

  ! xyz_VLon = -xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)
  ! xyz_VLat = -xyz_Rad*cos(xyz_Lon)
  ! xyz_VRad = 0.0

  xyz_VGradV0Lon = xyz_Rad*cos(xyz_Lat)*sin(xyz_Lon)*cos(xyz_Lon)
  xyz_VGradV0Lat = -xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)*sin(xyz_Lon)**2
  xyz_VGradV0Rad = -xyz_Rad*(sin(xyz_Lat)**2*sin(xyz_Lon)**2 + cos(xyz_lon)**2)

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : rigid rotation'
  write(6,*)'    xyz_Vlon=xyz_Rad*xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)'
  write(6,*)'    xyz_Vlat=-xyz_Rad*cos(xyz_Lon)'
  write(6,*)'    xyz_VRad = 0.0'

  call checkresult

! ----------------- �� 3 --------------------
 ! �٥���ޡ��� case1 �ν������
  xyz_Vrad = 5.0D0/8.0D0 * ( 8*ro - 6*xyz_Rad - 2*ri**4/xyz_Rad**3 ) &
               * cos(pi/2-xyz_Lat)
  xyz_Vlat = - 5.0D0/8.0D0 * ( 9*xyz_Rad - 8*ro - ri**4/xyz_Rad**3 ) &
               * sin(pi/2-xyz_Lat)
  xyz_Vlon = 5 * sin(pi*(xyz_Rad-ri)) * sin(2*(pi/2-xyz_Lat))

  stop
contains

 !------- ������ -------
  subroutine checkresult

    call wt_Potential2Vector( &
           xyz_VLON,xyz_VLAT,xyz_VRAD, &
           wt_VTor, wt_VPol)

    call wt_Potential2Vector( &
           xyz_RotVLON,xyz_RotVLAT,xyz_RotVRAD, &
           -wt_Lapla_wt(wt_VPol), wt_VTor)
    
    wt_Ke = wt_xyz(1/2.0*(xyz_VLon**2 + xyz_VLat**2 + xyz_VRad**2))

    xyz_VGradVLon = xyz_GradLon_wt(wt_Ke) &
                     - (xyz_VLat*xyz_RotVRad- xyz_VRad*xyz_RotVLat)

    xyz_VGradVLat = xyz_GradLat_wt(wt_Ke) &
                     - (xyz_VRad*xyz_RotVLon- xyz_VLon*xyz_RotVRad)

    xyz_VGradVRad = xyz_wt(wt_DRad_wt(wt_Ke)) &
                     - (xyz_VLon*xyz_RotVLat- xyz_VLat*xyz_RotVLon)

    write(6,*)
    write(6,*)'Checking VGradV_Lon '
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_VGradVLon(i,j,k)-xyz_VGradV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_VGradVLon(i,j,k),xyz_VGradV0Lon(i,j,k)
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking VGradV_Lat '
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_VGradVLat(i,j,k)-xyz_VGradV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_VGradVLat(i,j,k),xyz_VGradV0Lat(i,j,k)
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking VGradV_Rad '
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_VGradVRad(i,j,k)-xyz_VGradV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_VGradVRad(i,j,k),xyz_VGradV0rad(i,j,k)
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wttest14


