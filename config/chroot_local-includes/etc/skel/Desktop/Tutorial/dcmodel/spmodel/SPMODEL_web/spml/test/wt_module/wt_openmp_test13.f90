!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! サブルーチンのテスト
!    wt_Potential2Rotation
!
program wttest13

  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=7.0D0/13.0D0      ! 内外半径 \eta=0.35
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数
  real(8),parameter  :: ro=20.0D0/13.0D0     ! 内外半径 \eta=0.35

  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VTor    ! トロイダルポテンシャル
  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VPol    ! ポロイダルポテンシャル

  real(8), dimension(im,jm,0:km)     :: xyz_RotVLon  ! 慣性項(経度)
  real(8), dimension(im,jm,0:km)     :: xyz_RotVLat  ! 慣性項(緯度)
  real(8), dimension(im,jm,0:km)     :: xyz_RotVrad  ! 慣性項(動径)
  real(8), dimension(im,jm,0:km)     :: xyz_RotV0Lon  ! 慣性項(正解, 経度)
  real(8), dimension(im,jm,0:km)     :: xyz_RotV0Lat  ! 慣性項(正解, 緯度)
  real(8), dimension(im,jm,0:km)     :: xyz_RotV0rad  ! 慣性項(正解,動径)

  real(8), parameter :: eps = 1D-10
  real(8), parameter :: pi=3.1415926535897932385D0

  integer :: i,j,k

  call wt_Initial(im,jm,km,nm,lm,ri,ro,np)

  write( 6,* ) 'Test for wt_Rotation'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  ! 剛体回転場
  wt_VTor = wt_xyz(xyz_Rad * sin(xyz_Lat))
  wt_VPol = 0.0

  ! xyz_Vlon = xyz_Rad * cos(xyz_Lat)'
  ! xyz_Vlat = 0.0'
  ! xyz_VRad = 0.0'

  xyz_RotV0lon = 0.0
  xyz_RotV0lat = 2*cos(xyz_LAT)
  xyz_RotV0rad = 2*sin(xyz_LAT)

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xyz_Vlon = xyz_Rad * cos(xyz_Lat)'
  write(6,*)'    xyz_Vlat = 0.0'
  write(6,*)'    xyz_VRad = 0.0'

  call checkresult

! ----------------- 例 2 --------------------
  ! 剛体回転場(南北流)
  wt_VTor = wt_xyz(xyz_Rad * cos(xyz_Lat) * sin(xyz_Lon))
  wt_VPol = 0.0

  !  xyz_VLon = -xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)
  !  xyz_VLat = -xyz_Rad*cos(xyz_Lon)
  !  xyz_VRad = 0.0

  xyz_RotV0Lon = 2*cos(xyz_Lon)
  xyz_RotV0Lat = -2*sin(xyz_Lon)*sin(xyz_Lat)
  xyz_RotV0Rad = 2*sin(xyz_Lon)*cos(xyz_Lat)

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : rigid rotation'
  write(6,*)'    xyz_Vlon=xyz_Rad*xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)'
  write(6,*)'    xyz_Vlat=-xyz_Rad*cos(xyz_Lon)'
  write(6,*)'    xyz_VRad = 0.0'

  call checkresult

! ----------------- 例 3 --------------------
 ! 渦無し場

  wt_VTor = 0.0
  wt_VPol = wt_xyz(xyz_Rad * sin(xyz_Lat))

  ! xyz_Vlon = 0
  ! xyz_Vlat = 2 * xyz_Rad * cos(xyz_Lat)
  ! xyz_Vrad = 2 * xyz_Rad * sin(xyz_Lat)

  xyz_RotV0lon = 0.0
  xyz_RotV0lat = 0.0
  xyz_RotV0rad = 0.0

  write(6,*)
  write(6,*)
  write(6,*)'Example 3 : no rotation'
  write(6,*)'    xyz_Vlon = 0.0'
  write(6,*)'    xyz_Vlat = 0.0'
  write(6,*)'    xyz_VRad = xyz_Rad*2 * cos(xyz_Lat)'

  call checkresult

! ----------------- 例 4 --------------------
 ! ポロイダル速度場

  wt_VTor = 0.0
  wt_VPol = wt_xyz(xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lon))

  ! xyz_Vlon = 3 * xyz_Rad * cos(xyz_Lon)
  ! xyz_Vlat = 3 * xyz_Rad * sin(xyz_Lat) * sin(xyz_Lon)
  ! xyz_Vrad = 2 * xyz_Rad * sin(xyz_Lat) * cos(xyz_Lon)

  xyz_RotV0lon = 4*sin(xyz_Lat)*sin(xyz_Lon)
  xyz_RotV0lat = 4*cos(xyz_Lon)
  xyz_RotV0rad = 0.0

  write(6,*)
  write(6,*)
  write(6,*)'Example 4 : poloidal field'
  write(6,*)'    xyz_Vlon = 3 * xyz_Rad * cos(xyz_Lon)'
  write(6,*)'    xyz_Vlat = 3 * xyz_Rad * sin(xyz_Lat) * sin(xyz_Lon)'
  write(6,*)'    xyz_VRad = 2 * xyz_Rad * sin(xyz_Lat) * cos(xyz_Lon)'

  call checkresult

  stop
contains

 !------- 結果比較 -------
  subroutine checkresult

    call wt_Potential2Rotation(&
         xyz_RotVLon,xyz_RotVLat,xyz_RotVRad, wt_VTor, wt_VPol )

    write(6,*)
    write(6,*)'Checking RotV_Lon '
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_RotVLon(i,j,k)-xyz_RotV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVLon(i,j,k),xyz_RotV0Lon(i,j,k)
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Lat '
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_RotVLat(i,j,k)-xyz_RotV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVLat(i,j,k),xyz_RotV0Lat(i,j,k)
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Rad '
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_RotVRad(i,j,k)-xyz_RotV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVRad(i,j,k),xyz_RotV0rad(i,j,k)
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wttest13
