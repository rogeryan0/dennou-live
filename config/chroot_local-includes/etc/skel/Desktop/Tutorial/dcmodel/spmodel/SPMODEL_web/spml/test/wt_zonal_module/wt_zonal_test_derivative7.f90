!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module テストプログラム
!
!     サブルーチンのテスト
!       wt_Potential2Rotation
!
!履歴  2008/12/30  竹広真一
!
program wt_zonal_test_derivative7

  use dc_message, only : MessageNotify
  use wt_zonal_module
  implicit none

  integer,parameter  :: im=1, jm=16, km=8    ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8          ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=7.0D0/13.0D0      ! 内外半径 \eta=0.35
  real(8),parameter  :: ro=20.0D0/13.0D0     ! 内外半径 \eta=0.35

  real(8), dimension(nm+1,0:lm)        :: wt_VTor    ! トロイダルポテンシャル
  real(8), dimension(nm+1,0:lm)        :: wt_VPol    ! ポロイダルポテンシャル

  real(8), dimension(0:im-1,1:jm,0:km) :: xyz_RotVLon  ! 慣性項(経度)
  real(8), dimension(0:im-1,1:jm,0:km) :: xyz_RotVLat  ! 慣性項(緯度)
  real(8), dimension(0:im-1,1:jm,0:km) :: xyz_RotVrad  ! 慣性項(動径)
  real(8), dimension(0:im-1,1:jm,0:km) :: xyz_RotV0Lon  ! 慣性項(正解, 経度)
  real(8), dimension(0:im-1,1:jm,0:km) :: xyz_RotV0Lat  ! 慣性項(正解, 緯度)
  real(8), dimension(0:im-1,1:jm,0:km) :: xyz_RotV0rad  ! 慣性項(正解,動径)

  real(8), parameter :: eps = 1D-10

  integer :: i,j,k

  call MessageNotify('M','wt_zonal_test_derivative7', &
       'wt_zonal_module derivative subroutine test #7')

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  write( 6,* ) 'Test for wt_Rotation'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  ! 剛体回転場
  wt_VTor = wt_xyz(xyz_Rad * sin(xyz_Lat))
  wt_VPol = 0.0D0

  ! xyz_Vlon = xyz_Rad * cos(xyz_Lat)'
  ! xyz_Vlat = 0.0'
  ! xyz_VRad = 0.0'

  xyz_RotV0lon = 0.0D0
  xyz_RotV0lat = 2*cos(xyz_LAT)
  xyz_RotV0rad = 2*sin(xyz_LAT)

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xyz_Vlon = xyz_Rad * cos(xyz_Lat)'
  write(6,*)'    xyz_Vlat = 0.0D0'
  write(6,*)'    xyz_VRad = 0.0D0'

  call checkresult

! ----------------- 例 2 --------------------
 ! 渦無し場

  wt_VTor = 0.0D0
  wt_VPol = wt_xyz(xyz_Rad * sin(xyz_Lat))

  ! xyz_Vlon = 0
  ! xyz_Vlat = 2 * xyz_Rad * cos(xyz_Lat)
  ! xyz_Vrad = 2 * xyz_Rad * sin(xyz_Lat)

  xyz_RotV0lon = 0.0D0
  xyz_RotV0lat = 0.0D0
  xyz_RotV0rad = 0.0D0

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : no rotation'
  write(6,*)'    xyz_Vlon = 0.0D0'
  write(6,*)'    xyz_Vlat = 0.0D0'
  write(6,*)'    xyz_VRad = xyz_Rad*2 * cos(xyz_Lat)'

  call checkresult

  call MessageNotify('M','wt_zonal_test_derivative7', &
       'wt_zonal_module derivative function test #7 succeeded!')

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
          do i=0,im-1
             if ( abs(xyz_RotVLon(i,j,k)-xyz_RotV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVLon(i,j,k),xyz_RotV0Lon(i,j,k)
                call MessageNotify('E','wt_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Lat '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_RotVLat(i,j,k)-xyz_RotV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVLat(i,j,k),xyz_RotV0Lat(i,j,k)
                call MessageNotify('E','wt_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Rad '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_RotVRad(i,j,k)-xyz_RotV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVRad(i,j,k),xyz_RotV0rad(i,j,k)
                call MessageNotify('E','wt_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wt_zonal_test_derivative7
