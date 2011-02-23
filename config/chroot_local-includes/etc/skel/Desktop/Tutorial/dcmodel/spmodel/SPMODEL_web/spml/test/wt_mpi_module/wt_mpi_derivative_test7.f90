!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_mpi_module テストプログラム
!
!     サブルーチンのテスト
!       wt_Potential2Rotation
!
!履歴  2008/05/30  竹広真一
!
program wt_mpi_derivative_test7

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=7.0D0/13.0D0      ! 内外半径 \eta=0.35
  real(8),parameter  :: ro=20.0D0/13.0D0     ! 内外半径 \eta=0.35

  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VTor    ! トロイダルポテンシャル
  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VPol    ! ポロイダルポテンシャル

  real(8), allocatable  :: xvz_RotVLon(:,:,:)   ! 慣性項(経度)
  real(8), allocatable  :: xvz_RotVLat(:,:,:)   ! 慣性項(緯度)
  real(8), allocatable  :: xvz_RotVrad(:,:,:)   ! 慣性項(動径)
  real(8), allocatable  :: xvz_RotV0Lon(:,:,:)  ! 慣性項(正解, 経度)
  real(8), allocatable  :: xvz_RotV0Lat(:,:,:)  ! 慣性項(正解, 緯度)
  real(8), allocatable  :: xvz_RotV0rad(:,:,:)  ! 慣性項(正解,動径)

  real(8), parameter :: eps = 1D-10

  integer :: i,j,k

  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test7', &
       'wt_mpi_module derivative subroutine test #7')

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_RotVLon(0:im-1,jc,0:km))   ! 慣性項(経度)
  allocate(xvz_RotVLat(0:im-1,jc,0:km))   ! 慣性項(緯度)
  allocate(xvz_RotVrad(0:im-1,jc,0:km))   ! 慣性項(動径)
  allocate(xvz_RotV0Lon(0:im-1,jc,0:km))  ! 慣性項(正解, 経度)
  allocate(xvz_RotV0Lat(0:im-1,jc,0:km))  ! 慣性項(正解, 緯度)
  allocate(xvz_RotV0rad(0:im-1,jc,0:km))  ! 慣性項(正解,動径)

  write( 6,* ) 'Test for wt_Rotation'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  ! 剛体回転場
  wt_VTor = wt_xvz(xvz_Rad * sin(xvz_Lat))
  wt_VPol = 0.0

  ! xvz_Vlon = xvz_Rad * cos(xvz_Lat)'
  ! xvz_Vlat = 0.0'
  ! xvz_VRad = 0.0'

  xvz_RotV0lon = 0.0
  xvz_RotV0lat = 2*cos(xvz_LAT)
  xvz_RotV0rad = 2*sin(xvz_LAT)

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xvz_Vlon = xvz_Rad * cos(xvz_Lat)'
  write(6,*)'    xvz_Vlat = 0.0'
  write(6,*)'    xvz_VRad = 0.0'

  call checkresult

! ----------------- 例 2 --------------------
  ! 剛体回転場(南北流)
  wt_VTor = wt_xvz(xvz_Rad * cos(xvz_Lat) * sin(xvz_Lon))
  wt_VPol = 0.0

  !  xvz_VLon = -xvz_Rad*sin(xvz_Lat)*sin(xvz_Lon)
  !  xvz_VLat = -xvz_Rad*cos(xvz_Lon)
  !  xvz_VRad = 0.0

  xvz_RotV0Lon = 2*cos(xvz_Lon)
  xvz_RotV0Lat = -2*sin(xvz_Lon)*sin(xvz_Lat)
  xvz_RotV0Rad = 2*sin(xvz_Lon)*cos(xvz_Lat)

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : rigid rotation'
  write(6,*)'    xvz_Vlon=xvz_Rad*xvz_Rad*sin(xvz_Lat)*sin(xvz_Lon)'
  write(6,*)'    xvz_Vlat=-xvz_Rad*cos(xvz_Lon)'
  write(6,*)'    xvz_VRad = 0.0'

  call checkresult

! ----------------- 例 3 --------------------
 ! 渦無し場

  wt_VTor = 0.0
  wt_VPol = wt_xvz(xvz_Rad * sin(xvz_Lat))

  ! xvz_Vlon = 0
  ! xvz_Vlat = 2 * xvz_Rad * cos(xvz_Lat)
  ! xvz_Vrad = 2 * xvz_Rad * sin(xvz_Lat)

  xvz_RotV0lon = 0.0
  xvz_RotV0lat = 0.0
  xvz_RotV0rad = 0.0

  write(6,*)
  write(6,*)
  write(6,*)'Example 3 : no rotation'
  write(6,*)'    xvz_Vlon = 0.0'
  write(6,*)'    xvz_Vlat = 0.0'
  write(6,*)'    xvz_VRad = xvz_Rad*2 * cos(xvz_Lat)'

  call checkresult

! ----------------- 例 4 --------------------
 ! ポロイダル速度場

  wt_VTor = 0.0
  wt_VPol = wt_xvz(xvz_Rad**2 * cos(xvz_Lat)*sin(xvz_Lon))

  ! xvz_Vlon = 3 * xvz_Rad * cos(xvz_Lon)
  ! xvz_Vlat = 3 * xvz_Rad * sin(xvz_Lat) * sin(xvz_Lon)
  ! xvz_Vrad = 2 * xvz_Rad * sin(xvz_Lat) * cos(xvz_Lon)

  xvz_RotV0lon = 4*sin(xvz_Lat)*sin(xvz_Lon)
  xvz_RotV0lat = 4*cos(xvz_Lon)
  xvz_RotV0rad = 0.0

  write(6,*)
  write(6,*)
  write(6,*)'Example 4 : poloidal field'
  write(6,*)'    xvz_Vlon = 3 * xvz_Rad * cos(xvz_Lon)'
  write(6,*)'    xvz_Vlat = 3 * xvz_Rad * sin(xvz_Lat) * sin(xvz_Lon)'
  write(6,*)'    xvz_VRad = 2 * xvz_Rad * sin(xvz_Lat) * cos(xvz_Lon)'

  call checkresult

  call MessageNotify('M','wt_mpi_derivative_test7', &
       'wt_module derivative function test #7 succeeded!')

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

  stop
contains

 !------- 結果比較 -------
  subroutine checkresult

    call wt_Potential2RotationMPI(&
         xvz_RotVLon,xvz_RotVLat,xvz_RotVRad, wt_VTor, wt_VPol )

    write(6,*)
    write(6,*)'Checking RotV_Lon '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_RotVLon(i,j,k)-xvz_RotV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_RotVLon(i,j,k),xvz_RotV0Lon(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Lat '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_RotVLat(i,j,k)-xvz_RotV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_RotVLat(i,j,k),xvz_RotV0Lat(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Rad '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_RotVRad(i,j,k)-xvz_RotV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_RotVRad(i,j,k),xvz_RotV0rad(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wt_mpi_derivative_test7

