!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_mpi_module テストプログラム
!
!      関数のテスト
!           wt_DivLon_xvz, wt_DivLat_xvz
!
!履歴  2008/05/30  竹広真一
!  
program wt_mpi_derivative_test3

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), allocatable :: xvz_VLon(:,:,:)
  real(8), allocatable :: xvz_VLat(:,:,:)
  real(8), allocatable :: xvz_DivLon(:,:,:)
  real(8), allocatable :: xvz_DivLat(:,:,:)
  real(8), allocatable :: xvz_Data(:,:,:)

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test3', &
       'wt_mpi_module derivative function test #3')

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_VLon(0:im-1,jc,0:km))
  allocate(xvz_VLat(0:im-1,jc,0:km))
  allocate(xvz_DivLon(0:im-1,jc,0:km))
  allocate(xvz_DivLat(0:im-1,jc,0:km))
  allocate(xvz_Data(0:im-1,jc,0:km))

  write( 6,* ) 'Test for wt_DivLon_xvz, wt_DivLat_xvz'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  xvz_VLon   = xvz_rad**n * cos(xvz_Lat)**2*sin(xvz_Lon)
  xvz_DivLon = xvz_rad**(n-1)*cos(xvz_Lat)*cos(xvz_Lon)

  xvz_VLat   = xvz_rad**n * cos(xvz_Lat)**2*sin(xvz_Lon)
  xvz_DivLat = -3*xvz_rad**(n-1)*cos(xvz_Lat)*sin(xvz_Lat)*sin(xvz_Lon)

  write(6,*)
  write(6,*)'Example 1'
  write(6,*)'  xvz_VLon=xvz_rad**n * cos(xvz_Lat)**2*sin(xvz_Lon)'
  write(6,*)'  xvz_VLat=xvz_rad**n * cos(xvz_Lat)**2*sin(xvz_Lon)'
  call checkresult

  call MessageNotify('M','wt_test_derivative3', &
       'wt_module derivative function test #3 succeeded!')

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

  stop
contains

  subroutine checkresult

    write(6,*)'Checking DivLon (1/r cos(Lat) dLon)'
    xvz_Data =  xvz_wt(wt_DivLon_xvz(xvz_VLon))

    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_DivLon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_DivLon(i,j,k)
                call MessageNotify('E','wt_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xvz_Data =  xvz_wt(wt_DivLat_xvz(xvz_VLat))
    write(6,*)'Checking DivLat (1/r cos(Lat) dLat cos(Lat))'
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_DivLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_DivLat(i,j,k)
                call MessageNotify('E','wt_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult


end program wt_mpi_derivative_test3

