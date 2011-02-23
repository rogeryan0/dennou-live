!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_mpi_module テストプログラム
!
!      関数のテスト
!      xvz_GradLon_wt, xvz_GradLat_wt, wt_Div_xvz_xvz_xvz
!  
!履歴  2002/05/30  竹広真一
!
program wt_mpi_derivative_test2

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), allocatable :: xvz_VLon(:,:,:)
  real(8), allocatable :: xvz_VLat(:,:,:)
  real(8), allocatable :: xvz_VRad(:,:,:)
  real(8), allocatable :: xvz_GradLon(:,:,:)
  real(8), allocatable :: xvz_GradLat(:,:,:)
  real(8), allocatable :: xvz_Div(:,:,:)
  real(8), allocatable :: xvz_Data(:,:,:)
  real(8), allocatable :: xvz_Psi(:,:,:)

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test2', &
       'wt_mpi_module derivative function test #2')

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_VLon(0:im-1,jc,0:km))
  allocate(xvz_VLat(0:im-1,jc,0:km))
  allocate(xvz_VRad(0:im-1,jc,0:km))
  allocate(xvz_GradLon(0:im-1,jc,0:km))
  allocate(xvz_GradLat(0:im-1,jc,0:km))
  allocate(xvz_Div(0:im-1,jc,0:km))
  allocate(xvz_Data(0:im-1,jc,0:km))
  allocate(xvz_Psi(0:im-1,jc,0:km))

  write( 6,* ) 'Test for xvz_GradLon_wt, xvz_GradLat_wt, wt_Div_xvz_xvz_xvz.'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  xvz_VRad = 0
  xvz_Psi = xvz_Rad**n * cos(xvz_Lat)*sin(xvz_Lon)   ! r**2 P_1^1

  xvz_GradLon =  xvz_Rad**(n-1)*cos(xvz_Lon)
  xvz_GradLat = -xvz_Rad**(n-1)*sin(xvz_Lat)*sin(xvz_Lon)

  xvz_Div = - 2* xvz_Psi/xvz_Rad**2

  write(6,*)
  write(6,*)'P11 field'
  call checkresult

! ----------------- 例 2 --------------------
  xvz_VRad = 0
  xvz_Psi = xvz_Rad**n * cos(xvz_Lat)*sin(xvz_Lat) * sin(xvz_Lon) ! P_2^1

  xvz_GradLon =  xvz_Rad**(n-1)*sin(xvz_Lat)*cos(xvz_Lon)
  xvz_GradLat =  xvz_Rad**(n-1)*cos(2*xvz_Lat)*sin(xvz_Lon)

  xvz_Div = - 6* xvz_Psi/xvz_Rad**2

  write(6,*)
  write(6,*)'P21 field'
  call checkresult

  call MessageNotify('M','wt_test_derivative2', &
       'wt_module derivative function test #2 succeeded!')

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

  stop
contains

  subroutine checkresult

    xvz_VLon =  xvz_GradLon_wt(wt_xvz(xvz_Psi))
    write(6,*)'Checking GradLon (1/r cos(Lat) d/dLon)'
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_VLon(i,j,k)-xvz_GradLon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_VLon(i,j,k), xvz_GradLon(i,j,k)
                call MessageNotify('E','wt_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xvz_VLat =  xvz_GradLat_wt(wt_xvz(xvz_Psi))
    write(6,*)'Checking GradLat (1/r dLat)'
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_VLat(i,j,k)-xvz_GradLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_VLat(i,j,k), xvz_GradLat(i,j,k)
                call MessageNotify('E','wt_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xvz_Data = xvz_wt(wt_Div_xvz_xvz_xvz(xvz_VLon,xvz_VLat,xvz_VRad))

    write(6,*)'Checking Divergence'
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_Div(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_Div(i,j,k)
                call MessageNotify('E','wt_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wt_mpi_derivative_test2

