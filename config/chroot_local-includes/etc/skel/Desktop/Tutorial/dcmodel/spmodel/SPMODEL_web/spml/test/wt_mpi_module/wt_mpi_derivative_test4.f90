!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_mpi_module テストプログラム
!
!   wt_KxRGrad_wt, xvz_KGrad_wt, wt_QOperator_wt のテスト
!
!履歴  2008/05/30  竹広真一
!  
program wt_mpi_derivative_test4

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), allocatable  :: xvz_Data(:,:,:)
  real(8), allocatable  :: xvz_Psi(:,:,:)
  real(8), allocatable  :: xvz_KxRGrad(:,:,:)
  real(8), allocatable  :: xvz_KGrad(:,:,:)
  real(8), allocatable  :: xvz_QOperator(:,:,:)

  integer, parameter :: n=2
  real(8), parameter :: eps = 1D-8

  integer :: i,j,k

  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test4', &
       'wt_mpi_module derivative function test #4')

  write( 6,* ) 'Test for wt_KxRGrad_wt, xvz_KGrad_wat, wt_QOperator_wt'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_mpi_initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_Data(0:im-1,jc,0:km))
  allocate(xvz_Psi(0:im-1,jc,0:km))
  allocate(xvz_KxRGrad(0:im-1,jc,0:km))
  allocate(xvz_KGrad(0:im-1,jc,0:km))
  allocate(xvz_QOperator(0:im-1,jc,0:km))

! ----------------- 例 1 --------------------
  xvz_Psi = xvz_rad**n * cos(xvz_lat)*sin(xvz_lon)   ! r**2 Y_1^1

  xvz_KxRGrad = xvz_rad**n * cos(xvz_lat)*cos(xvz_lon)

  ! k ・▽ r**n Y_1^1 = (n-1)*r**(n-1)* Y_2^1
  xvz_KGrad = (n-1)*xvz_rad**(n-1)* cos(xvz_lat)*sin(xvz_lat)*sin(xvz_lon) 

  ! Q r**n Y_1^1 = -3*(n-1)*r**(n-1)* Y_2^1
  xvz_QOperator = - 3*(n-1)*xvz_rad**(n-1)* cos(xvz_lat)*sin(xvz_lat)*sin(xvz_lon) 

  write(6,*)
  write(6,*)'Y_1^1 field'
  call checkresult

! ----------------- 例 2 --------------------
  xvz_Psi = cos(xvz_lat)*sin(xvz_lat) * sin(xvz_lon) ! Y_2^1

  xvz_KxRGrad = cos(xvz_lat)*sin(xvz_lat) * cos(xvz_lon) ! Y_2^1

  ! k・▽ Y_2^1 = (- 4 Y_3^1 / 15 - Y_1^1 /5) 
  xvz_KGrad = cos(2*xvz_lat)*cos(xvz_lat)*sin(xvz_lon)/xvz_rad

  xvz_QOperator = (16*sin(xvz_lat)**2 - 5)*cos(xvz_lat)*sin(xvz_lon)/xvz_rad

  write(6,*)
  write(6,*)'Y_2^1 field'
  call checkresult

  call MessageNotify('M','wt_mpi_derivative_test4', &
       'wt_mpi_module derivative function test #4 succeeded!')

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

  stop
contains

  subroutine checkresult
    write(6,*)'Checking k x r grad '
    xvz_Data = xvz_wt(wt_KxRGrad_wt(wt_xvz(xvz_Psi)))

    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_KxRGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_KxRGrad(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking k grad '
    xvz_Data = xvz_KGrad_wt(wt_xvz(xvz_Psi))

    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_KGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_KGrad(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking Q operator '
    xvz_Data = xvz_wt(wt_QOperator_wt(wt_xvz(xvz_Psi)))

    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_QOperator(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_QOperator(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult

end program wt_mpi_derivative_test4

