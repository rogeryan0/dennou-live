!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_mpi_module テストプログラム
!
!      wt_DivRad_wt のテスト (1/r^2 d/dr r^2)
!  
!履歴  2008/05/30  竹広真一
!
program wt_mpi_derivative_test1

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), allocatable ::  wt_Data(:,:)
  real(8), allocatable ::  xvz_Data(:,:,:)
  real(8), allocatable ::  xvz_Data1(:,:,:)
  real(8), parameter :: eps=1.0D-10
  integer, parameter :: nmin=1, nmax=10

  integer :: n

  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test1', &
       'wt_mpi_module derivative function test #1')

  write( 6,* ) 'Test for wt_Divrat_wt'
!!$  write(6,*) 'exponent N, of Radial dependence for r^N?'
!!$
!!$  write(6,*) 'n?'
!!$  read(5,*) n

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(wt_Data((nm+1)**2,0:lm))
  allocate(xvz_Data(0:im-1,jc,0:km))
  allocate(xvz_Data1(0:im-1,jc,0:km))

  do n=nmin,nmax
     write(6,*) 'n=',n
     xvz_Data = xvz_Rad**n
     xvz_Data1 = (n+2)*xvz_Rad**(n-1)

     xvz_Data = xvz_wt(wt_DivRad_wt(wt_xvz(xvz_Data)))

     if ( maxval(abs(xvz_Data-xvz_Data1)) .gt. eps ) then
        write( 6,* ) 'Error is...', maxval(abs(xvz_Data-xvz_Data1))
        call MessageNotify('E','wt_mpi_derivative_test1', &
             'Derivative error too large.')
     end if

  enddo
  call MessageNotify('M','wt_mpi_derivative_test1', &
       'wt_mpi_module derivative function test #1 succeeded!')

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

end program wt_mpi_derivative_test1
