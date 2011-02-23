!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_mpi_module テストプログラム
!
!履歴  2008/05/21  竹広真一
!
program eee_mpi_test_intavr

  use dc_message, only : MessageNotify
  use eee_mpi_module
  implicit none
  include 'mpif.h'

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=16, km=64          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=5, nm=21           ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8), allocatable :: zxv_Data(:,:,:)         ! 格子データ
  real(8), allocatable :: xv_Data(:,:)            ! 格子データ
  real(8), allocatable :: zx_Data(:,:)            ! 格子データ
  real(8), allocatable :: zv_Data(:,:)            ! 格子データ
  real(8), allocatable :: x_Data(:)               ! 格子データ
  real(8), allocatable :: v_Data(:)               ! 格子データ
  real(8), allocatable :: z_Data(:)               ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  integer            :: np, ip, ierr

  call MessageNotify('M','eee_mpi_test_intavr', &
       'eee_mpi_module integral/average function tests')

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

 !---------------- 座標値の設定 ---------------------
  call eee_mpi_initial(im,jm,km,lm,mm,nm)

 !---------------- 変数の割付け ---------------------
  allocate(zxv_Data(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(xv_Data(0:im-1,js(ip):je(ip)))
  allocate(zv_Data(0:km-1,js(ip):je(ip)))
  allocate(zx_Data(0:km-1,0:im-1))
  allocate(z_Data(0:km-1))
  allocate(x_Data(0:im-1))
  allocate(v_Data(js(ip):je(ip)))

!-------------------------- 3 次元データ ------------------------------
  zxv_Data = sin(zxv_X) * cos(zxv_Y) * cos(zxv_Z)
  write(6,*) 'f = sin(X)*cos(Y)*cos(Z)'

    call check0d(IntZXV_zxv(zxv_Data)-0.0, eps, 'IntZXV_zxv') 
    call check0d(AvrZXV_zxv(zxv_Data)-0.0, eps, 'AvrZXV_zxv')

    call check1d(x_IntZV_zxv(zxv_Data) - 0.0, eps, 'x_IntZV_zxv') 
    call check1d(x_AvrZV_zxv(zxv_Data) - 0.0, eps, 'x_AvrZV_zxv') 
    call check1d(v_IntZX_zxv(zxv_Data) - 0.0, eps, 'v_IntZX_zxv') 
    call check1d(v_AvrZX_zxv(zxv_Data) - 0.0, eps, 'v_AvrZX_zxv') 
    call check1d(z_IntXV_zxv(zxv_Data) - 0.0, eps, 'x_IntXV_zxv') 
    call check1d(z_AvrXV_zxv(zxv_Data) - 0.0, eps, 'x_AvrXV_zxv') 

    call check2d(xv_IntZ_zxv(zxv_Data) - 0.0, eps, 'xv_IntZ_zxv')
    call check2d(xv_AvrZ_zxv(zxv_Data) - 0.0, eps, 'xv_AvrZ_zxv')
    call check2d(zx_IntV_zxv(zxv_Data) - 0.0, eps, 'zx_IntV_zxv')
    call check2d(zx_AvrV_zxv(zxv_Data) - 0.0, eps, 'zx_AvrV_zxv')
    call check2d(zv_IntX_zxv(zxv_Data) - 0.0, eps, 'zv_IntX_zxv')
    call check2d(zv_AvrX_zxv(zxv_Data) - 0.0, eps, 'zv_AvrX_zxv')

  zxv_Data = (1 - sin(zxv_X)) * cos(zxv_Y)**2 * cos(zxv_Z)**2
  write(6,*)
  write(6,*) 'f = (1-sin(X)) * cos(Y)**2 * cos(Z)**2'

    call check0d(IntZXV_zxv(zxv_Data)-2.0*pi**3, eps, 'IntZXV_zxv') 
    call check0d(AvrZXV_zxv(zxv_Data)-0.25, eps, 'AvrZXV_zxv')

    call check1d(x_IntZV_zxv(zxv_Data) - (1 - sin(x_X))*pi**2, eps, 'x_IntZV_zxv') 
    call check1d(x_AvrZV_zxv(zxv_Data) - (1 - sin(x_X))/4, eps, 'x_AvrZV_zxv') 
    call check1d(v_IntZX_zxv(zxv_Data) - cos(v_Y)**2 * 2*pi**2, eps, 'v_IntZX_zxv') 
    call check1d(v_AvrZX_zxv(zxv_Data) - cos(v_Y)**2 /2 , eps, 'v_AvrZX_zxv') 
    call check1d(z_IntXV_zxv(zxv_Data) - cos(z_Z)**2 *2*pi**2 , eps, 'x_IntXV_zxv') 
    call check1d(z_AvrXV_zxv(zxv_Data) - cos(z_Z)**2 /2, eps, 'x_AvrXV_zxv') 

    call check2d(xv_IntZ_zxv(zxv_Data) &
         - (1 - sin(zxv_X(1,:,:)))*cos(zxv_Y(1,:,:))**2*pi, eps, 'xv_IntZ_zxv')
    call check2d(xv_AvrZ_zxv(zxv_Data) &
         - (1 - sin(zxv_X(1,:,:)))*cos(zxv_Y(1,:,:))**2/2,eps, 'xv_AvrZ_zxv')
    call check2d(zv_IntX_zxv(zxv_Data) &
         - cos(zxv_Y(:,1,:))**2 * cos(zxv_Z(:,1,:))**2*2*pi, eps, 'zv_IntX_zxv')
    call check2d(zv_AvrX_zxv(zxv_Data) &
         - cos(zxv_Y(:,1,:))**2 * cos(zxv_Z(:,1,:))**2, eps, 'zv_AvrX_zxv')

    call check2d(zx_IntV_zxv(zxv_Data) &
         - (1 - sin(zxv_X(:,:,js(ip))))*cos(zxv_Z(:,:,js(ip)))**2*pi, eps, 'zx_IntV_zxv')
    call check2d(zx_AvrV_zxv(zxv_Data) &
         - (1 - sin(zxv_X(:,:,js(ip))))*cos(zxv_Z(:,:,js(ip)))**2/2, eps,'zx_AvrV_zxv')

!-------------------------- 2 次元データ ------------------------------
  xv_Data = sin(zxv_X(1,:,:)) * cos(zxv_Y(1,:,:))
  write(6,*) 'f = sin(X)*cos(Y)'

    call check0d(IntXV_xv(xv_Data)-0.0, eps, 'IntXV_xv') 
    call check0d(AvrXV_xv(xv_Data)-0.0, eps, 'AvrXV_xv')

    call check1d(v_IntX_xv(xv_Data) - 0.0, eps, 'v_IntX_xv') 
    call check1d(v_AvrX_xv(xv_Data) - 0.0, eps, 'v_AvrX_xv') 

    call check1d(x_IntV_xv(xv_Data) - 0.0, eps, 'x_IntV_xv')
    call check1d(x_AvrV_xv(xv_Data) - 0.0, eps, 'x_AvrV_xv')

  xv_Data = (1 - sin(zxv_X(1,:,:))) * cos(zxv_Y(1,:,:))**2
  write(6,*)
  write(6,*) 'f = (1-sin(X)) * cos(Y)**2'

    call check0d(IntXV_xv(xv_Data) - 2.0*pi**2, eps, 'IntXV_xv') 
    call check0d(AvrXV_xv(xv_Data) - 0.5, eps, 'AvrXV_xv')

    call check1d(v_IntX_xv(xv_Data) - 2*cos(v_Y)**2*pi, eps, 'v_IntX_xv') 
    call check1d(v_AvrX_xv(xv_Data) - cos(v_Y)**2, eps, 'v_AvrX_xv')

    call check1d(x_IntV_xv(xv_Data) - (1 - sin(x_X))*pi, eps, 'x_IntV_xv')
    call check1d(x_AvrV_xv(xv_Data) - 0.5*(1 - sin(x_X)), eps, 'x_AvrV_xv')

  zx_Data = sin(zxv_X(:,:,js(ip))) * cos(zxv_Z(:,:,js(ip)))
  write(6,*) 'f = sin(X)*cos(Y)'

    call check0d(IntZX_zx(zx_Data)-0.0, eps, 'IntZX_zx') 
    call check0d(AvrZX_zx(zx_Data)-0.0, eps, 'AvrZX_zx')

    call check1d(z_IntX_zx(zx_Data) - 0.0, eps, 'z_IntX_zx') 
    call check1d(z_AvrX_zx(zx_Data) - 0.0, eps, 'z_AvrX_zx') 

    call check1d(x_IntZ_zx(zx_Data) - 0.0, eps, 'x_IntZ_zx')
    call check1d(x_AvrZ_zx(zx_Data) - 0.0, eps, 'x_AvrZ_zx')

  zx_Data = (1 - sin(zxv_X(:,:,js(ip)))) * cos(zxv_Z(:,:,js(ip)))**2
  write(6,*)
  write(6,*) 'f = (1-sin(X)) * cos(Z)**2'

    call check0d(IntZX_zx(zx_Data) - 2.0*pi**2, eps, 'IntZX_zx') 
    call check0d(AvrZX_zx(zx_Data) - 0.5, eps, 'AvrZX_zx')

    call check1d(z_IntX_zx(zx_Data) - 2*cos(z_Z)**2*pi, eps, 'z_IntX_zx') 
    call check1d(z_AvrX_zx(zx_Data) - cos(z_Z)**2, eps, 'z_AvrX_zx')

    call check1d(x_IntZ_zx(zx_Data) - (1 - sin(x_X))*pi, eps, 'x_IntZ_zx')
    call check1d(x_AvrZ_zx(zx_Data) - 0.5*(1 - sin(x_X)), eps, 'x_AvrZ_zx')

  zv_Data = sin(zxv_Y(:,1,:)) * cos(zxv_Z(:,1,:))
  write(6,*) 'f = sin(Y)*cos(Y)'

    call check0d(IntZV_zv(zv_Data)-0.0, eps, 'IntZV_zv') 
    call check0d(AvrZV_zv(zv_Data)-0.0, eps, 'AvrZV_zv')

    call check1d(z_IntV_zv(zv_Data) - 0.0, eps, 'z_IntV_zv') 
    call check1d(z_AvrV_zv(zv_Data) - 0.0, eps, 'z_AvrV_zv') 

    call check1d(v_IntZ_zv(zv_Data) - 0.0, eps, 'v_IntZ_zv')
    call check1d(v_AvrZ_zv(zv_Data) - 0.0, eps, 'v_AvrZ_zv')

  zv_Data = (1 - sin(zxv_Y(:,1,:))) * cos(zxv_Z(:,1,:))**2
  write(6,*)
  write(6,*) 'f = (1-sin(Y)) * cos(Z)**2'

    call check0d(IntZV_zv(zv_Data) - 2.0*pi**2, eps, 'IntZV_zv') 
    call check0d(AvrZV_zv(zv_Data) - 0.5, eps, 'AvrZV_zv')

    call check1d(z_IntV_zv(zv_Data) - 2*cos(z_Z)**2*pi, eps, 'z_IntV_zv') 
    call check1d(z_AvrV_zv(zv_Data) - cos(z_Z)**2, eps, 'z_AvrV_zv')

    call check1d(v_IntZ_zv(zv_Data) - (1 - sin(v_Y))*pi, eps, 'v_IntZ_zv')
    call check1d(v_AvrZ_zv(zv_Data) - 0.5*(1 - sin(v_Y)), eps, 'v_AvrZ_zv')

!-------------------------- 1 次元データ ------------------------------
  x_Data = 1 - sin(2*x_X)
  write(6,*)
  write(6,*) 'f = 1-sin(2*X)'

    call check0d(IntX_x(x_Data) - 2.0d0*pi, eps, 'IntX_x')
    call check0d(AvrX_x(x_Data) - 1.0d0, eps, 'AvrX_x')

  v_Data = cos(v_Y)**2
  write(6,*)
  write(6,*) 'f = cos(Y)**2'

    call check0d(IntV_v(v_Data) - 1.0*pi, eps, 'IntV_v')
    call check0d(AvrV_v(v_Data) - 0.5, eps, 'AvrV_v')

  z_Data = 1 - sin(2*z_Z)
  write(6,*)
  write(6,*) 'f = 1-sin(2*Z)'

    call check0d(IntZ_z(z_Data) - 2.0d0*pi, eps, 'IntZ_z')
    call check0d(AvrZ_z(z_Data) - 1.0d0, eps, 'AvrZ_z')

  call MessageNotify('M','eee_mpi_test_intavr', &
       'eee_mpi_module integral/average function tests succeeded!')

  call MPI_FINALIZE(IERR)

 stop
contains

  subroutine check2d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:)                ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    character(len=3) ::cip
    integer i,j

    if ( present(funcname) )then
       write(cip,'(I3)') IP
       write(6,*) '  Checking ', funcname, ' for IP='//trim(adjustl(cip))
    endif

    do j=1,size(var,2)
       do i=1,size(var,1)
          if (abs(var(i,j)) .gt. eps ) then
             write(6,*) '    Value larger than EPS : i= ', i,' j= ',j, var(i,j)
             call MessageNotify('E','eee_test_intavr', &
                  'Integral/average error too large')
          endif
       enddo
    enddo
  end subroutine check2d

  subroutine check1d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:)                  ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    character(len=3) ::cip
    integer i

    if ( present(funcname) )then
       write(cip,'(I3)') IP
       write(6,*) '  Checking ', funcname, ' for IP='//trim(adjustl(cip))
    endif

    do i=1,size(var)
       if (abs(var(i)) .gt. eps ) then
          write(6,*) '    Value larger than EPS : i= ', i, var(i)
          call MessageNotify('E','eee_test_intavr', &
               'Integral/average error too large')
       endif
    enddo
  end subroutine check1d

  subroutine check0d(var,eps,funcname)   ! var の絶対値が eps 以上だと出力
    real(8) :: var                       ! 判定する配列
    real(8) :: eps                       ! 誤差
    real(8) :: vartmp(1)                 ! 
    character(len=*), optional :: funcname

    vartmp(1) = var
    if ( present(funcname) ) then
       call check1d(vartmp,eps,funcname)
    else
       call check1d(vartmp,eps)
    endif
  end subroutine check0d

end program eee_mpi_test_intavr
