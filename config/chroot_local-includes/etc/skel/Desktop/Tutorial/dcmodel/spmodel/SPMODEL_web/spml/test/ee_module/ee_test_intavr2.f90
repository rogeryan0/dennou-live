!----------------------------------------------------------------------
!     Copyright (c) 2002-2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module テストプログラム
!
!履歴  2002/08/20  竹広真一
!      2007/11/09  竹広真一  エラーメッセージ追加
!      2008/05/10  竹広真一  解像度複数設定チェック
!
program ee_intavr

  use dc_message, only : MessageNotify
  use ee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16, jm=16            ! 格子点の設定(X,Y)
  integer, parameter :: km=5, lm=5              ! 切断波数の設定(X,Y)

  integer, parameter :: im2=64, jm2=64            ! 格子点の設定(X,Y)
  integer, parameter :: km2=21, lm2=21            ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: x_Data(0:im-1)            ! 格子データ
  real(8)            :: y_Data(0:jm-1)            ! 格子データ

  real(8)            :: yx_Data2(0:jm2-1,0:im2-1) ! 格子データ
  real(8)            :: x_Data2(0:im2-1)          ! 格子データ
  real(8)            :: y_Data2(0:jm2-1)          ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  integer            :: id1, id2

  call MessageNotify('M','ee_test_intavr', &
       'ee_module integral/average function tests')

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)        ! スペクトル初期化
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  ! id1
  write(*,*) 'for id1'
  call ee_ChangeResolutionDomain(id1)

  yx_Data = sin(pi*yx_X) * cos(pi*yx_Y)
  write(6,*) 'f = sin(pi*X)*cos(pi*Y)'

    call check0d(IntYX_yx(yx_Data)-0.0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data)-0.0, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 0.0, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - 0.0, eps, 'y_AvrX_yx') 

    call check1d(x_IntY_yx(yx_Data) - 0.0, eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 0.0, eps, 'x_AvrY_yx')

  yx_Data = (1 - sin(pi*yx_X)) * cos(pi*yx_Y)**2
  write(6,*)
  write(6,*) 'f = (1-sin(pi*X)) * cos(pi*Y)**2'

    call check0d(IntYX_yx(yx_Data) - 2.0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data) - 0.5, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 2*cos(pi*y_Y)**2, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - cos(pi*y_Y)**2, eps, 'y_AvrX_yx')

    call check1d(x_IntY_yx(yx_Data) - (1 - sin(pi*x_X)), eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 0.5*(1 - sin(pi*x_X)), eps, 'x_AvrY_yx')

  x_Data = 1 - sin(2*pi*x_X)
  write(6,*)
  write(6,*) 'f = 1-sin(2*pi*X)'

    call check0d(IntX_x(x_Data) - 2.0d0, eps, 'IntX_x')
    call check0d(AvrX_x(x_Data) - 1.0d0, eps, 'AvrX_x')

  y_Data = cos(pi*y_Y)**2
  write(6,*)
  write(6,*) 'f = cos(pi*Y)**2'

    call check0d(IntY_y(y_Data) - 1.0, eps, 'IntY_y')
    call check0d(AvrY_y(y_Data) - 0.5, eps, 'AvrY_y')

  ! id2
  write(*,*) 'for id2'
  call ee_ChangeResolutionDomain(id2)

  yx_Data2 = sin(pi*yx_X) * cos(pi*yx_Y)
  write(6,*) 'f = sin(pi*X)*cos(pi*Y)'

    call check0d(IntYX_yx(yx_Data2)-0.0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data2)-0.0, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data2) - 0.0, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data2) - 0.0, eps, 'y_AvrX_yx') 

    call check1d(x_IntY_yx(yx_Data2) - 0.0, eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data2) - 0.0, eps, 'x_AvrY_yx')

  yx_Data2 = (1 - sin(pi*yx_X)) * cos(pi*yx_Y)**2
  write(6,*)
  write(6,*) 'f = (1-sin(pi*X)) * cos(pi*Y)**2'

    call check0d(IntYX_yx(yx_Data2) - 2.0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data2) - 0.5, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data2) - 2*cos(pi*y_Y)**2, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data2) - cos(pi*y_Y)**2, eps, 'y_AvrX_yx')

    call check1d(x_IntY_yx(yx_Data2) - (1 - sin(pi*x_X)), eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data2) - 0.5*(1 - sin(pi*x_X)), eps, 'x_AvrY_yx')

  x_Data2 = 1 - sin(2*pi*x_X)
  write(6,*)
  write(6,*) 'f = 1-sin(2*pi*X)'

    call check0d(IntX_x(x_Data2) - 2.0d0, eps, 'IntX_x')
    call check0d(AvrX_x(x_Data2) - 1.0d0, eps, 'AvrX_x')

  y_Data2 = cos(pi*y_Y)**2
  write(6,*)
  write(6,*) 'f = cos(pi*Y)**2'

    call check0d(IntY_y(y_Data2) - 1.0, eps, 'IntY_y')
    call check0d(AvrY_y(y_Data2) - 0.5, eps, 'AvrY_y')


  call MessageNotify('M','ee_test_intavr', &
       'ee_module integral/average function tests succeeded!')

 stop
contains

  subroutine check1d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:)                  ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var)
       if (abs(var(i)) .gt. eps ) then
          write(6,*) '    Value larger than EPS : i= ', i, var(i)
          call MessageNotify('E','ee_test_intavr', &
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

end program ee_intavr
