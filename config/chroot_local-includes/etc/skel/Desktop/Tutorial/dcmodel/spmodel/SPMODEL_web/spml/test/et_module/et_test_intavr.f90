!----------------------------------------------------------------------
!     Copyright (c) 2002-2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module テストプログラム (積分・平均計算)
!
!履歴  2002/08/20  竹広真一
!      2007/11/14  竹広真一  x,y 入れ換え. エラーメッセージ追加
!
program et_test_intavr

  use dc_message, only : MessageNotify
  use et_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=32             ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! 格子データ
  real(8)            :: x_Data(0:im-1)          ! 格子データ
  real(8)            :: y_Data(0:jm)            ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','et_test_intvar', &
                     'et_module integrate/averaging function tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  yx_Data = sin(pi*yx_X) * yx_Y**2
  write(6,*) 'f = sin(pi*X)*Y**2'

    call check0d(IntYX_yx(yx_Data)-0.0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data)-0.0, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 0.0, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - 0.0, eps, 'y_AvrX_yx') 

    call check1d(x_IntY_yx(yx_Data) - 2/3.0d0 * sin(pi*x_X), eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 1/3.0d0 * sin(pi*x_X), eps, 'x_AvrY_yx')

  yx_Data = (1 - sin(pi*yx_X)) * cos(pi*yx_Y/2)
  write(6,*)
  write(6,*) 'f = (1-sin(pi*X))*cos(pi*Y/2)'

    call check0d(IntYX_yx(yx_Data) - 8/pi, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data) - 2/pi, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 2*cos(pi*y_Y/2), eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - cos(pi*y_Y/2), eps, 'y_AvrX_yx')

    call check1d(x_IntY_yx(yx_Data) - 4/pi * (1 - sin(pi*x_X)), eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 2/pi * (1 - sin(pi*x_X)), eps, 'x_AvrY_yx')

  x_Data = 1 - sin(2*pi*x_X)
  write(6,*)
  write(6,*) 'f = 1-sin(2*pi*X)'

    call check0d(IntX_x(x_Data) - 2.0d0, eps, 'IntX_x')
    call check0d(AvrX_x(x_Data) - 1.0d0, eps, 'AvrX_x')

  y_Data = cos(pi*y_Y/2)
  write(6,*)
  write(6,*) 'f = cos(pi*Y/2)'

    call check0d(IntY_y(y_Data) - 4/pi, eps, 'IntY_y')
    call check0d(AvrY_y(y_Data) - 2/pi, eps, 'AvrY_y')

  y_Data = cos(pi*y_Y)**2
  write(6,*)
  write(6,*) 'f = cos(pi*Y)**2'

    call check0d(IntY_y(y_Data) - 1.0, eps, 'IntY_y')
    call check0d(AvrY_y(y_Data) - 0.5, eps, 'AvrY_y')

  call MessageNotify('M','et_test_intvar', &
                     'et_module integrate/averaging function tests succeeded!')
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
          call MessageNotify('E','et_test_intavr', &
               'Integrate/Avrage error too large.')
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

end program et_test_intavr

