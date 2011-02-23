!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module テストプログラム
!
!履歴  2008/05/04  竹広真一
!
program eee_intavr

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=64, km=16          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=5           ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8)            :: zyx_Data(0:km-1,0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: yx_Data(0:jm-1,0:im-1)            ! 格子データ
  real(8)            :: zx_Data(0:km-1,0:im-1)            ! 格子データ
  real(8)            :: zy_Data(0:km-1,0:jm-1)            ! 格子データ
  real(8)            :: x_Data(0:im-1)                    ! 格子データ
  real(8)            :: y_Data(0:jm-1)                    ! 格子データ
  real(8)            :: z_Data(0:km-1)                    ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','eee_test_intavr', &
       'eee_module integral/average function tests')

 !---------------- 座標値の設定 ---------------------
  call eee_initial(im,jm,km,lm,mm,nm)


!-------------------------- 3 次元データ ------------------------------
  zyx_Data = sin(zyx_X) * cos(zyx_Y) * cos(zyx_Z)
  write(6,*) 'f = sin(X)*cos(Y)*cos(Z)'

    call check0d(IntZYX_zyx(zyx_Data)-0.0, eps, 'IntZYX_zyx') 
    call check0d(AvrZYX_zyx(zyx_Data)-0.0, eps, 'AvrXYX_zyx')

    call check1d(x_IntZY_zyx(zyx_Data) - 0.0, eps, 'x_IntZY_zyx') 
    call check1d(x_AvrZY_zyx(zyx_Data) - 0.0, eps, 'x_AvrZY_zyx') 
    call check1d(y_IntZX_zyx(zyx_Data) - 0.0, eps, 'y_IntZX_zyx') 
    call check1d(y_AvrZX_zyx(zyx_Data) - 0.0, eps, 'y_AvrZX_zyx') 
    call check1d(z_IntYX_zyx(zyx_Data) - 0.0, eps, 'x_IntZY_zyx') 
    call check1d(z_AvrYX_zyx(zyx_Data) - 0.0, eps, 'x_AvrZY_zyx') 

    call check2d(yx_IntZ_zyx(zyx_Data) - 0.0, eps, 'yx_IntZ_zyx')
    call check2d(yx_AvrZ_zyx(zyx_Data) - 0.0, eps, 'yx_AvrZ_zyx')
    call check2d(zx_IntY_zyx(zyx_Data) - 0.0, eps, 'zx_IntY_zyx')
    call check2d(zx_AvrY_zyx(zyx_Data) - 0.0, eps, 'zx_AvrY_zyx')
    call check2d(zy_IntX_zyx(zyx_Data) - 0.0, eps, 'zy_IntX_zyx')
    call check2d(zy_AvrX_zyx(zyx_Data) - 0.0, eps, 'zy_AvrX_zyx')

  zyx_Data = (1 - sin(zyx_X)) * cos(zyx_Y)**2 * cos(zyx_Z)**2
  write(6,*)
  write(6,*) 'f = (1-sin(X)) * cos(Y)**2 * cos(Z)**2'

    call check0d(IntZYX_zyx(zyx_Data)-2.0*pi**3, eps, 'IntZYX_zyx') 
    call check0d(AvrZYX_zyx(zyx_Data)-0.25, eps, 'AvrXYX_zyx')

    call check1d(x_IntZY_zyx(zyx_Data) - (1 - sin(x_X))*pi**2, eps, 'x_IntZY_zyx') 
    call check1d(x_AvrZY_zyx(zyx_Data) - (1 - sin(x_X))/4, eps, 'x_AvrZY_zyx') 
    call check1d(y_IntZX_zyx(zyx_Data) - cos(y_Y)**2 * 2*pi**2, eps, 'y_IntZX_zyx') 
    call check1d(y_AvrZX_zyx(zyx_Data) - cos(y_Y)**2 /2 , eps, 'y_AvrZX_zyx') 
    call check1d(z_IntYX_zyx(zyx_Data) - cos(z_Z)**2 *2*pi**2 , eps, 'x_IntZY_zyx') 
    call check1d(z_AvrYX_zyx(zyx_Data) - cos(z_Z)**2 /2, eps, 'x_AvrZY_zyx') 

    call check2d(yx_IntZ_zyx(zyx_Data) &
         - (1 - sin(zyx_X(1,:,:)))*cos(zyx_Y(1,:,:))**2*pi, eps, 'yx_IntZ_zyx')
    call check2d(yx_AvrZ_zyx(zyx_Data) &
         - (1 - sin(zyx_X(1,:,:)))*cos(zyx_Y(1,:,:))**2/2,eps, 'yx_AvrZ_zyx')
    call check2d(zx_IntY_zyx(zyx_Data) &
         - (1 - sin(zyx_X(:,1,:)))*cos(zyx_Z(:,1,:))**2*pi, eps, 'zx_IntY_zyx')
    call check2d(zx_AvrY_zyx(zyx_Data) &
         - (1 - sin(zyx_X(:,1,:)))*cos(zyx_Z(:,1,:))**2/2, eps,'zx_AvrY_zyx')
    call check2d(zy_IntX_zyx(zyx_Data) &
         - cos(zyx_Y(:,:,1))**2 * cos(zyx_Z(:,:,1))**2*2*pi, eps, 'zy_IntX_zyx')
    call check2d(zy_AvrX_zyx(zyx_Data) &
         - cos(zyx_Y(:,:,1))**2 * cos(zyx_Z(:,:,1))**2, eps, 'zy_AvrX_zyx')

!-------------------------- 2 次元データ ------------------------------
  yx_Data = sin(zyx_X(1,:,:)) * cos(zyx_Y(1,:,:))
  write(6,*) 'f = sin(X)*cos(Y)'

    call check0d(IntYX_yx(yx_Data)-0.0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data)-0.0, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 0.0, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - 0.0, eps, 'y_AvrX_yx') 

    call check1d(x_IntY_yx(yx_Data) - 0.0, eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 0.0, eps, 'x_AvrY_yx')

  yx_Data = (1 - sin(zyx_X(1,:,:))) * cos(zyx_Y(1,:,:))**2
  write(6,*)
  write(6,*) 'f = (1-sin(X)) * cos(Y)**2'

    call check0d(IntYX_yx(yx_Data) - 2.0*pi**2, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data) - 0.5, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 2*cos(y_Y)**2*pi, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - cos(y_Y)**2, eps, 'y_AvrX_yx')

    call check1d(x_IntY_yx(yx_Data) - (1 - sin(x_X))*pi, eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 0.5*(1 - sin(x_X)), eps, 'x_AvrY_yx')

  zx_Data = sin(zyx_X(:,1,:)) * cos(zyx_Z(:,1,:))
  write(6,*) 'f = sin(X)*cos(Y)'

    call check0d(IntZX_zx(zx_Data)-0.0, eps, 'IntZX_zx') 
    call check0d(AvrZX_zx(zx_Data)-0.0, eps, 'AvrZX_zx')

    call check1d(z_IntX_zx(zx_Data) - 0.0, eps, 'z_IntX_zx') 
    call check1d(z_AvrX_zx(zx_Data) - 0.0, eps, 'z_AvrX_zx') 

    call check1d(x_IntZ_zx(zx_Data) - 0.0, eps, 'x_IntZ_zx')
    call check1d(x_AvrZ_zx(zx_Data) - 0.0, eps, 'x_AvrZ_zx')

  zx_Data = (1 - sin(zyx_X(:,1,:))) * cos(zyx_Z(:,1,:))**2
  write(6,*)
  write(6,*) 'f = (1-sin(X)) * cos(Z)**2'

    call check0d(IntZX_zx(zx_Data) - 2.0*pi**2, eps, 'IntZX_zx') 
    call check0d(AvrZX_zx(zx_Data) - 0.5, eps, 'AvrZX_zx')

    call check1d(z_IntX_zx(zx_Data) - 2*cos(z_Z)**2*pi, eps, 'z_IntX_zx') 
    call check1d(z_AvrX_zx(zx_Data) - cos(z_Z)**2, eps, 'z_AvrX_zx')

    call check1d(x_IntZ_zx(zx_Data) - (1 - sin(x_X))*pi, eps, 'x_IntZ_zx')
    call check1d(x_AvrZ_zx(zx_Data) - 0.5*(1 - sin(x_X)), eps, 'x_AvrZ_zx')

  zy_Data = sin(zyx_Y(:,:,1)) * cos(zyx_Z(:,:,1))
  write(6,*) 'f = sin(Y)*cos(Y)'

    call check0d(IntZY_zy(zy_Data)-0.0, eps, 'IntZY_zy') 
    call check0d(AvrZY_zy(zy_Data)-0.0, eps, 'AvrZY_zy')

    call check1d(z_IntY_zy(zy_Data) - 0.0, eps, 'z_IntY_zy') 
    call check1d(z_AvrY_zy(zy_Data) - 0.0, eps, 'z_AvrY_zy') 

    call check1d(y_IntZ_zy(zy_Data) - 0.0, eps, 'y_IntZ_zy')
    call check1d(y_AvrZ_zy(zy_Data) - 0.0, eps, 'y_AvrZ_zy')

  zy_Data = (1 - sin(zyx_Y(:,:,1))) * cos(zyx_Z(:,:,1))**2
  write(6,*)
  write(6,*) 'f = (1-sin(Y)) * cos(Z)**2'

    call check0d(IntZY_zy(zy_Data) - 2.0*pi**2, eps, 'IntZY_zy') 
    call check0d(AvrZY_zy(zy_Data) - 0.5, eps, 'AvrZY_zy')

    call check1d(z_IntY_zy(zy_Data) - 2*cos(z_Z)**2*pi, eps, 'z_IntY_zy') 
    call check1d(z_AvrY_zy(zy_Data) - cos(z_Z)**2, eps, 'z_AvrY_zy')

    call check1d(y_IntZ_zy(zy_Data) - (1 - sin(y_Y))*pi, eps, 'y_IntZ_zy')
    call check1d(y_AvrZ_zy(zy_Data) - 0.5*(1 - sin(y_Y)), eps, 'y_AvrZ_zy')

!-------------------------- 1 次元データ ------------------------------
  x_Data = 1 - sin(2*x_X)
  write(6,*)
  write(6,*) 'f = 1-sin(2*X)'

    call check0d(IntX_x(x_Data) - 2.0d0*pi, eps, 'IntX_x')
    call check0d(AvrX_x(x_Data) - 1.0d0, eps, 'AvrX_x')

  y_Data = cos(y_Y)**2
  write(6,*)
  write(6,*) 'f = cos(Y)**2'

    call check0d(IntY_y(y_Data) - 1.0*pi, eps, 'IntY_y')
    call check0d(AvrY_y(y_Data) - 0.5, eps, 'AvrY_y')

  z_Data = 1 - sin(2*z_Z)
  write(6,*)
  write(6,*) 'f = 1-sin(2*Z)'

    call check0d(IntZ_z(z_Data) - 2.0d0*pi, eps, 'IntZ_z')
    call check0d(AvrZ_z(z_Data) - 1.0d0, eps, 'AvrZ_z')

  call MessageNotify('M','eee_test_intavr', &
       'eee_module integral/average function tests succeeded!')

 stop
contains

  subroutine check2d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:)                ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i,j

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do j=1,size(var,2)
       do i=1,size(var,1)
          if (abs(var(i,j)) .gt. eps ) then
             write(6,*) '    Value larger than EPS : i= ', i, var(i,j)
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
    integer i

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
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

end program eee_intavr
