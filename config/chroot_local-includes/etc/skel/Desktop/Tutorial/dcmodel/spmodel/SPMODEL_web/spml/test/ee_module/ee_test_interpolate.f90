!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module テストプログラム (補間計算)
!
!履歴  2008/10/28  竹広真一
!
program ee_test_transform

  use dc_message, only : MessageNotify
  use ee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=10              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: yx_Data1(0:jm-1,0:im-1)   ! 格子データ
  real(8)            :: ee_Data(-lm:lm,-km:km)    ! スペクトルデータ

  integer            :: k=2,l=5
  integer            :: i, j

 !---- 座標変数など ----
!!$  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: xmin = 0.0d0, xmax=4.0d0
!!$  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0
  real(8), parameter :: ymin = 2.0d0, ymax=4.0d0

  real(8), parameter :: x0 = 2.0/3.0, x1=3.0/5.0d0
  real(8), parameter :: y0 = 1.0/7.0, y1=5.0/9.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','ee_test_transform', &
       'ee_module transform function tests')

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of ee_module : Interpolation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation sin(k*pi*X)*sin(l*pi*Y)')
  call check0d(sin(k*pi*x0)*sin(l*pi*y0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(sin(k*pi*x1)*sin(l*pi*y1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation cos(k*pi*X)*cos(l*pi*Y)')
  call check0d(cos(k*pi*x0)*cos(l*pi*y0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(cos(k*pi*x1)*cos(l*pi*y1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation sin(k*pi*X)*cos(l*pi*Y)')
  call check0d(sin(k*pi*x0)*cos(l*pi*y0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(sin(k*pi*x1)*cos(l*pi*y1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation cos(k*pi*X)*sin(l*pi*Y)')
  call check0d(cos(k*pi*x0)*sin(l*pi*y0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(cos(k*pi*x1)*sin(l*pi*y1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = sin(k*pi*yx_X)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation sin(k*pi*X)')
  call check0d(sin(k*pi*x0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(sin(k*pi*x1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = cos(k*pi*yx_X) 
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(ee_yx(yx_Data)-ee_Data, eps, &
       'Interpolation cos(k*pi*X)')
  call check0d(cos(k*pi*x0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(cos(k*pi*x1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation cos(l*pi*Y)')
  call check0d(cos(l*pi*y0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(cos(l*pi*y1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation sin(l*pi*Y)')
  call check0d(sin(l*pi*y0)-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(sin(l*pi*y1)-Interpolate_ee(ee_Data, x1, y1),eps)

  yx_Data = 1.0D0
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call check2d(yx_Data1-yx_Data, eps, &
       'Interpolation 1.0')
  call check0d(1.0D0-Interpolate_ee(ee_Data, x0, y0),eps)
  call check0d(1.0D0-Interpolate_ee(ee_Data, x1, y1),eps)

  call MessageNotify('M','ee_test_Interpolate', &
       'ee_module Interpolation function tests succeeded!')

 stop
contains

  subroutine check0d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var                     ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    if (var .gt. eps ) then
       write(6,*) '    Value larger than EPS : ', var
       call MessageNotify('E','ee_test_Interpolate', &
            'Interpolation error too large')
    endif
  end subroutine check0d

  subroutine check2d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:)                ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i, j

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var,1)
       do j=1,size(var,2)
          if (abs(var(i,j)) .gt. eps ) then
             write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, var(i,j)
             call MessageNotify('E','ee_test_transform', &
                  'transform error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program ee_test_transform

