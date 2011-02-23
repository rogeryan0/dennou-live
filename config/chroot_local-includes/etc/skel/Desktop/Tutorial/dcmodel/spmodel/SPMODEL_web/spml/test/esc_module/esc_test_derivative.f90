!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  esc_module テストプログラム (微分計算)
!
!履歴  2007/11/12  竹広真一
!
program ese_test_derivative

  use dc_message, only : MessageNotify
  use esc_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=10              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! 格子データ
  real(8)            :: yx_Deriv(0:jm,0:im-1)   ! 格子データ

  integer            :: k=2, l=3

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','ese_test_derivative', &
       'esc_module derivative function tests')

 !---------------- 座標値の設定 ---------------------
  call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  write(6,*)
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  yx_Deriv = k*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_Dx_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(sin(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = l*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_Dy_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(sin(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_Lapla_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(sin(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_LaplaInv_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(sin(k*pi*X)*sin(l*pi*Y))')

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  yx_Deriv = -k*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_Dx_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(cos(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -l*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_Dy_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(cos(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_Lapla_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(cos(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_LaplaInv_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(cos(k*pi*X)*cos(l*pi*Y))')

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  yx_Deriv = k*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_Dx_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(sin(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -l*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_Dy_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(sin(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_Lapla_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(sin(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_LaplaInv_ec(ec_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(sin(k*pi*X)*cos(l*pi*Y))')

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  yx_Deriv = -k*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_Dx_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(cos(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = l*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ec(ec_Dy_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(cos(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_Lapla_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(cos(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_es(es_LaplaInv_es(es_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(cos(k*pi*X)*sin(l*pi*Y))')

  call MessageNotify('M','esc_test_derivative', &
       'esc_module derivative function tests succeeded!')

 stop
contains

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
             call MessageNotify('E','ee_test_derivative', &
                  'derivative error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program ese_test_derivative
