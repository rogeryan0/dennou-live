!----------------------------------------------------------------------
!     Copyright (c) 2005-2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module テストプログラム (ヤコビアン計算)
!
!履歴  2005/07/19  竹広真一
!      2007/11/13  竹広真一  エラーメッセージ追加
!
program et_test_jacobian

  use dc_message, only : MessageNotify
  use et_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=21              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_A(0:jm,0:im-1)     ! 格子データ
  real(8)            :: yx_B(0:jm,0:im-1)     ! 格子データ
  real(8)            :: yx_Jacob(0:jm,0:im-1)  ! 格子データ

  integer            :: k=3,l=2

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','et_test_jacobian', &
       'ee_module jacobian function tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of ee_module : Jacobian function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  yx_A = sin(k*pi*yx_Y)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = 0.0

  call check2d(yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(sin(k*pi*X),sin(l*pi*Y))')

  yx_A = sin(k*pi*yx_X)*yx_Y
  yx_B = cos(k*pi*yx_X)*yx_Y**2
  yx_Jacob = k*pi*(1+cos(k*pi*yx_X)**2) *yx_Y**2

  call check2d(yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(sin(k*pi*X)*yx_Y,cos(k*pi*yx_X)*Y**2')

  yx_A = cos(k*pi*yx_X)*yx_Y
  yx_B = yx_Y**3
  yx_Jacob = -3*k*pi* sin(k*pi*yx_X)*yx_Y**3

  call check2d(yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(cos(k*pi*X)*Y,Y**3))')

  yx_A = yx_Y**4
  yx_B = sin(l*pi*yx_X)
  yx_Jacob = -4*l*pi * yx_Y**3*cos(l*pi*yx_X)

  call check2d(yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(Y**4,sin(l*pi*X))')

  call MessageNotify('M','et_test_jacobian', &
       'et_module jacobian function tests succeeded!')

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
             call MessageNotify('E','et_test_jacobian', &
                  'jacobian error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program et_test_jacobian
