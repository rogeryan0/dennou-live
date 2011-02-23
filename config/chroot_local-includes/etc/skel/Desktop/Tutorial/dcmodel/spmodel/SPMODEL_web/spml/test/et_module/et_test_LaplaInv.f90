!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module テストプログラム
!      Laplacian 逆解き問題
!
!履歴  2007/11/19  竹広真一  エラーメッセージ追加
!      
!
program et_test_LaplaInv

  use dc_message, only : MessageNotify
  use et_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=21              ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  real(8)            :: yx_A(0:jm,0:im-1)     ! 格子データ
  real(8)            :: yx_A_sol(0:jm,0:im-1)  ! 格子データ
  real(8)            :: et_A(-km:km,0:lm)     ! スペクトルデータ

  real(8), dimension(-km:km,2)  :: values

  integer            :: k=2

  call MessageNotify('M','et_test_boudaries', &
       'et_module subroutine for boundary value problem tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

  yx_A = sin(k*pi*yx_X)*sin(pi*yx_Y)
  yx_A_sol = -sin(k*pi*yx_X)*sin(pi*yx_Y)/((k*pi)**2 + pi**2)
  et_A = et_yx(yx_a)
  values = 0.0
  yx_A = yx_et(et_LaplaInv_et(et_A,values))

  if ( maxval(abs(yx_A-yx_A_sol)) > eps ) then
     call MessageNotify('E','Test of et_LaplaInv', &
          'Calculation of Inverse Laplacian error too large.')
  endif

  call MessageNotify('M','Test of et_LaplaInv','Test of et_LaplaInv suceeded!')

end program et_test_LaplaInv
