!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eq_module テストプログラム
!      境界値問題
!
!履歴  2008/04/12  竹広真一  
!      
!
program eq_test_boundary

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=16              ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=21              ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter :: ra=2.0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  real(8)            :: rp_A(jm,0:im-1)          ! 格子データ
  real(8)            :: rp_dAdr(jm,0:im-1)       ! 格子データ
  real(8)            :: eq_A(-km:km,0:lm)        ! スペクトルデータ

  real(8), dimension(-km:km)  :: e_value
  real(8), dimension(0:im-1)  :: p_value

  integer            :: k=2

  call MessageNotify('M','eq_test_boudaries', &
       'eq_module subroutine for boundary value problem tests')

 !---------------- 座標値の設定 ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! スペクトル初期化

!----- eq_Boundary (D) ------
  rp_A = sin(k*rp_Phi)*sin(pi*rp_Rad/ra)
  eq_A = eq_rp(rp_a)
  e_value = 0.0 ; e_value(k) = 1.0
  p_value = p_e(e_value)

  call eq_Boundary(eq_A,value=e_value,cond='D')
  rp_A = rp_eq(eq_A)

  if ( maxval(abs(rp_A(jm,:)-p_value)) > eps )then
     call MessageNotify('E','Test of eq_Boundaries', &
          'B.C. adoption error too large.')
  endif

  call MessageNotify('M','Test of eq_Boundary','Test of D b.c. suceeded!')
 

!----- eq_Boundary (N) ------
  rp_A = sin(k*rp_Phi)*sin(pi*rp_Rad/ra)
  eq_A = eq_rp(rp_a)
  p_value = p_e(e_value)

  call eq_Boundary(eq_A,e_value,cond='N')
  rp_A = rp_eq(eq_A)
  rp_dAdr = rp_eq(eq_RadDRad_eq(eq_A))/rp_Rad

  if ( maxval(abs(rp_dAdr(jm,:)-p_value)) > eps )then
     call MessageNotify('E','Test of eq_Boundary', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of eq_Boundary','Test of N b.c. suceeded!')

  call MessageNotify('M','eq_test_boudary', &
       'eq_module subroutine for boundary value problem tests succeeded!')

end program eq_test_boundary
