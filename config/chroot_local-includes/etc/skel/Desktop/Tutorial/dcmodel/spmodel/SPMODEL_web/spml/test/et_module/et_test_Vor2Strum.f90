!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module テストプログラム
!      渦度から流線を求める関数
!
!履歴  2007/11/20  竹広真一  
!      
!
program et_test_Vor2Strm

  use dc_message, only : MessageNotify
  use et_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=32             ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-8            ! 判定誤差

  real(8)            :: yx_Vor(0:jm,0:im-1)     ! 格子データ
  real(8)            :: yx_Psi(0:jm,0:im-1)     ! 格子データ
  real(8)            :: yx_Vor_sol(0:jm,0:im-1) ! 格子データ
  real(8)            :: yx_Psi_sol(0:jm,0:im-1) ! 格子データ
  real(8)            :: yx_dPsidy(0:jm,0:im-1)  ! 格子データ
  real(8)            :: yx_d2Psidy2(0:jm,0:im-1)! 格子データ

  integer            :: k=2

  call MessageNotify('M','et_test_Vor2Strm', &
       'et_module Stream function calculation from vorticity tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

!----- et_Vor2Strm (FF) ------
  yx_Vor = sin(k*pi*yx_X)*sin(pi*yx_Y)
  yx_Psi = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor)),cond='FF')))
  yx_Psi_sol = -sin(k*pi*yx_X)*sin(pi*yx_Y)/((k*pi)**2+pi**2)

  if ( maxval(abs(yx_Psi_sol-yx_Psi)) > eps ) then
     call MessageNotify('E','Test of et_Vor2Strm', &
          'Calc of Stream func. error too large.')
  endif

  call MessageNotify('M','Test of et_Vor2Strm','Test of FF b.c. suceeded!')

!----- et_Vor2Strm (RF) ------
  yx_Psi_sol = sin(k*pi*yx_X)*(yx_Y-ymin)**3*(yx_Y-ymax)**2
  yx_Vor_sol = yx_et(et_Lapla_et(et_yx(yx_Psi_sol)))

  yx_Psi = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor_sol)), &
                                      cond='RF',new=.true.)))
  yx_Vor = yx_et(et_Lapla_et(et_yx(yx_Psi)))
  yx_dPsidy = yx_et(et_Dy_et(et_yx(yx_Psi)))
  yx_d2Psidy2 = yx_et(et_Dy_et(et_Dy_et(et_yx(yx_Psi))))

  if (      maxval(abs(yx_Vor_sol(2:jm-2,:)-yx_Vor(2:jm-2,:))) > eps &
       .OR. maxval(abs(yx_dPsidy(0,:))) > eps    &
       .OR. maxval(abs(yx_d2Psidy2(jm,:))) > eps  ) then
     call MessageNotify('E','Test of et_Vor2Strm', &
          'Calc of Stream func. error too large.')
  endif

  call MessageNotify('M','Test of et_Vor2Strm','Test of RF b.c. suceeded!')

!----- et_Vor2Strm (FR) ------
  yx_Psi_sol = sin(k*pi*yx_X)*(yx_Y-ymin)**2*(yx_Y-ymax)**3
  yx_Vor_sol = yx_et(et_Lapla_et(et_yx(yx_Psi_sol)))
  yx_Psi = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor_sol)), &
                                      cond='FR',new=.true.)))
  yx_Vor = yx_et(et_Lapla_et(et_yx(yx_Psi)))
  yx_dPsidy = yx_et(et_Dy_et(et_yx(yx_Psi)))
  yx_d2Psidy2 = yx_et(et_Dy_et(et_Dy_et(et_yx(yx_Psi))))

  if (      maxval(abs(yx_Vor_sol(2:jm-2,:)-yx_Vor(2:jm-2,:))) > eps &
       .OR. maxval(abs(yx_dPsidy(jm,:))) > eps    &
       .OR. maxval(abs(yx_d2Psidy2(0,:))) > eps  ) then
     call MessageNotify('E','Test of et_Vor2Strm', &
          'Calc of Stream func. error too large.')
  endif

  call MessageNotify('M','Test of et_Vor2Strm','Test of FR b.c. suceeded!')

!----- et_Vor2Strm (RR) ------
  yx_Psi_sol = sin(k*pi*yx_X)*(yx_Y-ymin)**2*(yx_Y-ymax)**2
  yx_Vor_sol = yx_et(et_Lapla_et(et_yx(yx_Psi_sol)))
  yx_Psi = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor_sol)), &
                                      cond='RR',new=.true.)))
  yx_Vor = yx_et(et_Lapla_et(et_yx(yx_Psi)))
  yx_dPsidy = yx_et(et_Dy_et(et_yx(yx_Psi)))
  yx_d2Psidy2 = yx_et(et_Dy_et(et_Dy_et(et_yx(yx_Psi))))

  if (      maxval(abs(yx_Vor_sol(2:jm-2,:)-yx_Vor(2:jm-2,:))) > eps &
       .OR. maxval(abs(yx_dPsidy(0,:))) > eps    &
       .OR. maxval(abs(yx_dPsidy(jm,:))) > eps  ) then
     call MessageNotify('E','Test of et_Vor2Strm', &
          'Calc of Stream func. error too large.')
  endif

  call MessageNotify('M','Test of et_Vor2Strm','Test of RR b.c. suceeded!')

  call MessageNotify('M','et_test_Vor2Strum', &
       'et_module Stream function calculation from vorticity tests')

end program et_test_Vor2Strm
