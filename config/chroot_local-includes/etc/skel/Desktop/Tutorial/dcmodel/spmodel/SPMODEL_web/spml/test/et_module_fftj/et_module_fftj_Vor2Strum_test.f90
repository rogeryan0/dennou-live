!----------------------------------------------------------------------
!   Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module_fftj テストプログラム (渦度から流線を求める関数)
!
!履歴  2009/09/11  竹広真一  et_test_Vor2Strum.f90 を fftj 用に改造
!
program et_module_fftj_Vor2Strm_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use et_module_fftj
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
  real(8)            :: x_Zero(0:im-1) = 0.0D0  ! 格子データ

  integer            :: k=2

  ! 判定誤差設定
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  call MessageNotify('M','et_module_fftj_Vor2Strum_test', &
       'et_module_fftj Vor2Strum functions tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

!----- et_Vor2Strm (FF) ------
  yx_Vor = sin(k*pi*yx_X)*sin(pi*yx_Y)
  yx_Psi_sol = -sin(k*pi*yx_X)*sin(pi*yx_Y)/((k*pi)**2+pi**2)

  call AssertEqual(&
    message='Vor2Strum (FF)',                                     &
    answer =yx_Psi_sol,                                           &
    check = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor)),cond='FF'))),&
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!----- et_Vor2Strm (RF) ------
  yx_Psi_sol = sin(k*pi*yx_X)*(yx_Y-ymin)**3*(yx_Y-ymax)**2
  yx_Vor_sol = yx_et(et_Lapla_et(et_yx(yx_Psi_sol)))

  yx_Psi = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor_sol)), &
                                      cond='RF',new=.true.)))
  yx_Vor = yx_et(et_Lapla_et(et_yx(yx_Psi)))
  yx_dPsidy = yx_et(et_Dy_et(et_yx(yx_Psi)))
  yx_d2Psidy2 = yx_et(et_Dy_et(et_Dy_et(et_yx(yx_Psi))))

  call AssertEqual(&
    message='Vor2Strum (RF, vorticity)',                          &
    answer =yx_Vor_sol(2:jm-2,:),                                 &
    check = yx_Vor(2:jm-2,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Vor2Strum (RF, Top)',                                &
    answer =x_Zero,                                               &
    check = yx_dPsidy(0,:),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Vor2Strum (RF, Bottom)',                             &
    answer =x_Zero,                                               &
    check = yx_d2Psidy2(jm,:),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!----- et_Vor2Strm (FR) ------
  yx_Psi_sol = sin(k*pi*yx_X)*(yx_Y-ymin)**2*(yx_Y-ymax)**3
  yx_Vor_sol = yx_et(et_Lapla_et(et_yx(yx_Psi_sol)))
  yx_Psi = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor_sol)), &
                                      cond='FR',new=.true.)))
  yx_Vor = yx_et(et_Lapla_et(et_yx(yx_Psi)))
  yx_dPsidy = yx_et(et_Dy_et(et_yx(yx_Psi)))
  yx_d2Psidy2 = yx_et(et_Dy_et(et_Dy_et(et_yx(yx_Psi))))

  call AssertEqual(&
    message='Vor2Strum (FR, vorticity)',                          &
    answer =yx_Vor_sol(2:jm-2,:),                                 &
    check = yx_Vor(2:jm-2,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Vor2Strum (FR, Top)',                                &
    answer =x_Zero,                                               &
    check = yx_d2Psidy2(0,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Vor2Strum (FR, Bottom)',                             &
    answer =x_Zero,                                               &
    check = yx_dPsidy(jm,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!----- et_Vor2Strm (RR) ------
  yx_Psi_sol = sin(k*pi*yx_X)*(yx_Y-ymin)**2*(yx_Y-ymax)**2
  yx_Vor_sol = yx_et(et_Lapla_et(et_yx(yx_Psi_sol)))
  yx_Psi = yx_et(at_ay(ey_Vor2Strm_ey(ay_at(et_yx(yx_Vor_sol)), &
                                      cond='RR',new=.true.)))
  yx_Vor = yx_et(et_Lapla_et(et_yx(yx_Psi)))
  yx_dPsidy = yx_et(et_Dy_et(et_yx(yx_Psi)))
  yx_d2Psidy2 = yx_et(et_Dy_et(et_Dy_et(et_yx(yx_Psi))))

  call AssertEqual(&
    message='Vor2Strum (RR, vorticity)',                          &
    answer =yx_Vor_sol(2:jm-2,:),                                 &
    check = yx_Vor(2:jm-2,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Vor2Strum (RR, Top)',                                &
    answer =x_Zero,                                               &
    check = yx_dPsidy(0,:),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Vor2Strum (RR, Bottom)',                             &
    answer =x_Zero,                                               &
    check = yx_dPsidy(jm,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','et_module_fftj_Vor2Strum_test', &
       'et_module_fftj Vor2Strum functions tests succeeded!')

end program et_module_fftj_Vor2Strm_test
