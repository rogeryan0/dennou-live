!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wq_module_sjpack テストプログラム
!
!      磁場トロイダルポテンシャルの境界値問題
!
!履歴  2009/12/11  竹広真一   wq_test_tormagbc.f90 より SJPACK 用に改造
!
program wq_module_sjpack_tormagbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module_sjpack

  implicit none

  integer,parameter  :: im=32, jm=16, km=8   ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=15         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=0.5D0               ! 球半径

  real(8), dimension(0:im-1,1:jm,km)       :: xyr_TORMAG
  real(8), dimension(0:im-1,1:jm,km)       :: xyr_TORMAG_orig
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wq_TORMAG
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wq_TORMAG_orig
  real(8), dimension((nm+1)*(nm+1),km)     :: wr_TORMAG

  real(8), dimension(0:im-1,1:jm,km)       :: xyr_Zero = 0.0D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  real(8), parameter  :: pi=3.1415926535897932385D0

  call MessageNotify('M','wq_module_sjpack_tormagbc_test', &
       'wq_module_sjpack wq_TormagBoundary subroutine test')

  call wq_initial(im,jm,km,nm,lm,ra)

 !==================== wq_TormagBoundary =========================
  ! P_10
  xyr_TORMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ra)/ra )*xyr_Rad
  ! P_1_1
  !xyr_TORMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/(ro-ri) )
  !xyr_TORMAG = 2*sin(xyr_lat)**2 * cos( pi*(xyr_rad-ri)/(ro-ri) )

  wq_TORMAG = wq_xyr(xyr_TORMAG)
  wq_TORMAG_orig = wq_TORMAG

  call wq_TormagBoundary(wq_TORMAG)
  xyr_TORMAG = xyr_wq(wq_TORMAG)

  call AssertEqual(&
       message='wq_TormagBoundaryGrid (Top B.C.)',                   &
       answer = xyr_Zero(:,:,km),                                    &
       check = xyr_TORMAG(:,:,km),                                   &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wq_TormagBoundary (internal value)',                 &
       answer = wq_Tormag(:,0:lm-2),                                 &
       check = wq_Tormag_orig(:,0:lm-2),                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

 !==================== wq_TormagBoundaryGrid =========================
  ! P_10
  !xyr_TORMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ra)/ra )
  ! P_1_1
  xyr_TORMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ra)/ra )*xyr_Rad
  !xyr_TORMAG = 2*sin(xyr_lat)**2 * cos( pi*(xyr_rad-ra)/ra )

  xyr_TORMAG_orig = xyr_TORMAG
  wr_TORMAG = wr_xyr(xyr_TORMAG)
  call wr_TormagBoundaryGrid(wr_TORMAG)
  xyr_TORMAG = xyr_wr(wr_TORMAG)

  call AssertEqual(&
       message='wq_TormagBoundaryGrid (Top B.C.)',                   &
       answer = xyr_Zero(:,:,km),                                    &
       check = xyr_TORMAG(:,:,km),                                   &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wq_TormagBoundaryGrid (internal value)',             &
       answer = xyr_TORMAG_orig(:,:,1:km-1),                         &
       check = xyr_TORMAG(:,:,1:km-1),                               &
       significant_digits = check_digits, ignore_digits = ignore     &
       )


  call MessageNotify('M','wq_module_sjpack_tormagbc_test', &
       'wq_module_sjpack wq_TormagBoundary subroutine test succeeded!')

end program wq_module_sjpack_tormagbc_test
