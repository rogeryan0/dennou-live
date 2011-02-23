!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_aq_galerkin_RR_test1d
!
!      チェビシェフ−ガラーキン法テストプログラム(1次元)
!      非圧縮流体の流線関数・流れポテンシャル用, 両端粘着条件
!      (両端で値と 1 階微分が 0)
!
!        f(:,i=0)=f(:,i=im)=f'(:,i=0)=f'(:,i=im)=0 
!        [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      例 : f(x) = (x-xmin)**2 * (xmax-x) **2
!
!履歴  2006/01/05  竹広真一  新規作成
!      2006/01/18  竹広真一  更新
!      2006/01/24  竹広真一  モジュール変更に伴う改訂
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_aq_galerkin_RR_test1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_aq_galerkin_RRFF
  use at_module

  implicit none

  integer, parameter :: im=8, km=8
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0

  real(8) :: g_data(0:im)
  real(8) :: g_data_orig(0:im)
  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = - 11

  call MessageNotify('M','at_ag_galerkin_RR_test1d', &
    & '1D both no-slip B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_aq_galerkin_RRFF_Initial(im,km,'RR')     !--- 両端粘着条件 ---

  g_data = (g_X-xmin)**2*(xmax-g_X)**2
  g_data_orig = g_data
  g_data = g_q(q_t(t_g(g_data)))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                      &
    & answer  = g_data_orig,                                      &
    & check   = g_data,                                           &
    & significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','at_ag_galerkin_RR_test1d', &
    & '1D both no-slip B.C. succeeded')

end program at_aq_galerkin_RR_test1d

