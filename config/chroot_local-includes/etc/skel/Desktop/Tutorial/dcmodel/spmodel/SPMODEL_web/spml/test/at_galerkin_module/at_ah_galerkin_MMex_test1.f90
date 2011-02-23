!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ah_galerkin_MMex_test1d
!
!      チェビシェフ−ガラーキン法
!      ディリクレ・ノイマン混合境界条件(可変係数)用モジュールテストプログラム
!      
!          cfdx1_xmax(j) f'(j,i=0)  + cfdx0_xmax f(j,i=0)  = 0,
!          cfdx1_xmin(j) f'(j,i=im) + cfdx0_xmin f(j,i=im) = 0,
!
!      例1 : 両端ディリクレ条件
!            cfdx1_xmax=0, cfdx0_xmax = 1, cfdx1_xmin=0, cfdx0_xmin = 1
!            f(x) = (x-xmin) * (xmax-x)  
!
!      例2 : 片端ディリクレ片端ノイマン境界条件
!            cfdx1_xmax=0, cfdx0_xmax = 1, cfdx1_xmin=1, cfdx0_xmin = 0
!            f(x) = (x-xmin)**2 * (xmax-x)
!
!      例3 : 片端ノイマン片端ディリクレ境界条件
!            cfdx1_xmax=1, cfdx0_xmax = 0, cfdx1_xmin=0, cfdx0_xmin = 1
!            f(x) = (x-xmin)* (xmax-x)**2
!
!      例4 : ディリクレ・ノイマン混合境界条件
!            cfdx1_xmax=1, cfdx0_xmax = -1, cfdx1_xmin=1, cfdx0_xmin = 1
!            f(x) = x**3 - 2 x**2, 0<x<1
!
!      例5 : 両端ノイマン混合境界条件
!            cfdx1_xmax=1, cfdx0_xmax = 1, cfdx1_xmin=0, cfdx0_xmin = 0
!            f(x) = (x-xmin)**2 * (xmax-x)**2, 0<x<1
!
!履歴  2006/01/06  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更に伴う改訂
!      2006/02/15  竹広真一  係数行列式 0 の場合に対応
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_ah_galerkin_MMex_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_ah_galerkin_MMex
  use at_module

  implicit none

  integer, parameter :: jm=5                   ! 1 次元目格子点数
  integer, parameter :: im=64                  ! 格子点数              
  integer, parameter :: km=64                  ! チェビシェフ切断波数  
  real(8), parameter :: xmin=0.0, xmax=1.0     ! 計算領域

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)

  real(8) :: cfdx0_xmin(jm)                    ! 境界条件係数(0階微分@x=xmin)
  real(8) :: cfdx1_xmin(jm)                    ! 境界条件係数(1階微分@x=xmin)
  real(8) :: cfdx0_xmax(jm)                    ! 境界条件係数(0階微分@x=xmax)
  real(8) :: cfdx1_xmax(jm)                    ! 境界条件係数(1階微分@x=xmax)
  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = - 12

  call MessageNotify('M','at_ah_galerkin_MMex_test1d', &
    & '1D Mixed B.C.')

  call at_Initial(im,km,xmin,xmax)

  !--- (j=1)両端ディリクレ境界条件 ---
  cfdx0_xmax(1)=1.0D0 ; cfdx1_xmax(1)=0.0D0
  cfdx0_xmin(1)=1.0D0 ; cfdx1_xmin(1)=0.0D0

  ag_data(1,:) = (g_X-xmin)*(xmax-g_X)

  !--- (j=2)片端ディリクレ片端ノイマン境界条件 ---
  cfdx0_xmax(2)=1.0D0 ; cfdx1_xmax(2)=0.0D0
  cfdx0_xmin(2)=0.0D0 ; cfdx1_xmin(2)=1.0D0

  ag_data(2,:) = (g_X-xmin)**2 * (xmax-g_X)

  !--- (j=3)片端ノイマン片端ディリクレ境界条件 ---
  cfdx0_xmax(3)=0.0D0 ; cfdx1_xmax(3)=1.0D0
  cfdx0_xmin(3)=1.0D0 ; cfdx1_xmin(3)=0.0D0

  ag_data(3,:) = (g_X-xmin) * (xmax-g_X)**2

  !--- (j=4)ディリクレ・ノイマン混合境界条件 ---
  cfdx0_xmax(4)=1.0D0 ; cfdx1_xmax(4)=-1.0D0
  cfdx0_xmin(4)=1.0D0 ; cfdx1_xmin(4)=1.0D0 

  ag_data(4,:) = g_X**3 - 2*g_X**2

  !--- (j=5)両端ノイマン境界条件 ---
  cfdx0_xmax(5)=0.0D0 ; cfdx1_xmax(5)=1.0D0
  cfdx0_xmin(5)=0.0D0 ; cfdx1_xmin(5)=1.0D0

  ag_data(5,:) = (g_X-xmin)**2*(xmax-g_X)**2

  !--- モジュール初期化 ---
  call at_ah_galerkin_MMex_Initial(im,km,jm,         &
       cfdx0_xmax=cfdx0_xmax,cfdx1_xmax=cfdx1_xmax,  &
       cfdx0_xmin=cfdx0_xmin,cfdx1_xmin=cfdx1_xmin    )

  ag_data_orig = ag_data
  ag_data = ag_ah(ah_ag(ag_data))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','at_ah_galerkin_MMex_test1d', &
    & '1D Mixed B.C. succeeded')

end program at_ah_galerkin_MMex_test1
