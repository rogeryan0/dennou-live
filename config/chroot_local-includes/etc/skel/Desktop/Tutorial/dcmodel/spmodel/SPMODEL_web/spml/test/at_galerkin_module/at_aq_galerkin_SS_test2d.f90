!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_aq_galerkin_SS_test2d
!
!      チェビシェフ−ガラーキン法テストプログラム(2次元)
!      非圧縮流体の流線関数・流れポテンシャル用, 両端自由すべり条件
!      (両端で値と 2 階微分が 0)
!
!        f(:,i=0)=f(:,i=im)=f''(:,i=0)=f''(:,i=im)=0 
!        [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      例  : f(x) = sin(j*pi*x),    0<x<1
!
!履歴  2006/01/05  竹広真一  新規作成
!      2006/01/24  竹広真一  モジュール変更に伴う改訂
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_aq_galerkin_SS_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_aq_galerkin_RRFF
  use at_module

  implicit none

  integer, parameter :: jm=3                   ! 1 次元目格子点数
  integer, parameter :: im=32                  ! 格子点数              
  integer, parameter :: km=32                  ! チェビシェフ切断波数  
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0 ! 計算領域

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)
  real(8) :: pi
  integer :: j

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = - 11

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','at_aq_galerkin_SS_test2d', &
    & '2D Slip-Slip B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_aq_galerkin_RRFF_Initial(im,km,'SS')    !--- 両端自由すべり条件 ---

  do j=1,jm
     ag_data(j,:) = sin(j*pi*g_X)
  enddo
  ag_data_orig = ag_data
  ag_data = ag_aq(aq_ag(ag_data))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    & significant_digits = check_digits, ignore_digits = ignore &
    )
!  だめな例
!  ag_data = ag_aq(aq_Dx_aq(aq_Dx_aq(aq_data)))
!  よい例 : 微分値はチェビシェフ係数で保持するべし
  ag_data = ag_aq(aq_at(at_Dx_at(at_Dx_at(at_aq(aq_ag(ag_data))))))

  do j=1,jm
    ag_data_orig(j,:) = -(j*pi)**2 * ag_data_orig(j,:)
  enddo

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> (Dx)^2 -> Grid ',          &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    & significant_digits = check_digits, ignore_digits = ignore &
    )

  call MessageNotify('M','at_aq_galerkin_SS_test2d', &
    & '2D Slip-Slip B.C. succeeded!')
  
end program at_aq_galerkin_SS_test2d
