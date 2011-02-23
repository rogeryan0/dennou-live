!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_aq_galerkin_RR_test1d
!
!      チェビシェフ−ガラーキン法テストプログラム(2次元)
!      非圧縮流体の流線関数・流れポテンシャル用, 両端粘着条件
!      (両端で値と 1 階微分が 0)
!
!        f(:,i=0)=f(:,i=im)=f'(:,i=0)=f'(:,i=im)=0 
!        [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      例 : f(x) = cosh(alpha)*cos(lambda*t) - cos(lambda)*cosh(alpha*t)
!              t=(xmax+xmin)/2 + (xmax-xmin)/2 * t
!              lambda * tan(lambda) = -alpha * tanh(alpha)
!
!履歴  2006/01/05  竹広真一  新規作成
!      2006/01/24  竹広真一  モジュール変更に伴う改訂
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_aq_galerkin_RR_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_aq_galerkin_RRFF
  use at_module

  implicit none

  integer, parameter :: jm=5                   ! 1 次元目格子点数
  integer, parameter :: im=32                  ! 格子点数              
  integer, parameter :: km=32                  ! チェビシェフ切断波数  
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0 ! 計算領域

  real(8), parameter :: nu=2.0D0            ! 境界条件係数

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)
  real(8) :: g_Tau(0:im)

  real(8) :: lambda(jm)                          ! 解析解係数
  integer :: j
  ! 判定誤差設定
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = - 8


  lambda = InvXtanX(-nu*tanh(nu),jm)

  call MessageNotify('M','at_ag_galerkin_DD_test2d', &
    & '2D both no-slip B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_aq_galerkin_RRFF_Initial(im,km,'RR')   ! --- 両端粘着条件 ---

  g_Tau = 2.0D0/(xmax-xmin)*(g_X-(xmax+xmin)/2.0D0)
  do j=1,jm
     ag_data(j,:) = cosh(nu)*cos(lambda(j)*g_Tau) &
                   - cos(lambda(j))*cosh(nu*g_Tau)
  end do
  ag_data_orig = ag_data
  ag_data= ag_aq(aq_ag(ag_data))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    & significant_digits = check_digits, ignore_digits = ignore &
    )

  call MessageNotify('M','at_ag_galerkin_DD_test2d', &
    & '2D both no-slip B.C. succeeded' )

contains
  !
  ! x*tan(x)=val の解を求める
  !
  function InvXtanX(val,n)
    real(8), intent(IN) :: val                ! x*tan(x)=val < 0
    integer, intent(IN) :: n                  ! 求める解の個数
    real(8)             :: InvXtanX(n)
    real(8), parameter  :: eps = 5.0D-15     ! 解の精度

    real(8) :: pi
    integer :: i
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    pi = atan(1.0D0)*4.0D0

    do i=1,n
       xs=-pi/2.0D0  + i*pi + eps
       xl=i*pi

       ValS = xs*tan(xs)-val ; ValL = xl*tan(xl)-val
       if ( ValS * ValL .GT. 0.0D0 ) &
         call MessageNotify('E','InvXtanX',&
         'Initial values of ValS and ValL are the same sign.')
1000   xm = (xs + xl)/2.0d0
       ValM = xm*tan(xm) - val
       
       if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=xs*tan(xs)-val
       else
         xl = xm ; ValL=xl*tan(xl)-val
       endif
       
       if ( abs(xl-xs) .lt. eps ) then
         InvXtanX(i) = xm
         goto 99
       endif
       
       goto 1000
       
99   end do
   end function InvXtanX
   
end program at_aq_galerkin_RR_test2d
