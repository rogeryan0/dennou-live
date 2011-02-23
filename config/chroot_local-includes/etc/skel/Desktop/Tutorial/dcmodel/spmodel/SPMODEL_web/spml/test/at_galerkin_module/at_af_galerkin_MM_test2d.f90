!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_af_galerkin_MM_test1d
!
!      チェビシェフ−ガラーキン法
!      ディリクレ・ノイマン混合境界条件用モジュールテストプログラム(2次元)
!          cfdx1_xmax f'(:,i=0)  + cfdx0_xmax f(:,i=0)  = 0,
!          cfdx1_xmin f'(:,i=im) + cfdx0_xmin f(:,i=im) = 0,
!
!      例 : cfdx1_xmax=1, cfdx0_xmax =alpha, cfdx1_xmin=1, cfdx0_xmin =-alpha
!
!           f(x) = cos(lambda*t)
!              t=(xmax+xmin)/2 + (xmax-xmin)/2 * t
!              lambda tan(lambda) = alpha*(xmax-xmin)/2
!
!履歴  2006/01/04  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更に伴う改訂
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_af_galerkin_MM_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_af_galerkin_MM, only: at_af_galerkin_MM_Initial, &
                               af_at, at_af, af_ag, ag_af
  use at_module
  use dc_message

  implicit none

  integer, parameter :: jm=10                  ! 1 次元目格子点数
  integer, parameter :: im=64                  ! 格子点数              
  integer, parameter :: km=64                  ! チェビシェフ切断波数  
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0 ! 計算領域

  real(8), parameter :: alpha=2.0D0            ! 境界条件係数

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)

  real(8) :: lambda(jm)                          ! 解析解係数
  integer :: j
  ! 判定誤差設定
  integer, parameter :: check_digits = 5
  integer, parameter :: ignore = - 6

  lambda = InvXtanX(alpha*(xmax-xmin)/2.0D0,jm)

  call MessageNotify('M','at_af_galerkin_MM_test2d', &
    & '2D Mixed B.C.')

  call at_Initial(im,km,xmin,xmax)
  !--- ディリクレ・ノイマン混合境界条件 ---
  call at_af_galerkin_MM_Initial(im,km,        &
       cfdx0_xmax=alpha, cfdx1_xmax=1.0D0, &
       cfdx0_xmin=-alpha,  cfdx1_xmin=1.0D0    )

  do j=1,jm
     ag_data(j,:) = cos(lambda(j)*(2.0D0/(xmax-xmin)*(g_X-(xmax+xmin)/2.0D0)))
  enddo
  ag_data_orig = ag_data
  ag_data = ag_af(af_ag(ag_data))

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> Grid ')

!  だめな例
!  ag_data = ag_af(af_Dx_af(af_Dx_af(af_data)))

!  よい例 : 微分値はチェビシェフ係数で保持するべし
  ag_data = ag_af(af_at(at_Dx_at(at_Dx_at(at_af(af_ag(ag_data))))))

  do j=1,jm
    ag_data_orig(j,:) = -(lambda(j)/(xmax-xmin)*2)**2 * ag_data_orig(j,:) 
  end do

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin ->(Dx)^2 ->  Grid ')

  call MessageNotify('M','at_af_galerkin_MM_test2d', &
    & '2D Mixed B.C. succeeded')


contains
  subroutine check2d(var, true, funcname)
    real(8) :: var(:,:)
    real(8) :: true(:,:)
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check2d

  !
  ! x*tan(x)=val の解を求める
  !
  function InvXtanX(val,n)
    real(8), intent(IN) :: val                ! x*tan(x)=val > 0
    integer, intent(IN) :: n                  ! 求める解の個数
    real(8)             :: InvXtanX(n)
    real(8), parameter  :: eps = 1.0D-14     ! 解の精度

    real(8) :: pi = 3.1415926535897932385D0
    integer :: i
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    do i=1,n
       xs=(i-1)*pi
       xl=pi/2.0D0  + (i-1.0d0)*pi - eps

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

99  end do
  end function InvXtanX

end program at_af_galerkin_MM_test2d
