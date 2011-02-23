!----------------------------------------------------------------------
! Copyright (c) 2007-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  aq_module テストプログラム
!
!履歴  2007/12/28  竹広真一  
!      2008/12/04  佐々木洋平 DGamma の廃止に伴い exp(gammaln) へ変更
!      2009/07/24  佐々木洋平 dc_test を使用するように修正
!      
!
program aq_test_weight

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

  integer, parameter :: im=16, km=31           ! 格子点数, 切断波数
  real(8), parameter :: ra=2                   ! 領域の大きさ
  real(8), parameter :: alpha=0.5D0            ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta= 1.0D0            ! 展開多項式パラメター  0 < β
  real(8), parameter :: gamma=2.0D0*alpha+beta ! 展開多項式パラメター 
  real(8) :: gammaln
  external gammaln

  real(8), dimension(im) :: g_z
  real(8) :: Intsol

  ! 判定誤差設定
  integer, parameter :: check_digits = 7
  integer, parameter :: ignore = -8

  call MessageNotify('M', 'aq_test_boundary_weight', &
       'aq_module weight test')

  call aq_Initial(im,km,ra,alpha,beta,(/0/))

  !  W(R) = R^beta/(a^2-R^2)^(1-alpha)
  !
  !  \int_0^a R^n W(R) dR 
  !       = a^{n+gamma-1} \int_0^1 r^(n+beta) (1-r^2)^(alpha-1) dr
  !       = (1/2) a^{n+gamma-1}Γ((n+beta+1)/2)Γ(alpha)/Γ(alpha+(n+beta+1)/2)
  !
  !  \int_0^1 r^n w(r) dr = \int_0^1 r^(n+beta) (1-r^2)^(alpha-1) dr
  !                   = (1/2)\int_0^1 t^{(n+beta-1)/2} (1-t)^(alpha-1) dt
  !                   = (1/2) B((n+beta+1)/2,alpha)
  !                   = (1/2) Γ((n+beta+1)/2)Γ(alpha)/Γ(alpha+(n+beta+1)/2)
  !

  g_z(:) = 1.0D0
  Intsol = ra**(gamma-1) * 0.5D0 * exp(gammaln(alpha)) &
              *exp(gammaln((beta+1)*0.5D0))/exp(gammaln(alpha+(Beta+1)*0.5D0))

  call AssertEqual(&
    message='aq_test_weight(1)',                                &
    answer = Intsol,                                            &
    check  = abs(sum(g_z * g_R_weight)),                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  g_z(:) = g_R**2
  Intsol = ra**(2+gamma-1)/2 &
       * exp(gammaln(alpha)) &
       * exp(gammaln((2+beta+1)/2))/exp(gammaln(alpha+(2+Beta+1)/2))

  call AssertEqual(&
    message='aq_test_weight(2)',                                &
    answer = Intsol,                                            &
    check  = abs(sum(g_z * g_R_weight)),                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call aq_Initial(im,km,ra,alpha,beta,(/1/))

  g_z(:) = g_R**3
  Intsol = ra**(3+gamma-1)/2 &
       * exp(gammaln(alpha)) &
       * exp(gammaln((3+beta+1)/2))/exp(gammaln(alpha+(3+Beta+1)/2))

  call AssertEqual(&
    message='aq_test_weight(3)',                                &
    answer = Intsol,                                            &
    check  = abs(sum(g_z * g_R_weight)),                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M', 'aq_test_boundary_weight', &
       'aq_module weight test succeeded')

end program aq_test_weight



