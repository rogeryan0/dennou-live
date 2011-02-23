!----------------------------------------------------------------------
! Copyright (c) 2002-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  aq_module �ƥ��ȥץ����
!
!����  2008/04/02  �ݹ�����
!      2009/07/24  ��������ʿ dc_test ����Ѥ���褦�˽���
!
program aq_intavr

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, km=31       ! �ʻ�����, �����ȿ�
  real(8), parameter :: ra=2               ! �ΰ���礭��

  integer, parameter :: lm=2               ! �����ȿ�������

 !---- �ѿ� ----
  real(8)            :: g_Data(im)     ! �ʻҥǡ���
  real(8)            :: ag_Data(lm,im)    ! �ʻҥǡ���

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: alpha=0.5D0            ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8), parameter :: beta= 1.0D0            ! Ÿ��¿�༰�ѥ�᥿��  0 < ��
  real(8), parameter :: gamma=2.0D0*alpha+beta ! Ÿ��¿�༰�ѥ�᥿�� 

  ! Ƚ�������
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  real(8) :: gammaln
  external gammaln

  real(8) :: Intsol, Avrsol
  real(8) :: a_Intsol(lm), a_Avrsol(lm)

 !---------------- ��ɸ�ͤ����� ---------------------

  call MessageNotify('M', 'aq_test_intavr', &
       'aq_module Integral/average function test')

  call MessageNotify('M','test of Int_g, Avr_g', &
       'test of Int_g, Avr_g ')

  call aq_Initial(im,km,ra,alpha,beta,(/0/))

  !  W(R) = R^beta/(a^2-R^2)^(1-alpha)
  !
  !  \int_0^a R^n W(R) dR 
  !       = a^{n+gamma-1} \int_0^1 r^(n+beta) (1-r^2)^(alpha-1) dr
  !       = (1/2) a^{n+gamma-1}��((n+beta+1)/2)��(alpha)/��(alpha+(n+beta+1)/2)
  !
  !  \int_0^1 r^n w(r) dr = \int_0^1 r^(n+beta) (1-r^2)^(alpha-1) dr
  !                   = (1/2)\int_0^1 t^{(n+beta-1)/2} (1-t)^(alpha-1) dt
  !                   = (1/2) B((n+beta+1)/2,alpha)
  !                   = (1/2) ��((n+beta+1)/2)��(alpha)/��(alpha+(n+beta+1)/2)
  !

  g_Data = 1.0D0
  Intsol = ra**(gamma-1)/2 &
             * exp(gammaln(alpha))&
             *exp(gammaln((beta+1)/2))/exp(gammaln(alpha+(beta+1)/2))
  Avrsol = 1.0D0

  call AssertEqual(&
    message='Integral function test(1)',                        &
    answer = Intsol,                                            &
    check  = Int_g(g_Data),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  
  call AssertEqual(&
    message='averagean function test(1)',                       &
    answer = Avrsol,                                            &
    check  = Avr_g(g_Data),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  g_Data = g_R**2.0D0
  Intsol = ra**(2+gamma-1)*0.5D0 &
       * exp(gammaln(alpha)) &
       * exp(gammaln((2+beta+1)*0.5D0))/exp(gammaln(alpha+(2+Beta+1)*0.5d0))
  Avrsol = Intsol/(ra**(gamma-1)*0.5D0 &
                   * exp(gammaln(alpha))&
                   * exp(gammaln((beta+1)*0.5D0))/exp(gammaln(alpha+(Beta+1)*0.5D0)))

  call AssertEqual(&
    message='Integral function test(2)',                        &
    answer = Intsol,                                            &
    check  = Int_g(g_Data),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  
  call AssertEqual(&
    message='averagean function test(2)',                       &
    answer = Avrsol,                                            &
    check  = Avr_g(g_Data),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','Test of Int_g, Avr_g', &
       'test of Int_g, Avr_g succeeded')

  call MessageNotify('M', 'aq_test_intavr', &
       'test of Int_ag, Avr_ag')

  call aq_Initial(im,km,ra,alpha,beta,(/2,1/))

  ag_Data(1,:) = g_R**4.0D0
  ag_Data(2,:) = g_R**5.0D0

  a_IntSol(1) =  ra**(4+gamma-1)/2 &
                  * exp(gammaln(alpha)) & 
                  * exp(gammaln((4+beta+1)/2))/exp(gammaln(alpha+(4+Beta+1)/2))
  a_IntSol(2) =  ra**(5+gamma-1)/2 &
                  * exp(gammaln(alpha)) &
                  * exp(gammaln((5+beta+1)/2))/exp(gammaln(alpha+(5+Beta+1)/2))
  a_AvrSol = a_IntSol/(ra**(gamma-1)/2 &
                  * exp(gammaln(alpha)) &
                  * exp(gammaln((beta+1)/2))/exp(gammaln(alpha+(Beta+1)/2)))

  call AssertEqual(&
    message='Integral function test(3)',                        &
    answer = a_Intsol,                                          &
    check  = a_Int_ag(ag_Data),                                 &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  
  call AssertEqual(&
    message='averagean function test(3)',                       &
    answer = a_Avrsol,                                          &
    check  = a_Avr_ag(ag_Data),                                 &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M', 'aq_test_intavr', &
       'test of Int_ag, Avr_ag succeeded')

  call MessageNotify('M', 'aq_test_intavr', &
       'aq_module Integral/average function test succeeded')


end program aq_intavr
