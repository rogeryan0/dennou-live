!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_aq_galerkin_SR_test1d
!
!      �����ӥ����աݥ��顼����ˡ�ƥ��ȥץ����(1����)
!      �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
!      ��ü��ͳ���٤���, ��üǴ����
!      (ξü���ͤ� 0, ��¦�� 2 ����ʬ, �⤦������ 1 ����ʬ�� 0)
!
!        f(:,i=0)=f(:,i=im)=f''(:,i=0)=f'(:,i=im)=0 
!        [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      �� : f(x) = (x-xmin)**2 * (xmax-x) **3
!
!����  2006/01/06  �ݹ�����  ��������
!      2006/01/24  �ݹ�����  �⥸�塼���ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_aq_galerkin_SR_test1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_aq_galerkin_RRFF
  use at_module

  implicit none

  integer, parameter :: im=8, km=8
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0

  real(8) :: g_data(0:im)
  real(8) :: g_data_orig(0:im)
  ! Ƚ�������
  integer, parameter :: check_digits = 15
  integer, parameter :: ignore = - 16

  call MessageNotify('M','at_aq_galerkin_SR_test1d', &
    & '1D Slip-Rigid B.C. ')

  call at_Initial(im,km,xmin,xmax)
  call at_aq_galerkin_RRFF_Initial(im,km,'SR')  

  g_data = (g_X-xmin)**2*(xmax-g_X)**3
  g_data_orig = g_data
  g_data= g_q(q_t(t_g(g_data)))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = g_data_orig,                                    &
    & check   = g_data,                                         &
    & significant_digits = check_digits, ignore_digits = ignore &
    )

  call MessageNotify('M','at_aq_galerkin_SR_test1d', &
    & '1D Slip-Rigid B.C. succeeded')

end program at_aq_galerkin_SR_test1d
