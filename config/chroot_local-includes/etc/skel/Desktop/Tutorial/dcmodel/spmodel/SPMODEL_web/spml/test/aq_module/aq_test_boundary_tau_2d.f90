!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  aq_module �ƥ��ȥץ����
!      1 ��������������
!
!����  2008/04/02  �ݹ�����
!      2009/07/24  ��������ʿ dc_test ����Ѥ���褦�˽���
!
program aq_test_boundary_tau_2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=4, km=7       ! �ʻ�����, �����ȿ�
  real(8), parameter :: ra=2               ! �ΰ���礭��

  integer, parameter :: lm=2               ! �����ȿ�������

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: alpha=0.5D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8), parameter :: beta= 1.0D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < ��
!  real(8), parameter :: gamma=2*alpha+beta ! Ÿ��¿�༰�ѥ�᥿�� 

 !---- �ѿ� ----
  real(8), dimension(lm,im) :: ag_a
  real(8), dimension(lm,im) :: ag_dadx
  real(8), dimension(lm,0:km) :: aq_a
  real(8), dimension(lm)      :: value

  ! Ƚ�������
  integer, parameter :: check_digits = 15
  integer, parameter :: ignore = -16

  value(:) = (/2.0D0,1.0D0/)

  call aq_Initial(im,km,ra,alpha,beta,(/1,2/))

  call MessageNotify('M', 'aq_test_boundary_tau_2d', &
       'aq_module boundary test(2d)')

!----- aq_BoundaryTau_D ------

  ag_a(1,:) = sin(g_R)
  ag_a(2,:) = cos(g_R)
!!$  ag_a(1,:) = g_R
!!$  ag_a(2,:) = g_R**2
  aq_a = aq_ag(ag_a)
  call aq_BoundaryTau_D(aq_a,value)
  ag_a = ag_aq(aq_a)
  call AssertEqual(&
    message='ag_BoundaryTau_D',                                 &
    answer = value,                                             &
    check  = ag_a(:,im),                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

!----- aq_BoundaryTau_N ------

  ag_a(1,:) = sin(g_R)
  ag_a(2,:) = cos(g_R)
  aq_a = aq_ag(ag_a)
  call aq_BoundaryTau_N(aq_a,value)
  ag_dadx = ag_aq(aq_rDr_aq(aq_a))/spread(g_R,1,lm)

  call AssertEqual(&
    message='ag_BoundaryTau_N',                                 &
    answer = value,                                             &
    check  = ag_dadx(:,im),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M', 'aq_test_boundary_tau_2d', &
       'aq_module boundary test(2d) succeeded')

end program aq_test_boundary_tau_2d
