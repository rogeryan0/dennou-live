!----------------------------------------------------------------------
! Copyright (c) 2002-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  aq_module �ƥ��ȥץ����
!      1 ��������������
!
!����  2008/04/02  �ݹ�����
!      2009/07/22  ��������ʿ dc_test ����Ѥ���褦�˽���
!
program aq_test_boundary_grid_2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=4, km=7         ! �ʻ�����, �����ȿ�
  real(8), parameter :: ra=2               ! �ΰ���礭��

  integer, parameter :: lm=2               ! �����ȿ�������

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: alpha=0.5D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8), parameter :: beta= 1.0D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < ��
!  real(8), parameter :: gamma=2*alpha+beta ! Ÿ��¿�༰�ѥ�᥿�� 

 !---- �ѿ� ----
  real(8), dimension(lm,im) :: ag_a
  real(8), dimension(lm,im) :: ag_dadx
!  real(8), dimension(lm,0:km) :: aq_a
  real(8), dimension(lm)      :: value

  ! Ƚ�������
  integer, parameter :: check_digits = 14
  integer, parameter :: ignore = -15

  value(:) = (/2.0D0,1.0D0/)

  call aq_Initial(im,km,ra,alpha,beta,(/1,2/))

  ag_a(1,:) = sin(g_R)
  ag_a(2,:) = cos(g_R)
!!$  ag_a(1,:) = g_R
!!$  ag_a(2,:) = g_R**2
!  aq_a = aq_ag(ag_a)

!----- ag_BoundaryGrid_D ------
  call MessageNotify('M', 'aq_test_boundary_grid_2d', &
       'aq_module boundary test(2d)')

  call ag_BoundaryGrid_D(ag_a,value)
  call AssertEqual(&
    message='ag_BoundaryGrid_D',                                &
    answer = value,                                             &
    check  = ag_a(:,im),                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

!----- ag_BoundaryGrid_N ------

  ag_a(1,:) = sin(g_R)
  ag_a(2,:) = cos(g_R)
!  aq_a = aq_ag(ag_a)

  call ag_BoundaryGrid_N(ag_a,value)
  ag_dadx = ag_aq(aq_rDr_aq(aq_ag(ag_a)))/spread(g_R,1,lm)

  call AssertEqual(&
    message='ag_BoundaryGrid_N',                                &
    answer = value,                                             &
    check  = ag_dadx(:,im),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M', 'aq_test_boundary_grid_1d', &
       'aq_module boundary test(2d) succeded.')

end program aq_test_boundary_grid_2d
