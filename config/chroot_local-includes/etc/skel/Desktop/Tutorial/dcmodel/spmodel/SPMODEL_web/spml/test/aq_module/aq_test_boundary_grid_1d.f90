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
program aq_test_boundary_grid_1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, km=31       ! �ʻ�����, �����ȿ�
  real(8), parameter :: ra=2               ! �ΰ���礭��

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: alpha=0.5D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8), parameter :: beta= 1.0D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < ��
!  real(8), parameter :: gamma=2*alpha+beta ! Ÿ��¿�༰�ѥ�᥿�� 
  real(8), parameter :: amin=2.0D0

  real(8), dimension(im)   :: g_a, g_a_orig
  real(8), dimension(im)   :: g_dadx
!  real(8), dimension(0:km) :: q_a
  real(8)                  :: value=amin

  ! Ƚ�������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call aq_Initial(im,km,ra,alpha,beta,(/2/))
  call MessageNotify('M', 'aq_test_boundary_grid_1d', &
       'aq_module boundary test(1d)')
  
  g_a = (g_R-ra)**2.0D0 + 1.0D0
!  q_a = q_g(g_a)
  g_a_orig = g_a

!----- ag_BoundaryGrid_D ------

  call ag_BoundaryGrid_D(g_a, value)
  call AssertEqual(&
    message='ag_BoundaryGrid_D, 1st. ',                         &
    answer = amin,                                              &
    check  = g_a(im),                                           &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  call AssertEqual(&
    message='ag_BoundaryGrid_D, 2nd.',                          &
    answer = g_a_orig(1:im-1),                                  &
    check  = g_a(1:im-1),                                       &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

!----- ag_BoundaryGrid_N ------
  call ag_BoundaryGrid_N(g_a,value)
!   call AssertEqual(&
!     message='ag_BoundaryGrid_N, 1st.',                          &
!     answer = g_a_orig,                                          &
!     check  = g_a,                                               &
!     significant_digits = check_digits, ignore_digits = ignore   &
!     )
  g_dadx = g_q(q_rDr_q(q_g(g_a)))/g_R
  call AssertEqual(&
    message='ag_BoundaryGrid_N, 2nd.',                          &
    answer = amin,                                              &
    check  = g_dadx(im),                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  call MessageNotify('M', 'aq_test_boundary_grid_1d', &
       'aq_module boundary test(1d) succeded.')

end program aq_test_boundary_grid_1d
