!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ab_galerkin_ND_test1d
!
!      �����ӥ����աݥ��顼����ˡ
!      ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭������ѥ⥸�塼��ƥ��ȥץ����(1����)
!          f'(i=0)=f(i=im)=0 [ f'(x=xmax)=f(x=xmin)=0 ]
!
!      �� : f(x) = (x-xmin) * (xmax-x)**2
!
!����  2005/12/30  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_ab_galerkin_ND_test1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_ab_galerkin_ND
  use at_module
  implicit none

  integer, parameter :: im=8, km=4
  real(8), parameter :: xmin=0.0, xmax=1.0
  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8) :: g_data(0:im)
  real(8) :: g_data_orig(0:im)

  call MessageNotify('M','at_ab_galerkin_ND_test1d', &
       & '1D Neumann-Dirichlet B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_ab_galerkin_ND_Initial(im,km)     !--- ξü�ǥ��ꥯ�� ---

  g_data = (g_X-xmin)*(xmax-g_X)**2
  g_data_orig = g_data
  g_data = g_b(b_t(t_g(g_data)))

  call check1d(g_data_orig, g_data, "Grid->Galerkin->Grid ")

  call MessageNotify('M','at_ab_galerkin_ND_test1d', &
       & '1D Neumann-Dirichlet B.C. succeeded')

contains
  subroutine check1d(var, true, funcname)
    real(8) :: var(:)
    real(8) :: true(:)
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check1d

end program at_ab_galerkin_ND_test1d
