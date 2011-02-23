!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_af_galerkin_MM_test1d
!
!      �����ӥ����աݥ��顼����ˡ
!      �ǥ��ꥯ�졦�Υ��ޥ󺮹維������ѥ⥸�塼��ƥ��ȥץ����(1����)
!          cfdx1_xmax f'(:,i=0)  + cfdx0_xmax f(:,i=0)  = 0,
!          cfdx1_xmin f'(:,i=im) + cfdx0_xmin f(:,i=im) = 0,
!
!      ��1 : ξü�ǥ��ꥯ����
!            cfdx1_xmax=0, cfdx0_xmax = 1, cfdx1_xmin=0, cfdx0_xmin = 1
!            f(x) = (x-xmin) * (xmax-x)  
!
!      ��2 : ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
!            cfdx1_xmax=0, cfdx0_xmax = 1, cfdx1_xmin=1, cfdx0_xmin = 0
!            f(x) = (x-xmin)**2 * (xmax-x)
!
!      ��3 : ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
!            cfdx1_xmax=1, cfdx0_xmax = 0, cfdx1_xmin=0, cfdx0_xmin = 1
!            f(x) = (x-xmin)* (xmax-x)**2
!
!      ��4 : �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
!            cfdx1_xmax=1, cfdx0_xmax = -1, cfdx1_xmin=1, cfdx0_xmin = 1
!            f(x) = x**3 - 2 x**2, 0<x<1
!
!      ��5 : ξü�Υ��ޥ󺮹維�����
!            cfdx1_xmax=1, cfdx0_xmax = 1, cfdx1_xmin=0, cfdx0_xmin = 0
!            f(x) = (x-xmin)**2 * (xmax-x)**2, 0<x<1
!
!����  2006/01/03  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2006/02/15  �ݹ�����  �������� 0 �ξ����б�
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_af_galerkin_MM_test1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_af_galerkin_MM
  use at_module

  implicit none

  integer, parameter :: im=8, km=4
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0

  real(8) :: g_data(0:im)
  real(8) :: g_data_orig(0:im)

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = - 12

  call at_Initial(im,km,xmin,xmax)

  call MessageNotify('M','at_af_galerkin_MM_test1d', &
    & '1D Mixed B.C.')

  !--- ξü�ǥ��ꥯ�춭����� ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=1.0D0, cfdx1_xmax=0.0D0, &
       cfdx0_xmin=1.0D0, cfdx1_xmin=0.0D0  )

  g_data = (g_X-xmin)*(xmax-g_X)
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check both Dirichlt B.C.')


  !--- ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳���� ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=1.0D0, cfdx1_xmax=0.0D0, &
       cfdx0_xmin=0.0D0, cfdx1_xmin=1.0D0  )

  g_data = (g_X-xmin)**2 * (xmax-g_X)
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check Dirichlt and Neuman B.C.')

  !--- ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭����� ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=0.0D0, cfdx1_xmax=1.0D0, &
       cfdx0_xmin=1.0D0, cfdx1_xmin=0.0D0  )

  g_data = (g_X-xmin) * (xmax-g_X)**2
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check Neumman and Dirichlt B.C.')

  !--- �ǥ��ꥯ�졦�Υ��ޥ󺮹維����� ---
  call at_af_galerkin_MM_Initial(im,km,        &
       cfdx0_xmax=1.0D0, cfdx1_xmax=-1.0D0, &
       cfdx0_xmin=1.0D0, cfdx1_xmin=1.0D0    )

  g_data = g_X**3 - 2*g_X**2
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check Neumman and Dirichlt mixed B.C.')


  !--- ξü�Υ��ޥ󶭳���� ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=0.0D0, cfdx1_xmax=1.0D0, &
       cfdx0_xmin=0.0D0, cfdx1_xmin=1.0D0  )

  g_data = (g_X-xmin)**2*(xmax-g_X)**2
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check both Neumman B.C.')

  call MessageNotify('M','at_af_galerkin_MM_test1d', &
    & '1D Mixed B.C. succeeded')

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

end program at_af_galerkin_MM_test1d
