!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ah_galerkin_MMex_test1d
!
!      �����ӥ����աݥ��顼����ˡ
!      �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����(���ѷ���)�ѥ⥸�塼��ƥ��ȥץ����
!      
!          cfdx1_xmax(j) f'(j,i=0)  + cfdx0_xmax f(j,i=0)  = 0,
!          cfdx1_xmin(j) f'(j,i=im) + cfdx0_xmin f(j,i=im) = 0,
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
!����  2006/01/06  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2006/02/15  �ݹ�����  �������� 0 �ξ����б�
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_ah_galerkin_MMex_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_ah_galerkin_MMex
  use at_module

  implicit none

  integer, parameter :: jm=5                   ! 1 �����ܳʻ�����
  integer, parameter :: im=64                  ! �ʻ�����              
  integer, parameter :: km=64                  ! �����ӥ����������ȿ�  
  real(8), parameter :: xmin=0.0, xmax=1.0     ! �׻��ΰ�

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)

  real(8) :: cfdx0_xmin(jm)                    ! ������ﷸ��(0����ʬ@x=xmin)
  real(8) :: cfdx1_xmin(jm)                    ! ������ﷸ��(1����ʬ@x=xmin)
  real(8) :: cfdx0_xmax(jm)                    ! ������ﷸ��(0����ʬ@x=xmax)
  real(8) :: cfdx1_xmax(jm)                    ! ������ﷸ��(1����ʬ@x=xmax)
  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = - 12

  call MessageNotify('M','at_ah_galerkin_MMex_test1d', &
    & '1D Mixed B.C.')

  call at_Initial(im,km,xmin,xmax)

  !--- (j=1)ξü�ǥ��ꥯ�춭����� ---
  cfdx0_xmax(1)=1.0D0 ; cfdx1_xmax(1)=0.0D0
  cfdx0_xmin(1)=1.0D0 ; cfdx1_xmin(1)=0.0D0

  ag_data(1,:) = (g_X-xmin)*(xmax-g_X)

  !--- (j=2)��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳���� ---
  cfdx0_xmax(2)=1.0D0 ; cfdx1_xmax(2)=0.0D0
  cfdx0_xmin(2)=0.0D0 ; cfdx1_xmin(2)=1.0D0

  ag_data(2,:) = (g_X-xmin)**2 * (xmax-g_X)

  !--- (j=3)��ü�Υ��ޥ���ü�ǥ��ꥯ�춭����� ---
  cfdx0_xmax(3)=0.0D0 ; cfdx1_xmax(3)=1.0D0
  cfdx0_xmin(3)=1.0D0 ; cfdx1_xmin(3)=0.0D0

  ag_data(3,:) = (g_X-xmin) * (xmax-g_X)**2

  !--- (j=4)�ǥ��ꥯ�졦�Υ��ޥ󺮹維����� ---
  cfdx0_xmax(4)=1.0D0 ; cfdx1_xmax(4)=-1.0D0
  cfdx0_xmin(4)=1.0D0 ; cfdx1_xmin(4)=1.0D0 

  ag_data(4,:) = g_X**3 - 2*g_X**2

  !--- (j=5)ξü�Υ��ޥ󶭳���� ---
  cfdx0_xmax(5)=0.0D0 ; cfdx1_xmax(5)=1.0D0
  cfdx0_xmin(5)=0.0D0 ; cfdx1_xmin(5)=1.0D0

  ag_data(5,:) = (g_X-xmin)**2*(xmax-g_X)**2

  !--- �⥸�塼������ ---
  call at_ah_galerkin_MMex_Initial(im,km,jm,         &
       cfdx0_xmax=cfdx0_xmax,cfdx1_xmax=cfdx1_xmax,  &
       cfdx0_xmin=cfdx0_xmin,cfdx1_xmin=cfdx1_xmin    )

  ag_data_orig = ag_data
  ag_data = ag_ah(ah_ag(ag_data))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','at_ah_galerkin_MMex_test1d', &
    & '1D Mixed B.C. succeeded')

end program at_ah_galerkin_MMex_test1
