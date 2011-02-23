!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ap_galerkin_DN_test2d
!
!      �����ӥ����աݥ��顼����ˡ
!      ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳�����ѥ⥸�塼��ƥ��ȥץ����(2����)
!          f(:,i=0)=f'(:,i=im)=0 [ f(:,x=xmax)=f'(:,x=xmin)=0 ]
!
!      ��1 : f(x) = cos(pi/2*x),    0<x<1
!      ��2 : f(x) = cos(3*pi/2*x),  0<x<1
!
!����  2005/12/30  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_ap_galerkin_DN_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_ap_galerkin_DN
  use at_module

  implicit none

  integer, parameter :: jm=2                   ! 1 �����ܳʻ�����
  integer, parameter :: im=32                  ! �ʻ�����              
  integer, parameter :: km=32                  ! �����ӥ����������ȿ�  
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0 ! �׻��ΰ�

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)

  ! Ƚ�������
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = - 9
  real(8) :: pi
  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','at_ap_galerkin_DN_test2d', &
    & '2D Dirichlt-Neumman B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_ap_galerkin_DN_Initial(im,km) 

  ag_data(1,:) = cos(pi/2*g_X)
  ag_data(2,:) = cos(3*pi/2*g_X)
  ag_data_orig = ag_data
  ag_data = ag_ap(ap_ag(ag_data))

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> Grid ')

!  �������
!  ag_data = ag_ap(ap_Dx_ap(ap_Dx_ap(ap_data)))

!  �褤�� : ��ʬ�ͤϥ����ӥ����շ������ݻ�����٤�
  ag_data = ag_ap(ap_at(at_Dx_at(at_Dx_at(at_ap(ap_ag(ag_data))))))
  ag_data_orig(1,:) = -   pi**2/4*ag_data_orig(1,:)
  ag_data_orig(2,:) = - 9*pi**2/4*ag_data_orig(2,:)
  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> (Dx)^2 -> Grid ')

  call MessageNotify('M','at_ap_galerkin_DN_test2d', &
    & '2D Dirichlt-Neumman B.C. succeeded')

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

end program at_ap_galerkin_DN_test2d
