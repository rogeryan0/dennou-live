!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ad_galerkin_DD_test2d
!
!      �����ӥ����աݥ��顼����ˡ
!      ξü�ǥ��ꥯ�춭������ѥ⥸�塼��ƥ��ȥץ����(2����)
!          f(:,x=xmin)=f(:,x=xmax)=0
!
!      ��1 : f(x) = sin(pi*x),    0<x<1
!      ��2 : f(x) = sin(2*pi*x),  0<x<1
!
!����  2005/12/30  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_ad_galerkin_DD_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_ad_galerkin_DD
  use at_module

  implicit none

  integer, parameter :: jm=2                   ! 1 �����ܳʻ�����
  integer, parameter :: im=32                  ! �ʻ�����              
  integer, parameter :: km=32                  ! �����ӥ����������ȿ�  
  integer, parameter :: ks=2                   ! ���顼���������㼡��
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0 ! �׻��ΰ�

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)
  real(8) :: ad_data(jm,ks:km)
  ! Ƚ�������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11
  real(8) :: pi
  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','at_ad_galerkin_DD_test2d', &
       & '2D both Dirichlet B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_ad_galerkin_DD_Initial(im,km)           !--- ξü�ǥ��ꥯ�� ---

  ag_data(1,:) = sin(pi*g_X)
  ag_data(2,:) = sin(2*pi*g_X)
  ag_data_orig = ag_data

  ad_data = ad_ag(ag_data)
  ag_data = ag_ad(ad_data)

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> Grid ')

!  �������
!  ag_data = ag_ad(ad_Dx_ad(ad_Dx_ad(ad_data)))

!  �褤�� : ��ʬ�ͤϥ����ӥ����շ������ݻ�����٤�
  ad_data = ad_at(at_Dx_at(at_Dx_at(at_ad(ad_data))))
  ag_data = ag_ad(ad_data)
  
  ag_data_orig(1,:) = ag_data_orig(1,:) * (-pi**2)
  ag_data_orig(2,:) = ag_data_orig(2,:) * (-4*pi**2)

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> (Dx)^2 -> Grid ')

  call MessageNotify('M','at_ad_galerkin_DD_test2d', &
       & '2D both Dirichlet B.C. succeeded!')

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
  

end program at_ad_galerkin_DD_test2d
