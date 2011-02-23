!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_av_galerkin_NN_test2d
!
!      �����ӥ����աݥ��顼����ˡ
!      ξü�Υ��ޥ󶭳�����ѥ⥸�塼��ƥ��ȥץ����(2����)
!          f(:,x=xmin)=f(:,x=xmax)=0
!
!      ��1 : f(x) = cos(pi*x),    0<x<1
!      ��2 : f(x) = cos(2*pi*x),  0<x<1
!
!����  2005/12/30  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_av_galerkin_NN_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_av_galerkin_NN
  use at_module

  implicit none

  integer, parameter :: jm=2                   ! 1 �����ܳʻ�����
  integer, parameter :: im=32                  ! �ʻ�����              
  integer, parameter :: km=32                  ! �����ӥ����������ȿ�  
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0 ! �׻��ΰ�

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)
  real(8) :: pi
  ! Ƚ�������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = - 11


  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','at_av_galerkin_NN_test2d', &
    & '2D both Neumman B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_av_galerkin_NN_Initial(im,km)           !--- ξü�ǥ��ꥯ�� ---

  ag_data(1,:) = cos(pi*g_X)
  ag_data(2,:) = cos(2.0d0*pi*g_X)
  ag_data_orig = ag_data
  ag_data = ag_av(av_ag(ag_data))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    & significant_digits = check_digits, ignore_digits = ignore &
    )
!  �������
!  ag_data = ag_av(av_Dx_av(av_Dx_av(av_data)))
!  �褤�� : ��ʬ�ͤϥ����ӥ����շ������ݻ�����٤�
  ag_data = ag_av(av_at(at_Dx_at(at_Dx_at(at_av(av_ag(ag_data))))))
  ag_data_orig(1,:) = -     pi**2 * ag_data_orig(1,:) 
  ag_data_orig(2,:) = - 4 * pi**2 * ag_data_orig(2,:) 

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> (Dx)^2 -> Grid',           &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    & significant_digits = check_digits, ignore_digits = ignore &
    )

  call MessageNotify('M','at_av_galerkin_NN_test2d', &
    & '2D both Neumman B.C. succeeded')

end program at_av_galerkin_NN_test2d
