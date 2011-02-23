!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_av_galerkin_NN_test1d
!
!      �����ӥ����աݥ��顼����ˡ
!      ξü�ǥ��ꥯ�춭������ѥ⥸�塼��ƥ��ȥץ����(1����)
!          f'(x=xmin)=f'(x=xmax)=0
!
!      �� : f(x) = (x-xmin)**2 * (x-xmax)**2
!
!
!����  2005/12/30  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_av_galerkin_NN_test1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_av_galerkin_NN
  use at_module

  implicit none

  integer, parameter :: im=6, km=6
  real(8), parameter :: xmin=0.0, xmax=1.0

  real(8) :: g_data(0:im)
  real(8) :: g_data_orig(0:im)
  ! Ƚ�������
  integer, parameter :: check_digits = 15
  integer, parameter :: ignore = - 16

  call MessageNotify('M','at_av_galerkin_NN_test1d', &
    & '1D both Neumman B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_av_galerkin_NN_Initial(im,km)    

  g_data = (g_X-xmin)**2*(xmax-g_X)**2
  g_data_orig = g_data
  g_data = g_v(v_t(t_g(g_data)))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = g_data_orig,                                    &
    & check   = g_data,                                         &
    & significant_digits = check_digits, ignore_digits = ignore &
    )

  call MessageNotify('M','at_av_galerkin_NN_test1d', &
    & '1D both Neumman B.C. succeeded')

end program at_av_galerkin_NN_test1d
