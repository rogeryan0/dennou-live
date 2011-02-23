!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_aq_galerkin_SS_test2d
!
!      �����ӥ����աݥ��顼����ˡ�ƥ��ȥץ����(2����)
!      �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����, ξü��ͳ���٤���
!      (ξü���ͤ� 2 ����ʬ�� 0)
!
!        f(:,i=0)=f(:,i=im)=f''(:,i=0)=f''(:,i=im)=0 
!        [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      ��  : f(x) = sin(j*pi*x),    0<x<1
!
!����  2006/01/05  �ݹ�����  ��������
!      2006/01/24  �ݹ�����  �⥸�塼���ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_aq_galerkin_SS_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_aq_galerkin_RRFF
  use at_module

  implicit none

  integer, parameter :: jm=3                   ! 1 �����ܳʻ�����
  integer, parameter :: im=32                  ! �ʻ�����              
  integer, parameter :: km=32                  ! �����ӥ����������ȿ�  
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0 ! �׻��ΰ�

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)
  real(8) :: pi
  integer :: j

  ! Ƚ�������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = - 11

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','at_aq_galerkin_SS_test2d', &
    & '2D Slip-Slip B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_aq_galerkin_RRFF_Initial(im,km,'SS')    !--- ξü��ͳ���٤��� ---

  do j=1,jm
     ag_data(j,:) = sin(j*pi*g_X)
  enddo
  ag_data_orig = ag_data
  ag_data = ag_aq(aq_ag(ag_data))

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> Grid ',                    &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    & significant_digits = check_digits, ignore_digits = ignore &
    )
!  �������
!  ag_data = ag_aq(aq_Dx_aq(aq_Dx_aq(aq_data)))
!  �褤�� : ��ʬ�ͤϥ����ӥ����շ������ݻ�����٤�
  ag_data = ag_aq(aq_at(at_Dx_at(at_Dx_at(at_aq(aq_ag(ag_data))))))

  do j=1,jm
    ag_data_orig(j,:) = -(j*pi)**2 * ag_data_orig(j,:)
  enddo

  call AssertEqual(&
    & message = 'Grid -> Galerkin -> (Dx)^2 -> Grid ',          &
    & answer  = ag_data_orig,                                   &
    & check   = ag_data,                                        &
    & significant_digits = check_digits, ignore_digits = ignore &
    )

  call MessageNotify('M','at_aq_galerkin_SS_test2d', &
    & '2D Slip-Slip B.C. succeeded!')
  
end program at_aq_galerkin_SS_test2d
