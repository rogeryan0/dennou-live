!----------------------------------------------------------------------
! Copyright (c) 2007-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_module �ƥ��ȥץ���� :: ��ִؿ��Υƥ���
!
!����  2007/10/24  �ݹ�����
!      2009/01/04  �ݹ�����  spml ��ˡ��ȿ���뤿��ʣ��������б��Ǻ��
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_test_interpolate

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none

  integer, parameter    :: im=32, km=32
  real(8), parameter    :: xmin=-5.0D0, xmax=1.0D0
  ! ��֤������κ�ɸ
  real(8), parameter    :: xval=5.0d-1                       
  !--- Interpolate_t ��
  real(8)                    :: zsol, zint             ! �����(����, �׻���)
  real(8), dimension(0:im)   :: g_z
  !--- a_Interpolate_at ��
  real(8), dimension(2)      :: ysol, yint          ! �����(����, �׻���)
  real(8), dimension(2,0:im) :: ag_y
  ! Ƚ�������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11
  !--- ����ѿ�
  integer :: i

  call MessageNotify('M','at_test_interpolate', &
    & 'Test of Interpolate function')

  do i=1,2
    call at_initial(im,km/i,xmin,xmax)
    
    !--------- Test of "Interpolate_t" ----------
    g_z = sin(g_X-(xmax+xmin)/3.0d0)
    zsol = sin(xval-(xmax+xmin)/3.0d0)
    zint = Interpolate_t(t_g(g_z),xval)
    call check0d(zint, zsol, 'Test of Interpolate_t')
    
    !--------- Test of "a_Interpolate_at" ----------
    ag_y(1,:) = cos(g_X-(xmax+xmin)/4.0d0)
    ag_y(2,:) = sin(g_X+(xmax+xmin)/4.0d0)
    ysol(1) =  cos(xval-(xmax+xmin)/4.0d0)
    ysol(2) = sin(xval+(xmax+xmin)/4.0d0)
    yint = a_Interpolate_at(at_ag(ag_y),xval)
    call check1d(yint, ysol, 'Test of a_Interpolate_at')
    
  enddo
  
call MessageNotify('M','Test of Interpolate_t', &
  & 'Test of Interpolate function succeeded')

contains
  subroutine check1d(var, true, funcname)
    real(8) :: var(:)
    real(8) :: true(:)
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,     &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check1d

  subroutine check0d(var0d,true0d, funcname)
    real(8) :: var0d
    real(8) :: true0d
    real(8) :: var_tmp(1)
    real(8) :: true_tmp(1)
    character(len=*), optional :: funcname

    var_tmp(1) = var0d
    true_tmp(1) = true0d
    call check1d(var_tmp, true_tmp, funcname)
  end subroutine check0d
  

end program at_test_interpolate
