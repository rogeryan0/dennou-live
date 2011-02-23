!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  au_module �ƥ��ȥץ����
!
!����  2002/08/20  �ݹ�����
!      2007/10/24  �ݹ�����  ���顼��å������ɲ�
!
program au_intavr

  use dc_message, only : MessageNotify
  use au_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32            ! �ʻ���������
  integer, parameter :: km=32,lm=2        ! �����ȿ�������

 !---- �ѿ� ----
  real(8)            :: g_Data(0:im)     ! �ʻҥǡ���
  real(8)            :: ag_Data(lm,0:im)    ! �ʻҥǡ���

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: ra=3.0

  real(8), parameter :: eps = 1.0d-3            ! Ƚ���
  integer :: i

 !---------------- ��ɸ�ͤ����� ---------------------

do i=1,2
  call au_Initial(im,km/i,ra,(/0,0/))               ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*)'+++++ Test of Int_g, Avr_g +++++'

  g_Data = 1 - g_R
  write(6,*) 'f = 1-r'

    call check0d(Int_g(g_Data) - (ra-ra**2/2), eps, 'Int_g')
    call check0d(Avr_g(g_Data) - (1-ra/2), eps, 'Avr_g')

  call MessageNotify('M','Test of Int_g, Avr_g', &
       'Test of Int_g, Avr_g suceeded!')

  write(6,*)
  write(6,*)'+++++ Test of a_Int_ag, a_Avr_ag +++++'
  ag_Data(1,:) = g_R**2
  ag_Data(2,:) = g_R**5
  write(6,*) 'f = g_R**2, g_R**5'

    call check1d(a_Int_ag(ag_Data) - (/ra**3/3,ra**6/6/), eps, 'a_Int_ag')
    call check1d(a_Avr_ag(ag_Data) - (/ra**2/3,ra**5/6/), eps, 'a_Avr_ag')

  call MessageNotify('M','Test of a_Int_ag, a_Avr_ag', &
       'Test of a_Int_ag, a_Avr_ag suceeded!')

end do
stop
contains

  subroutine check1d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:)                  ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname
    integer i

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var)
       if (abs(var(i)) .gt. eps ) then
          write(6,*) '    Value larger than EPS : i= ', i, var(i)
          call MessageNotify('E','Test of'//funcname, 'Error too large.')
       endif
    enddo
  end subroutine check1d

  subroutine check0d(var,eps,funcname)   ! var �������ͤ� eps �ʾ���Ƚ���
    real(8) :: var                       ! Ƚ�ꤹ������
    real(8) :: eps                       ! ��
    real(8) :: vartmp(1)                 ! 
    character(len=*), optional :: funcname

    vartmp(1) = var
    if ( present(funcname) ) then
       call check1d(vartmp,eps,funcname)
    else
       call check1d(vartmp,eps)
    endif
  end subroutine check0d

end program au_intavr
