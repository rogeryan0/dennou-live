!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eq_module �ƥ��ȥץ���� (�����Ѵ�)
!
!����  2008/04/12  �ݹ�����
!      
program eq_test_transform

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, jm=8            ! �ʻ���������(Phi,Rad)
  integer, parameter :: km=5, lm=15            ! �����ȿ�������(Phi,Rad)

 !---- �ѿ� ----
  real(8)            :: rp_Data(jm,0:im-1)      ! �ʻҥǡ���

  integer            :: k=3

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: ra=3.0

  real(8), parameter :: eps = 1.0d-8            ! Ƚ���

  call MessageNotify('M','eq_test_transform', &
       'eq_module transform function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of eq_module : grid -> spectral -> grid transformation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l

  rp_Data = sin(k*rp_Phi) * rp_Rad**k
  call check2d(rp_eq(eq_rp(rp_Data))-rp_Data, eps, &
       'Inverse transform sin(k*Phi)*Rad**k')

  rp_Data = cos(k*rp_Phi) * rp_Rad**(k+2)
  call check2d(rp_eq(eq_rp(rp_Data))-rp_Data, eps,&
       'Inverse transoform cos(k*Phi)*Rad**(k+2)')

  rp_Data = sin(k*rp_Phi) * rp_Rad**(k+4)
  call check2d(rp_eq(eq_rp(rp_Data))-rp_Data, eps, &
       'Inverse transform sin(k*pi*Phi)*Rad**(k+4)')

  rp_Data = cos(k*rp_Phi) *rp_Rad**(k+6)
  call check2d(rp_eq(eq_rp(rp_Data))-rp_Data, eps,&
       'Inverse transform cos(k*pi*Phi)*Rad**(k+6)')

  call MessageNotify('M','eq_test_transform', &
       'eq_module transform function tests succeeded!')

 stop
contains

  subroutine check2d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:)                ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname
    integer i, j

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var,1)
       do j=1,size(var,2)
          if (abs(var(i,j)) .gt. eps ) then
             write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, var(i,j)
             call MessageNotify('E','eq_test_transform', &
                  'transform error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program eq_test_transform

