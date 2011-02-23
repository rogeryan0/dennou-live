!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eq_module �ƥ��ȥץ���� (�䥳�ӥ���׻�)
!
!����  2008/04/13  �ݹ�����
!
program eq_test_jacobian

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=16            ! �ʻ���������(Phi,Rad)
  integer, parameter :: km=10, lm=21            ! �����ȿ�������(Phi,Rad)

 !---- �ѿ� ----
  real(8)            :: rp_A(jm,0:im-1)        ! �ʻҥǡ���
  real(8)            :: rp_B(jm,0:im-1)        ! �ʻҥǡ���
  real(8)            :: rp_Jacob(jm,0:im-1)    ! �ʻҥǡ���

  integer            :: k=3,l=2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: ra=2.0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-8            ! Ƚ���

  call MessageNotify('M','eq_test_jacobian', &
       'eq_module jacobian function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of eq_module : Jacobian function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  rp_A = sin(k*rp_Phi)*rp_Rad**k
  rp_B = sin(k*rp_Phi)*rp_Rad**k
  rp_Jacob = 0.0

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(sin(k*Phi)*rp_Rad**k,sin(k*Phi)*rp_Rad**k)')

  rp_A = sin(k*rp_Phi)*rp_Rad**k
  rp_B = cos(k*rp_Phi)*rp_Rad**k
  rp_Jacob = -k**2 * rp_Rad**(2*k-2)

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(sin(k*Phi)*rp_Rad**k,cos(k*rp_Phi)*Rad**k')

  rp_A = cos(k*rp_Phi)*rp_Rad**(k+2)
  rp_B = rp_Rad**2
  rp_Jacob =  2 * k * sin(k*rp_Phi)*rp_Rad**(k+2)

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(cos(k*Phi)*Rad**(k+2),Rad**2))')

  rp_A = rp_Rad**4
  rp_B = sin(l*rp_Phi)*rp_Rad**l
  rp_Jacob = 4 * l * rp_Rad**(l+2) * cos(l*rp_Phi)

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(Rad**4,sin(l*Phi)*Rad**l)')

  call MessageNotify('M','eq_test_jacobian', &
       'eq_module jacobian function tests succeeded!')

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
             call MessageNotify('E','eq_test_jacobian', &
                  'jacobian error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program eq_test_jacobian
