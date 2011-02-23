!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_module �ƥ��ȥץ����
!      Laplacian �ղ�����
!
!����  2008/04/12  �ݹ�����
!      
!
program eq_test_LaplaInv

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, jm=16              ! �ʻ���������(X,Y)
  integer, parameter :: km=5, lm=21             ! �����ȿ�������(X,Y)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: ra=1.5

  real(8), parameter :: eps = 1.0d-10         ! Ƚ���

  real(8)            :: rp_A(jm,0:im-1)       ! �ʻҥǡ���
  real(8)            :: rp_A_sol(jm,0:im-1)   ! �ʻҥǡ���
  real(8)            :: eq_A(-km:km,0:lm)     ! ���ڥ��ȥ�ǡ���

  real(8), dimension(-km:km) :: value         ! �������

  integer,parameter  :: k=2, l=k+2

  call MessageNotify('M','eq_test_LaplaInv', &
       'eq_module subroutine for Inverse Laplacian tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! ���ڥ��ȥ�����

 !----- eq_LaplaInv_eq ------
  rp_A = sin(k*rp_Phi) * rp_Rad**l 
  rp_A_sol = sin(k*rp_Phi) * rp_Rad**(l+2)/((l+2)**2 - k**2)

  value = e_p(rp_A_sol(jm,:))

  eq_A = eq_rp(rp_A)
  rp_A = rp_eq(eq_LaplaInv_eq(eq_A,value))

  call check2d(rp_A-rp_A_sol, eps, 'LaplaInv(sin(k*Phi)*Rad**(l+2)')

  call MessageNotify('M','Test of eq_LaplaInv','Test of eq_LaplaInv suceeded!')

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
             call MessageNotify('E','ee_test_LaplaInv', &
                  'Inverse Laplacian error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program eq_test_LaplaInv
