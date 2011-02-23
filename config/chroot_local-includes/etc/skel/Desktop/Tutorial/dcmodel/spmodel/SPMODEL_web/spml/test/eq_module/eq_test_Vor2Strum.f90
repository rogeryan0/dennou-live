!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eq_module �ƥ��ȥץ����
!      ���٤���ή�������ؿ�
!
!����  2008/04/13  �ݹ�����  
!      
!
program eq_test_Vor2Strm

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=16             ! �ʻ���������(Phi,Rad)
  integer, parameter :: km=10, lm=21             ! �����ȿ�������(Phi,Rad)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: ra=1.5

  real(8), parameter :: eps = 1.0d-8            ! Ƚ���

  real(8)            :: rp_Vor(jm,0:im-1)     ! �ʻҥǡ���
  real(8)            :: rp_Psi(jm,0:im-1)     ! �ʻҥǡ���
  real(8)            :: rp_Vor_sol(jm,0:im-1) ! �ʻҥǡ���
  real(8)            :: rp_Psi_sol(jm,0:im-1) ! �ʻҥǡ���
  real(8)            :: rp_dPsidr(jm,0:im-1)  ! �ʻҥǡ���
  real(8)            :: rp_d2Psidr2(jm,0:im-1)! �ʻҥǡ���

  real(8)            :: eq_Psi(-km:km,0:lm)   ! ���ڥ��ȥ�ǡ���

  integer            :: k=2

  call MessageNotify('M','eq_test_Vor2Strm', &
       'eq_module Stream function calculation from vorticity tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! ���ڥ��ȥ�����

!----- eq_Vor2Strm (F) ------
  rp_Psi_sol = sin(k*rp_Phi)*rp_Rad**k*(rp_Rad**2-ra**2)**3

  rp_Vor_sol = rp_er(er_Lapla_eq(eq_rp(rp_Psi_sol)))
  eq_Psi = eq_Vor2Strm_eq(eq_rp(rp_Vor_sol),cond='F',new=.true.)
  rp_Psi = rp_eq(eq_Psi)
  rp_Vor = rp_er(er_Lapla_eq(eq_Psi))
  rp_d2Psidr2 = rp_eq(eq_RadDRad_eq(eq_RadDRad_eq(eq_Psi)) &
                      -2.0D0*eq_RadDRad_eq(eq_Psi))/rp_Rad**2

  call check2d(rp_Psi - rp_Psi_sol, eps, &
           'F-B.C. eq_Vor2Strm_eq Stream function test')

  call check2d(rp_Vor - rp_Vor_sol, eps, &
           'F-B.C. eq_Vor2Strm_eq Vorticity test')

  if ( maxval(abs(rp_Psi(jm,:))) > eps  ) then
     call MessageNotify('E','Test of eq_Vor2Strm', &
          'Kinematic B.C. error too large.')
  endif
  if ( maxval(abs(rp_d2Psidr2(jm,:))) > eps  ) then
     call MessageNotify('E','Test of eq_Vor2Strm', &
          'Free B.C. error too large.')
  endif

  call MessageNotify('M','Test of eq_Vor2Strm','Test of F-b.c. suceeded!')

!----- eq_Vor2Strm (R) ------
  rp_Psi_sol = sin(k*rp_Phi)*rp_Rad**k*(rp_Rad**2-ra**2)**2
  rp_Vor_sol = rp_er(er_Lapla_eq(eq_rp(rp_Psi_sol)))
  eq_Psi = eq_Vor2Strm_eq(eq_rp(rp_Vor_sol),cond='R',new=.true.)
  rp_Psi = rp_eq(eq_Psi)
  rp_Vor = rp_er(er_Lapla_eq(eq_Psi))
  rp_dPsidr = rp_eq(eq_RadDRad_eq(eq_Psi))/rp_Rad

  call check2d(rp_Psi - rp_Psi_sol, eps, &
           'R-B.C. eq_Vor2Strm_eq Stream function test')

  call check2d(rp_Vor - rp_Vor_sol, eps, &
           'R-B.C. eq_Vor2Strm_eq Vorticity test')

  if ( maxval(abs(rp_Psi(jm,:))) > eps  ) then
     call MessageNotify('E','Test of eq_Vor2Strm', &
          'Kinematic B.C. error too large.')
  endif
  if ( maxval(abs(rp_dPsidr(jm,:))) > eps  ) then
     call MessageNotify('E','Test of eq_Vor2Strm', &
          'Rigid B.C. error too large.')
  endif

  call MessageNotify('M','Test of eq_Vor2Strm','Test of R-b.c. suceeded!')

  call MessageNotify('M','eq_test_Vor2Strum', &
       'eq_module Stream function calculation from vorticity tests')

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
             call MessageNotify('E','eq_test_Vor2strm', &
                  'error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program eq_test_Vor2Strm
