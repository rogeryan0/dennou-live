!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  et_module �ƥ��ȥץ����
!      ����������
!
!����  2007/11/19  �ݹ�����  
!      
!
program et_test_boundaries

  use dc_message, only : MessageNotify
  use et_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=21              ! �����ȿ�������(X,Y)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  real(8)            :: yx_A(0:jm,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_dAdy(0:jm,0:im-1) ! �ʻҥǡ���
  real(8)            :: et_A(-km:km,0:lm)     ! ���ڥ��ȥ�ǡ���

  real(8), dimension(-km:km,2)  :: values

  integer            :: k=2

  call MessageNotify('M','et_test_boudaries', &
       'et_module subroutine for boundary value problem tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

!----- et_Boundaries (DD) ------
  yx_A = sin(k*pi*yx_X)*sin(pi*yx_Y)
  et_A = et_yx(yx_a)
  values = 0.0

  call et_Boundaries(et_A,values=values,cond='DD')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))

  if ( maxval(abs(yx_A(0,:)-values(0,1))) > eps .OR. &
       maxval(abs(yx_A(jm,:)-values(0,2))) > eps )then
     call MessageNotify('E','Test of et_Boundaries', &
          'B.C. adoption error too large.')
  endif

  call MessageNotify('M','Test of et_Boundaries','Test of DD b.c. suceeded!')
 
!----- et_Boundaries (DN) ------
  yx_A = sin(k*pi*yx_X)*sin(pi/2*yx_Y)
  et_A = et_yx(yx_a)

  call et_Boundaries(et_A,values,cond='DN')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))
  if ( maxval(abs(yx_A(0,:)-values(0,1))) > eps .OR. &
       maxval(abs(yx_dAdy(jm,:)-values(0,2))) > eps )then
     call MessageNotify('E','Test of et_Boundaries', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of et_Boundaries','Test of DN b.c. suceeded!')

!----- et_Boundaries (ND) ------
  yx_A = sin(k*pi*yx_X)*cos(pi/2*yx_Y)
  et_A = et_yx(yx_a)
  call et_Boundaries(et_A,values,cond='ND')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))

  if ( maxval(abs(yx_dAdy(0,:)-values(0,1))) > eps .OR. &
       maxval(abs(yx_A(jm,:)-values(0,2))) > eps )then
     call MessageNotify('E','Test of et_Boundaries', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of et_Boundaries','Test of ND b.c. suceeded!')

!----- et_Boundaries (ND) ------
  yx_A = sin(k*pi*yx_X)*cos(pi*yx_Y)
  et_A = et_yx(yx_a)
  call et_Boundaries(et_A,values,cond='NN')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))

  if ( maxval(abs(yx_dAdy(0,:)-values(0,1))) > eps .OR. &
       maxval(abs(yx_dAdy(jm,:)-values(0,2))) > eps )then
     call MessageNotify('E','Test of et_Boundaries', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of et_Boundaries','Test of NN b.c. suceeded!')

  call MessageNotify('M','et_test_boudaries', &
       'et_module subroutine for boundary value problem tests succeeded!')

end program et_test_boundaries
