!----------------------------------------------------------------------
!     Copyright (c) 2005-2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  esc_module �ƥ��ȥץ���� (�䥳�ӥ���׻�)
!
!����  2005/07/19  �ݹ�����
!      2007/11/13  �ݹ�����  ���顼��å������ɲ�
!
program esc_test_jacobian

  use dc_message, only : MessageNotify
  use esc_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_A(0:jm,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_B(0:jm,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_Jacob(0:jm,0:im-1)  ! �ʻҥǡ���

  integer            :: k=3,l=2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  call MessageNotify('M','esc_test_jacobian', &
       'ee_module jacobian function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of ee_module : Jacobian function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  yx_A = sin(k*pi*yx_Y)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = 0.0

  call check2d(yx_es(es_Jacobian_es_es(es_yx(yx_A),es_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(sin(k*pi*X),sin(l*pi*Y))')

  yx_A = sin(k*pi*yx_X)*sin(l*pi*yx_Y)
  yx_B = cos(k*pi*yx_X)*sin(l*pi*yx_Y)
  yx_Jacob = 0.5*k*l*pi**2 *sin(2*l*pi*yx_Y)

  call check2d(yx_es(es_Jacobian_es_es(es_yx(yx_A),es_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(sin(k*pi*X)*sin(l*pi*yx_Y),cos(k*pi*yx_X)*sin(l*pi*Y))')

  yx_A = cos(k*pi*yx_X)*sin(l*pi*yx_Y)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = k*l*pi**2 * sin(k*pi*yx_X) * sin(l*pi*yx_Y)**2

  call check2d(yx_ec(ec_Jacobian_es_ec(es_yx(yx_A),ec_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(cos(k*pi*X)*sin(l*pi*yx_Y),cos(l*pi*Y))')

  yx_A = sin(k*pi*yx_Y)
  yx_B = sin(l*pi*yx_X)
  yx_Jacob = - k*l*pi**2 * cos(k*pi*yx_Y)*cos(l*pi*yx_X)

  call check2d(yx_ec(ec_Jacobian_es_ec(es_yx(yx_A),ec_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(sin(k*pi*Y),sin(l*pi*X))')

  call MessageNotify('M','esc_test_jacobian', &
       'esc_module jacobian function tests succeeded!')

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
             call MessageNotify('E','esc_test_jacobian', &
                  'jacobian error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program esc_test_jacobian
