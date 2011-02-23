!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  esc_module �ƥ��ȥץ���� (�����Ѵ�)
!
!����  2007/11/12  �ݹ�����
!      
program esc_test_transform

  use dc_message, only : MessageNotify
  use esc_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ec_Data(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: es_Data(-km:km,lm)      ! ���ڥ��ȥ�ǡ���

  integer            :: k=2,l=5

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin =  0.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  call MessageNotify('M','esc_test_transform', &
       'esc_module transform function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of esc_module : grid -> spectral -> grid transformation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  es_Data = 0.0 ; es_Data(-k,l) = -0.5
  call check2d(es_yx(yx_Data)-es_Data, eps, &
       'Transform sin(k*pi*X)*sin(l*pi*Y)')
  call check2d(yx_es(es_yx(yx_Data))-yx_Data, eps, &
       'Inverse transform sin(k*pi*X)*sin(l*pi*Y)')

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ec_Data = 0.0 ; ec_Data(k,l) = 0.5
  call check2d(ec_yx(yx_Data)-ec_Data, eps, &
       'Transform cos(k*pi*X)*cos(l*pi*Y)')
  call check2d(yx_ec(ec_yx(yx_Data))-yx_Data, eps,&
       'Inverse transoform cos(k*pi*X)*cos(l*pi*Y)')

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ec_Data = 0.0 ; ec_Data(-k,l) = -0.5
  call check2d(ec_yx(yx_Data)-ec_Data, eps, &
       'Transform sin(k*pi*X)*cos(l*pi*Y)')
  call check2d(yx_ec(ec_yx(yx_Data))-yx_Data, eps, &
       'Inverse transform sin(k*pi*X)*cos(l*pi*Y)')

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  es_Data = 0.0 ; es_Data(k,l) = 0.5
  call check2d(es_yx(yx_Data)-es_Data, eps, &
       'Transform cos(k*pi*X)*sin(l*pi*Y)')
  call check2d(yx_es(es_yx(yx_Data))-yx_Data, eps,&
       'Inverse transform cos(k*pi*X)*sin(l*pi*Y)')

  call MessageNotify('M','esc_test_transform', &
       'esc_module transform function tests succeeded!')

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
             call MessageNotify('E','esc_test_transform', &
                  'transform error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program esc_test_transform

