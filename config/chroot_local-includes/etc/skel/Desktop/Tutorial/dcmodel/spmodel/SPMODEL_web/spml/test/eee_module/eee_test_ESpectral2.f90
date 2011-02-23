!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module �ƥ��ȥץ����
!
!����  2008/05/04  �ݹ�����
!      2008/05/10  �ݹ�����  ������ʣ����������å�
!
program eee_test_ESpectral

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=64, km=16          ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=5           ! �����ȿ�������(X,Y,Z)

  integer, parameter :: im2=64, jm2=64, km2=64       ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm2=21, mm2=21, nm2=21       ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8)            :: zyx_VelX(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� X)
  real(8)            :: zyx_VelY(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� Y)
  real(8)            :: zyx_VelZ(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� Z)

  real(8)            :: zyx_VorX(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� X)
  real(8)            :: zyx_VorY(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� Y)
  real(8)            :: zyx_VorZ(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� Z)

  real(8)            :: eee_VorX(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_VorY(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_VorZ(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: eee2_Zeta(-nm:nm,-mm:mm,-lm:lm,2) ! ���ڥ��ȥ�ǡ���x2

  real(8)            :: zyx_VelX2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(®�� X)
  real(8)            :: zyx_VelY2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(®�� Y)
  real(8)            :: zyx_VelZ2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(®�� Z)

  real(8)            :: zyx_VorX2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(���� X)
  real(8)            :: zyx_VorY2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(���� Y)
  real(8)            :: zyx_VorZ2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(���� Z)

  real(8)            :: eee_VorX2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_VorY2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_VorZ2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: eee2_Zeta2(-nm2:nm2,-mm2:mm2,-lm2:lm2,2) ! ���ڥ��ȥ�ǡ���x2

  real(8), allocatable :: ESp(:), ESp0(:)                 ! ���ͥ륮�����ڥ��ȥ�

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  integer :: l, m, n, i
  integer :: id1, id2

  call MessageNotify('M','eee_test_ZetaVelVor', &
       'eee_module Zeta-Vor,Vel transform functions tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call eee_initial(im,jm,km,lm,mm,nm,id1)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2,id2)

  allocate(ESp(ceiling(sqrt(lm**2.0+mm**2.0+nm**2.0))))
  allocate(ESp0(size(ESp)))

 !---------------- ®�١����٤����� ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  l=1 ; m=-1
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  ! id1
  write(*,*) 'for id1'
  call eee_ChangeResolution(id1)

  zyx_VelX = cos(l*zyx_X)*sin(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelY = sin(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelZ = sin(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)

  zyx_VorX = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX = eee_zyx(zyx_VorX)
  eee_VorY = eee_zyx(zyx_VorY)
  eee_VorZ = eee_zyx(zyx_VorZ)

 !---------------- ���� �� �ؤ��Ѵ� ---------------------
  eee2_Zeta = eee2_ZetaFromVor_eee_eee_eee(eee_VorX,eee_VorY,eee_VorZ)

 !---------------- ���ͥ륮�����ڥ��ȥ� ---------------------
  call ESpectralFromZeta(ESp,eee2_Zeta)

  ESp0 = 0.0
  ESp0(nint(sqrt(l**2.0+m**2.0+n**2.0))) &
       = 0.5*AvrZYX_zyx(zyx_VelX**2+zyx_VelY**2+zyx_VelZ**2)
  
!!$  do i=1, size(ESp)
!!$     write(6,*) i, ESp(i), ESP0(i)
!!$  enddo

  call check1d(ESp-ESp0,eps,'ESpectralFromZeta')

 !---------------- ®�١����٤����� ---------------------
  deallocate(ESp,ESp0)
  allocate(ESp(ceiling(sqrt(lm2**2.0+mm2**2.0+nm2**2.0))))
  allocate(ESp0(size(ESp)))

!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  ! id1
  write(*,*) 'for id2'
  call eee_ChangeResolution(id2)

  zyx_VelX2 = cos(l*zyx_X)*sin(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelY2 = sin(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelZ2 = sin(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)

  zyx_VorX2 = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY2 = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ2 = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX2 = eee_zyx(zyx_VorX2)
  eee_VorY2 = eee_zyx(zyx_VorY2)
  eee_VorZ2 = eee_zyx(zyx_VorZ2)

 !---------------- ���� �� �ؤ��Ѵ� ---------------------
  eee2_Zeta2 = eee2_ZetaFromVor_eee_eee_eee(eee_VorX2,eee_VorY2,eee_VorZ2)

 !---------------- ���ͥ륮�����ڥ��ȥ� ---------------------
  call ESpectralFromZeta(ESp,eee2_Zeta2)

  ESp0 = 0.0
  ESp0(nint(sqrt(l**2.0+m**2.0+n**2.0))) &
       = 0.5*AvrZYX_zyx(zyx_VelX2**2+zyx_VelY2**2+zyx_VelZ2**2)
  
!!$  do i=1, size(ESp)
!!$     write(6,*) i, ESp(i), ESP0(i)
!!$  enddo

  call check1d(ESp-ESp0,eps,'ESpectralFromZeta')

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
          write(6,*) &
               '    Value larger than EPS : i= ', i, var(i)
          call MessageNotify('E','eee_test_ESpectral', &
               'spectral calculation error too large')
       endif
    enddo
  end subroutine check1d

end program eee_test_ESpectral
