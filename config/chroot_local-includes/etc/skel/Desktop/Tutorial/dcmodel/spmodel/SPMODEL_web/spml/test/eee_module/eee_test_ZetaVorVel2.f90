!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module �ƥ��ȥץ����
!
!����  2008/05/04  �ݹ�����
!
program eee_test_ZetaVorVel

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=64, km=16          ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=5           ! �����ȿ�������(X,Y,Z)

  integer, parameter :: im2=64, jm2=64, km2=64       ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm2=21, mm2=21, nm2=21       ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8)  :: zyx_VelX(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� X)
  real(8)  :: zyx_VelY(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� Y)
  real(8)  :: zyx_VelZ(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� Z)

  real(8)  :: zyx_VorX(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� X)
  real(8)  :: zyx_VorY(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� Y)
  real(8)  :: zyx_VorZ(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� Z)

  real(8)  :: eee_VelX(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VelY(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VelZ(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���

  real(8)  :: eee_VorX(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VorY(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VorZ(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���

  real(8)  :: eee2_Zeta(-nm:nm,-mm:mm,-lm:lm,2) ! ���ڥ��ȥ�ǡ���x2


  real(8)  :: zyx_VelX2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(®�� X)
  real(8)  :: zyx_VelY2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(®�� Y)
  real(8)  :: zyx_VelZ2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(®�� Z)

  real(8)  :: zyx_VorX2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(���� X)
  real(8)  :: zyx_VorY2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(���� Y)
  real(8)  :: zyx_VorZ2(0:km2-1,0:jm2-1,0:im2-1)    ! �ʻҥǡ���(���� Z)

  real(8)  :: eee_VelX2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VelY2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VelZ2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���

  real(8)  :: eee_VorX2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VorY2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���
  real(8)  :: eee_VorZ2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! ���ڥ��ȥ�ǡ���

  real(8)  :: eee2_Zeta2(-nm2:nm2,-mm2:mm2,-lm2:lm2,2) ! ���ڥ��ȥ�ǡ���x2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  integer :: l, m, n
  integer :: id1, id2

  call MessageNotify('M','eee_test_ZetaVelVor', &
       'eee_module Zeta-Vor,Vel transform functions tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call eee_initial(im,jm,km,lm,mm,nm,id1)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2,id2)

 !---------------- ®�١����٤����� ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  l = 1 ; m=2
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  ! id1
  write(*,*) 'for id1'
  call eee_ChangeResolution(id1)

  zyx_VelX = cos(l*zyx_X)*sin(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelY = sin(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelZ = sin(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)

  eee_VelX = eee_zyx(zyx_VelX)
  eee_VelY = eee_zyx(zyx_VelY)
  eee_VelZ = eee_zyx(zyx_VelZ)

  zyx_VorX = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX = eee_zyx(zyx_VorX)
  eee_VorY = eee_zyx(zyx_VorY)
  eee_VorZ = eee_zyx(zyx_VorZ)

 !---------------- ���� �� �ؤ��Ѵ� ---------------------
  eee2_Zeta = eee2_ZetaFromVor_eee_eee_eee(eee_VorX,eee_VorY,eee_VorZ)

 !---------------- ���� �� ����ε��Ѵ�(����) ---------------------
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,1)-eee_VorX, eps, &
       'Transform and Inverse Transform of VorX')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,2)-eee_VorY, eps, &
       'Transform and Inverse Transform of VorY')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,3)-eee_VorZ, eps, &
       'Transform and Inverse Transform of VorZ')

 !---------------- ���� �� ����ε��Ѵ�(®��) ---------------------
  call check3d(eee_VelFromZeta_eee2(eee2_Zeta,1)-eee_VelX, eps, &
       'Transform and Inverse Transform of VelX')
  call check3d(eee_VelFromZeta_eee2(eee2_Zeta,2)-eee_VelY, eps, &
       'Transform and Inverse Transform of VelY')
  call check3d(eee_VelFromZeta_eee2(eee2_Zeta,3)-eee_VelZ, eps, &
       'Transform and Inverse Transform of VelZ')

 !---------------- ®�١����٤����� ---------------------
  ! id2
  write(*,*) 'for id2'
  call eee_ChangeResolution(id2)

  zyx_VelX2 = cos(l*zyx_X)*sin(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelY2 = sin(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelZ2 = sin(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)

  eee_VelX2 = eee_zyx(zyx_VelX2)
  eee_VelY2 = eee_zyx(zyx_VelY2)
  eee_VelZ2 = eee_zyx(zyx_VelZ2)

  zyx_VorX2 = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY2 = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ2 = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX2 = eee_zyx(zyx_VorX2)
  eee_VorY2 = eee_zyx(zyx_VorY2)
  eee_VorZ2 = eee_zyx(zyx_VorZ2)

 !---------------- ���� �� �ؤ��Ѵ� ---------------------
  eee2_Zeta2 = eee2_ZetaFromVor_eee_eee_eee(eee_VorX2,eee_VorY2,eee_VorZ2)

 !---------------- ���� �� ����ε��Ѵ�(����) ---------------------
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta2,1)-eee_VorX2, eps, &
       'Transform and Inverse Transform of VorX')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta2,2)-eee_VorY2, eps, &
       'Transform and Inverse Transform of VorY')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta2,3)-eee_VorZ2, eps, &
       'Transform and Inverse Transform of VorZ')

 !---------------- ���� �� ����ε��Ѵ�(®��) ---------------------
  call check3d(eee_VelFromZeta_eee2(eee2_Zeta2,1)-eee_VelX2, eps, &
       'Transform and Inverse Transform of VelX')
  call check3d(eee_VelFromZeta_eee2(eee2_Zeta2,2)-eee_VelY2, eps, &
       'Transform and Inverse Transform of VelY')
  call check3d(eee_VelFromZeta_eee2(eee2_Zeta2,3)-eee_VelZ2, eps, &
       'Transform and Inverse Transform of VelZ')


  call MessageNotify('M','eee_test_ZetaVorVel', &
       'eee_module Zeta-Vor,Vel transform functions tests')

 stop
contains

  subroutine check3d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:,:)              ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname
    integer i, j, k

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k)
                call MessageNotify('E','eee_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_test_ZetaVorVel
