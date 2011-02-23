!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eee_module �ƥ��ȥץ����
!
!����  2008/05/04  �ݹ�����
!
program eee_test_RotVelxVor

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=64, km=32          ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=10          ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8)            :: zyx_VelX(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� X)
  real(8)            :: zyx_VelY(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� Y)
  real(8)            :: zyx_VelZ(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(®�� Z)

  real(8)            :: zyx_VorX(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� X)
  real(8)            :: zyx_VorY(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� Y)
  real(8)            :: zyx_VorZ(0:km-1,0:jm-1,0:im-1)    ! �ʻҥǡ���(���� Z)

  real(8)            :: zyx_VelxVorX(0:km-1,0:jm-1,0:im-1)! �ʻҥǡ���(®��x���� X)
  real(8)            :: zyx_VelxVorY(0:km-1,0:jm-1,0:im-1)! �ʻҥǡ���(®��x���� Y)
  real(8)            :: zyx_VelxVorZ(0:km-1,0:jm-1,0:im-1)! �ʻҥǡ���(®��x���� Z)

  real(8)            :: eee_RotVelxVorX(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_RotVelxVorY(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_RotVelxVorZ(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: eee_VorX(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_VorY(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: eee_VorZ(-nm:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: eee2_Zeta(-nm:nm,-mm:mm,-lm:lm,2) ! ���ڥ��ȥ�ǡ���x2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  integer :: l, m, n, i

  call MessageNotify('M','eee_test_RotVelxVor', &
       'eee_module RotVelxVor functions tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call eee_initial(im,jm,km,lm,mm,nm)

 !---------------- ®�١����٤����� ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  l=2 ; m=-1
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

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

 !---------------- ���� �� ���颦x(®��x����)�η׻� ---------------------
  eee2_Zeta = eee2_RotVelxVor_eee2(eee2_Zeta)

 !---------------- �ʻ����Ǣ�x(®��x����)�η׻� ---------------------
  zyx_VelxVorX = zyx_VelY*zyx_VorZ - zyx_VelZ*zyx_VorY
  zyx_VelxVorY = zyx_VelZ*zyx_VorX - zyx_VelX*zyx_VorZ
  zyx_VelxVorZ = zyx_VelX*zyx_VorY - zyx_VelY*zyx_VorX

  eee_RotVelxVorX = eee_Dy_eee(eee_zyx(zyx_VelxVorZ)) &
                  - eee_Dz_eee(eee_zyx(zyx_VelxVorY))
  eee_RotVelxVorY = eee_Dz_eee(eee_zyx(zyx_VelxVorX)) &
                  - eee_Dx_eee(eee_zyx(zyx_VelxVorZ))
  eee_RotVelxVorZ = eee_Dx_eee(eee_zyx(zyx_VelxVorY)) &
                  - eee_Dy_eee(eee_zyx(zyx_VelxVorX))


 !---------------- �׻������å� ---------------------
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,1)-eee_RotVelxVorX, eps, &
       'Calculation of RotVelxVorX')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,2)-eee_RotVelxVorY, eps, &
       'Calculation of RotVelxVorY')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,3)-eee_RotVelxVorZ, eps, &
       'Calculation of RotVelxVorZ')

  call MessageNotify('M','eee_test_RotVelxVor', &
       'eee_module RotVelxVor functions tests suceeded')

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

end program eee_test_RotVelxVor
