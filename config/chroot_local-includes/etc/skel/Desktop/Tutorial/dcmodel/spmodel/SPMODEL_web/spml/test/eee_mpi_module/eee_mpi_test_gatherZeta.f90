!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eee_mpi_module �ƥ��ȥץ����
!
!����  2008/05/21  �ݹ�����
!
program eee_mpi_test_gatherZeta

  use dc_message, only : MessageNotify
  use eee_mpi_module
  use eee_module
  implicit none
  include 'mpif.h'

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=64, km=16          ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=5           ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8), allocatable :: zxv_VorX(:,:,:)    ! �ʻҥǡ���(���� X)
  real(8), allocatable :: zxv_VorY(:,:,:)    ! �ʻҥǡ���(���� Y)
  real(8), allocatable :: zxv_VorZ(:,:,:)    ! �ʻҥǡ���(���� Z)
  real(8), allocatable :: eef_VorX(:,:,:)    ! ���ڥ��ȥ�ǡ���
  real(8), allocatable :: eef_VorY(:,:,:)    ! ���ڥ��ȥ�ǡ���
  real(8), allocatable :: eef_VorZ(:,:,:)    ! ���ڥ��ȥ�ǡ���

  real(8), allocatable :: ee2f_Zeta(:,:,:,:) ! ���ڥ��ȥ�ǡ���x2

  real(8), allocatable :: zyx_VorX(:,:,:)    ! �ʻҥǡ���(���� X)
  real(8), allocatable :: zyx_VorY(:,:,:)    ! �ʻҥǡ���(���� Y)
  real(8), allocatable :: zyx_VorZ(:,:,:)    ! �ʻҥǡ���(���� Z)
  real(8), allocatable :: eee_VorX(:,:,:)    ! ���ڥ��ȥ�ǡ���
  real(8), allocatable :: eee_VorY(:,:,:)    ! ���ڥ��ȥ�ǡ���
  real(8), allocatable :: eee_VorZ(:,:,:)    ! ���ڥ��ȥ�ǡ���
  real(8), allocatable :: eee2_Zeta(:,:,:,:) ! ���ڥ��ȥ�ǡ���x2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  integer            :: np, ip, ierr
  integer :: l=2, m=1, n
  integer :: ll, k

  call MessageNotify('M','eee_mpi_test_ZetaVelVor', &
       'eee_mpi_module Zeta-Vor,Vel transform functions tests')

 !---------------- MPI �������� ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

 !---------------- ��ɸ�ͤ����� ---------------------
  call eee_mpi_initial(im,jm,km,lm,mm,nm)
  call eee_initial(im,jm,km,lm,mm,nm)

 !---------------- �ѿ��γ��դ� ---------------------
  allocate(zxv_VorX(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VorY(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VorZ(0:km-1,0:im-1,js(ip):je(ip)))

  allocate(eef_VorX(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VorY(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VorZ(-nm:nm,-mm:mm,2*lc(ip)))

  allocate(ee2f_Zeta(-nm:nm,-mm:mm,2,2*lc(ip)))

  allocate(zyx_VorX(0:km-1,0:jm-1,0:im-1))
  allocate(zyx_VorY(0:km-1,0:jm-1,0:im-1))
  allocate(zyx_VorZ(0:km-1,0:jm-1,0:im-1))
  allocate(eee_VorX(-nm:nm,-mm:mm,-lm:lm))
  allocate(eee_VorY(-nm:nm,-mm:mm,-lm:lm))
  allocate(eee_VorZ(-nm:nm,-mm:mm,-lm:lm))

  allocate(eee2_Zeta(-nm:nm,-mm:mm,-lm:lm,2))

 !---------------- ®�١����٤����� ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  zxv_VorX = (m-n)*sin(l*zxv_X)*cos(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorY = (n-l)*cos(l*zxv_X)*sin(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorZ = (l-m)*cos(l*zxv_X)*cos(m*zxv_Y)*sin(n*zxv_Z)

  eef_VorX = eef_zxv(zxv_VorX)
  eef_VorY = eef_zxv(zxv_VorY)
  eef_VorZ = eef_zxv(zxv_VorZ)

  zyx_VorX = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX = eee_zyx(zyx_VorX)
  eee_VorY = eee_zyx(zyx_VorY)
  eee_VorZ = eee_zyx(zyx_VorZ)

 !---------------- ���� �� �ؤ��Ѵ� ---------------------
  ee2f_Zeta = ee2f_ZetaFromVor_eef_eef_eef(eef_VorX,eef_VorY,eef_VorZ)

  eee2_Zeta = eee2_ee2f(ee2f_Zeta)

 !---------------- ���� �� ����ε��Ѵ�(����) ---------------------
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,1)-eee_VorX, eps, &
       'Transform and Inverse Transform of VorX')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,2)-eee_VorY, eps, &
       'Transform and Inverse Transform of VorY')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,3)-eee_VorZ, eps, &
       'Transform and Inverse Transform of VorZ')

  call check4d(eee2_ZetaFromVor_eee_eee_eee(eee_VorX,eee_VorY,eee_VorZ)-eee2_Zeta, eps, &
       'Gathering Transform of Zeta')

  ee2f_Zeta = ee2f_eee2(eee2_Zeta)

  call check4d(ee2f_eee2(eee2_Zeta)-ee2f_Zeta, eps, &
       'gathering and scatering Transform of Zeta')

  call MessageNotify('M','eee_mpi_test_gatherZeta', &
       'eee_mpi_module Zeta gathering functions tests')

  call MPI_FINALIZE(IERR)

 stop
contains

  subroutine check4d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:,:,:)            ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname
    character(len=3) ::cip
    integer i, j, k, l

    if ( present(funcname) )then
       write(cip,'(I3)') IP
       write(6,*) '  Checking ', funcname, ' for IP='//trim(adjustl(cip))
    endif

    do l=1,size(var,4)
       do k=1,size(var,3)
          do j=1,size(var,2)
             do i=1,size(var,1)
                if (abs(var(i,j,k,l)) .gt. eps ) then
                   write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, &
                  '  k= ', k, '  l= ', l, &
                  var(i,j,k,l)
                   call MessageNotify('E','eee_mpi_test_transform', &
                        'transform error too large')
                endif
             enddo
          enddo
       enddo
    enddo
  end subroutine check4d

  subroutine check3d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:,:)              ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname
    character(len=3) ::cip
    integer i, j, k

    if ( present(funcname) )then
       write(cip,'(I3)') IP
       write(6,*) '  Checking ', funcname, ' for IP='//trim(adjustl(cip))
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k)
                call MessageNotify('E','eee_mpi_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_mpi_test_gatherZeta
