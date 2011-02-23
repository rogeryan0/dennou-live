!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eee_mpi_module �ƥ��ȥץ����
!
!����  2008/05/21  �ݹ�����
!
program eee_mpi_test_EnergyHelicity

  use dc_message, only : MessageNotify
  use eee_mpi_module
  implicit none
  include 'mpif.h'

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=64, km=32          ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=10          ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8), allocatable :: zxv_VelX(:,:,:)    ! �ʻҥǡ���(®�� X)
  real(8), allocatable :: zxv_VelY(:,:,:)    ! �ʻҥǡ���(®�� Y)
  real(8), allocatable :: zxv_VelZ(:,:,:)    ! �ʻҥǡ���(®�� Z)

  real(8), allocatable :: zxv_VorX(:,:,:)    ! �ʻҥǡ���(���� X)
  real(8), allocatable :: zxv_VorY(:,:,:)    ! �ʻҥǡ���(���� Y)
  real(8), allocatable :: zxv_VorZ(:,:,:)    ! �ʻҥǡ���(���� Z)

  real(8), allocatable :: eef_VorX(:,:,:)    ! ���ڥ��ȥ�ǡ���
  real(8), allocatable :: eef_VorY(:,:,:)    ! ���ڥ��ȥ�ǡ���
  real(8), allocatable :: eef_VorZ(:,:,:)    ! ���ڥ��ȥ�ǡ���

  real(8), allocatable :: ee2f_Zeta(:,:,:,:) ! ���ڥ��ȥ�ǡ���x2

  real(8)            :: Energy, Helicity
  real(8)            :: EnergyHelicity(2)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  integer            :: np, ip, ierr
  integer :: l=2, m=1, n

  call MessageNotify('M','eee_mpi_test_EnergyHelicity', &
       'eee_mpi_module Energy and Helicity calculation tests')


 !---------------- MPI �������� ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

 !---------------- ��ɸ�ͤ����� ---------------------
  call eee_mpi_initial(im,jm,km,lm,mm,nm)

 !---------------- �ѿ��γ��դ� ---------------------
  allocate(zxv_VelX(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VelY(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VelZ(0:km-1,0:im-1,js(ip):je(ip)))

  allocate(zxv_VorX(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VorY(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VorZ(0:km-1,0:im-1,js(ip):je(ip)))

  allocate(eef_VorX(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VorY(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VorZ(-nm:nm,-mm:mm,2*lc(ip)))

  allocate(ee2f_Zeta(-nm:nm,-mm:mm,2,2*lc(ip)))

 !---------------- ®�١����٤����� ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  zxv_VelX = cos(l*zxv_X)*sin(m*zxv_Y)*sin(n*zxv_Z)
  zxv_VelY = sin(l*zxv_X)*cos(m*zxv_Y)*sin(n*zxv_Z)
  zxv_VelZ = sin(l*zxv_X)*sin(m*zxv_Y)*cos(n*zxv_Z)

  zxv_VorX = (m-n)*sin(l*zxv_X)*cos(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorY = (n-l)*cos(l*zxv_X)*sin(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorZ = (l-m)*cos(l*zxv_X)*cos(m*zxv_Y)*sin(n*zxv_Z)

  eef_VorX = eef_zxv(zxv_VorX)
  eef_VorY = eef_zxv(zxv_VorY)
  eef_VorZ = eef_zxv(zxv_VorZ)

 !------------- ���� �� �ؤ��Ѵ��� Energy Helicity �׻� -----------------
  ee2f_Zeta = ee2f_ZetaFromVor_eef_eef_eef(eef_VorX,eef_VorY,eef_VorZ)
  EnergyHelicity = EnergyHelicityFromZeta_ee2f(ee2f_Zeta)

 !---------------- �ʻ����� Energy, Helicity �η׻� ---------------------
  
  Energy = 0.5 * AvrZXV_zxv(zxv_VelX**2+zxv_VelY**2+zxv_VelZ**2)

  Helicity = AvrZXV_zxv(zxv_VelX**zxv_VorX +zxv_VelY**zxv_VorY + zxv_VelZ**zxv_VorZ)
  

 !---------------- �׻������å� ---------------------
  call check0d(EnergyHelicity(1)-Energy, eps, 'Calculation of Energy')
  call check0d(EnergyHelicity(2)-Helicity, eps, 'Calculation of Helicity')

  call MessageNotify('M','eee_mpi_test_EnergyHelicity', &
       'eee_mpi_module Energy Helicity calulcation tests suceeded')

  call MPI_FINALIZE(IERR)

 stop
contains

  subroutine check0d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var                     ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    if (abs(var) .gt. eps ) then
       write(6,*) '    Value larger than EPS : ', var
       call MessageNotify('E','eee_test_EnergyHelicity', &
                          'calculation error too large')
    end if

  end subroutine check0d


end program eee_mpi_test_EnergyHelicity
