!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_mpi_module �ƥ��ȥץ����
!
!     ���֥롼����Υƥ���
!       wt_Potential2Rotation
!
!����  2008/05/30  �ݹ�����
!
program wt_mpi_derivative_test7

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=7.0D0/13.0D0      ! �⳰Ⱦ�� \eta=0.35
  real(8),parameter  :: ro=20.0D0/13.0D0     ! �⳰Ⱦ�� \eta=0.35

  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VTor    ! �ȥ�����ݥƥ󥷥��
  real(8), dimension((nm+1)*(nm+1),0:lm) :: wt_VPol    ! �ݥ�����ݥƥ󥷥��

  real(8), allocatable  :: xvz_RotVLon(:,:,:)   ! ������(����)
  real(8), allocatable  :: xvz_RotVLat(:,:,:)   ! ������(����)
  real(8), allocatable  :: xvz_RotVrad(:,:,:)   ! ������(ư��)
  real(8), allocatable  :: xvz_RotV0Lon(:,:,:)  ! ������(����, ����)
  real(8), allocatable  :: xvz_RotV0Lat(:,:,:)  ! ������(����, ����)
  real(8), allocatable  :: xvz_RotV0rad(:,:,:)  ! ������(����,ư��)

  real(8), parameter :: eps = 1D-10

  integer :: i,j,k

  integer :: iproc, np, ierr

 !---------------- MPI �������� ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test7', &
       'wt_mpi_module derivative subroutine test #7')

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_RotVLon(0:im-1,jc,0:km))   ! ������(����)
  allocate(xvz_RotVLat(0:im-1,jc,0:km))   ! ������(����)
  allocate(xvz_RotVrad(0:im-1,jc,0:km))   ! ������(ư��)
  allocate(xvz_RotV0Lon(0:im-1,jc,0:km))  ! ������(����, ����)
  allocate(xvz_RotV0Lat(0:im-1,jc,0:km))  ! ������(����, ����)
  allocate(xvz_RotV0rad(0:im-1,jc,0:km))  ! ������(����,ư��)

  write( 6,* ) 'Test for wt_Rotation'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- �� 1 --------------------
  ! ���β�ž��
  wt_VTor = wt_xvz(xvz_Rad * sin(xvz_Lat))
  wt_VPol = 0.0

  ! xvz_Vlon = xvz_Rad * cos(xvz_Lat)'
  ! xvz_Vlat = 0.0'
  ! xvz_VRad = 0.0'

  xvz_RotV0lon = 0.0
  xvz_RotV0lat = 2*cos(xvz_LAT)
  xvz_RotV0rad = 2*sin(xvz_LAT)

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xvz_Vlon = xvz_Rad * cos(xvz_Lat)'
  write(6,*)'    xvz_Vlat = 0.0'
  write(6,*)'    xvz_VRad = 0.0'

  call checkresult

! ----------------- �� 2 --------------------
  ! ���β�ž��(����ή)
  wt_VTor = wt_xvz(xvz_Rad * cos(xvz_Lat) * sin(xvz_Lon))
  wt_VPol = 0.0

  !  xvz_VLon = -xvz_Rad*sin(xvz_Lat)*sin(xvz_Lon)
  !  xvz_VLat = -xvz_Rad*cos(xvz_Lon)
  !  xvz_VRad = 0.0

  xvz_RotV0Lon = 2*cos(xvz_Lon)
  xvz_RotV0Lat = -2*sin(xvz_Lon)*sin(xvz_Lat)
  xvz_RotV0Rad = 2*sin(xvz_Lon)*cos(xvz_Lat)

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : rigid rotation'
  write(6,*)'    xvz_Vlon=xvz_Rad*xvz_Rad*sin(xvz_Lat)*sin(xvz_Lon)'
  write(6,*)'    xvz_Vlat=-xvz_Rad*cos(xvz_Lon)'
  write(6,*)'    xvz_VRad = 0.0'

  call checkresult

! ----------------- �� 3 --------------------
 ! ��̵����

  wt_VTor = 0.0
  wt_VPol = wt_xvz(xvz_Rad * sin(xvz_Lat))

  ! xvz_Vlon = 0
  ! xvz_Vlat = 2 * xvz_Rad * cos(xvz_Lat)
  ! xvz_Vrad = 2 * xvz_Rad * sin(xvz_Lat)

  xvz_RotV0lon = 0.0
  xvz_RotV0lat = 0.0
  xvz_RotV0rad = 0.0

  write(6,*)
  write(6,*)
  write(6,*)'Example 3 : no rotation'
  write(6,*)'    xvz_Vlon = 0.0'
  write(6,*)'    xvz_Vlat = 0.0'
  write(6,*)'    xvz_VRad = xvz_Rad*2 * cos(xvz_Lat)'

  call checkresult

! ----------------- �� 4 --------------------
 ! �ݥ�����®�پ�

  wt_VTor = 0.0
  wt_VPol = wt_xvz(xvz_Rad**2 * cos(xvz_Lat)*sin(xvz_Lon))

  ! xvz_Vlon = 3 * xvz_Rad * cos(xvz_Lon)
  ! xvz_Vlat = 3 * xvz_Rad * sin(xvz_Lat) * sin(xvz_Lon)
  ! xvz_Vrad = 2 * xvz_Rad * sin(xvz_Lat) * cos(xvz_Lon)

  xvz_RotV0lon = 4*sin(xvz_Lat)*sin(xvz_Lon)
  xvz_RotV0lat = 4*cos(xvz_Lon)
  xvz_RotV0rad = 0.0

  write(6,*)
  write(6,*)
  write(6,*)'Example 4 : poloidal field'
  write(6,*)'    xvz_Vlon = 3 * xvz_Rad * cos(xvz_Lon)'
  write(6,*)'    xvz_Vlat = 3 * xvz_Rad * sin(xvz_Lat) * sin(xvz_Lon)'
  write(6,*)'    xvz_VRad = 2 * xvz_Rad * sin(xvz_Lat) * cos(xvz_Lon)'

  call checkresult

  call MessageNotify('M','wt_mpi_derivative_test7', &
       'wt_module derivative function test #7 succeeded!')

 !------ MPI�ν�λ ------

  call MPI_FINALIZE(IERR)

  stop
contains

 !------- ������ -------
  subroutine checkresult

    call wt_Potential2RotationMPI(&
         xvz_RotVLon,xvz_RotVLat,xvz_RotVRad, wt_VTor, wt_VPol )

    write(6,*)
    write(6,*)'Checking RotV_Lon '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_RotVLon(i,j,k)-xvz_RotV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_RotVLon(i,j,k),xvz_RotV0Lon(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Lat '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_RotVLat(i,j,k)-xvz_RotV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_RotVLat(i,j,k),xvz_RotV0Lat(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Rad '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_RotVRad(i,j,k)-xvz_RotV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_RotVRad(i,j,k),xvz_RotV0rad(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wt_mpi_derivative_test7

