!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_module �ƥ��ȥץ����
!
!     ���֥롼����Υƥ���
!       wt_Potential2Rotation
!
!����  2008/01/13  �ݹ�����  wt_test_derivative7.f90 ������
!      2008/07/05  ��������ʿ  �����������ѹ�
!
program wtu_test_derivative7

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=16, lmi=8       ! �����ȿ�������(���ư��, ��ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  real(8), dimension((nm+1)*(nm+1),0:lmo) :: wt_VTor  ! �ȥ�����ݥƥ󥷥��
  real(8), dimension((nm+1)*(nm+1),0:lmo) :: wt_VPol  ! �ݥ�����ݥƥ󥷥��

  real(8), dimension(0:im-1,1:jm,0:kmo)   :: xyz_RotVLon  ! ������(����)
  real(8), dimension(0:im-1,1:jm,0:kmo)   :: xyz_RotVLat  ! ������(����)
  real(8), dimension(0:im-1,1:jm,0:kmo)   :: xyz_RotVrad  ! ������(ư��)
  real(8), dimension(0:im-1,1:jm,0:kmo)   :: xyz_RotV0Lon  ! ������(����, ����)
  real(8), dimension(0:im-1,1:jm,0:kmo)   :: xyz_RotV0Lat  ! ������(����, ����)
  real(8), dimension(0:im-1,1:jm,0:kmo)   :: xyz_RotV0rad  ! ������(����,ư��)

  real(8), parameter :: eps = 1D-10

  integer :: i,j,k

  call MessageNotify('M','wtu_test_derivative7', &
       'wtu_module derivative subroutine test #7')

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  write( 6,* ) 'Test for wt_Rotation'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- �� 1 --------------------
  ! ���β�ž��
  wt_VTor = wt_xyz(xyz_Rad * sin(xyz_Lat))
  wt_VPol = 0.0D0

  ! xyz_Vlon = xyz_Rad * cos(xyz_Lat)'
  ! xyz_Vlat = 0.0'
  ! xyz_VRad = 0.0'

  xyz_RotV0lon = 0.0D0
  xyz_RotV0lat = 2*cos(xyz_LAT)
  xyz_RotV0rad = 2*sin(xyz_LAT)

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xyz_Vlon = xyz_Rad * cos(xyz_Lat)'
  write(6,*)'    xyz_Vlat = 0.0'
  write(6,*)'    xyz_VRad = 0.0'

  call checkresult

! ----------------- �� 2 --------------------
  ! ���β�ž��(����ή)
  wt_VTor = wt_xyz(xyz_Rad * cos(xyz_Lat) * sin(xyz_Lon))
  wt_VPol = 0.0D0

  !  xyz_VLon = -xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)
  !  xyz_VLat = -xyz_Rad*cos(xyz_Lon)
  !  xyz_VRad = 0.0

  xyz_RotV0Lon = 2*cos(xyz_Lon)
  xyz_RotV0Lat = -2*sin(xyz_Lon)*sin(xyz_Lat)
  xyz_RotV0Rad = 2*sin(xyz_Lon)*cos(xyz_Lat)

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : rigid rotation'
  write(6,*)'    xyz_Vlon=xyz_Rad*xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)'
  write(6,*)'    xyz_Vlat=-xyz_Rad*cos(xyz_Lon)'
  write(6,*)'    xyz_VRad = 0.0'

  call checkresult

! ----------------- �� 3 --------------------
 ! ��̵����

  wt_VTor = 0.0D0
  wt_VPol = wt_xyz(xyz_Rad * sin(xyz_Lat))

  ! xyz_Vlon = 0
  ! xyz_Vlat = 2 * xyz_Rad * cos(xyz_Lat)
  ! xyz_Vrad = 2 * xyz_Rad * sin(xyz_Lat)

  xyz_RotV0lon = 0.0D0
  xyz_RotV0lat = 0.0D0
  xyz_RotV0rad = 0.0D0

  write(6,*)
  write(6,*)
  write(6,*)'Example 3 : no rotation'
  write(6,*)'    xyz_Vlon = 0.0'
  write(6,*)'    xyz_Vlat = 0.0'
  write(6,*)'    xyz_VRad = xyz_Rad*2 * cos(xyz_Lat)'

  call checkresult

! ----------------- �� 4 --------------------
 ! �ݥ�����®�پ�

  wt_VTor = 0.0D0
  wt_VPol = wt_xyz(xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lon))

  ! xyz_Vlon = 3 * xyz_Rad * cos(xyz_Lon)
  ! xyz_Vlat = 3 * xyz_Rad * sin(xyz_Lat) * sin(xyz_Lon)
  ! xyz_Vrad = 2 * xyz_Rad * sin(xyz_Lat) * cos(xyz_Lon)

  xyz_RotV0lon = 4*sin(xyz_Lat)*sin(xyz_Lon)
  xyz_RotV0lat = 4*cos(xyz_Lon)
  xyz_RotV0rad = 0.0D0

  write(6,*)
  write(6,*)
  write(6,*)'Example 4 : poloidal field'
  write(6,*)'    xyz_Vlon = 3 * xyz_Rad * cos(xyz_Lon)'
  write(6,*)'    xyz_Vlat = 3 * xyz_Rad * sin(xyz_Lat) * sin(xyz_Lon)'
  write(6,*)'    xyz_VRad = 2 * xyz_Rad * sin(xyz_Lat) * cos(xyz_Lon)'

  call checkresult

  call MessageNotify('M','wtu_test_derivative7', &
       'wt_module derivative function test #7 succeeded!')

  stop
contains

 !------- ������ -------
  subroutine checkresult

    call wt_Potential2Rotation(&
         xyz_RotVLon,xyz_RotVLat,xyz_RotVRad, wt_VTor, wt_VPol )

    write(6,*)
    write(6,*)'Checking RotV_Lon '
    do k=0,kmo
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_RotVLon(i,j,k)-xyz_RotV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVLon(i,j,k),xyz_RotV0Lon(i,j,k)
                call MessageNotify('E','wtu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Lat '
    do k=0,kmo
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_RotVLat(i,j,k)-xyz_RotV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVLat(i,j,k),xyz_RotV0Lat(i,j,k)
                call MessageNotify('E','wtu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Rad '
    do k=0,kmo
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_RotVRad(i,j,k)-xyz_RotV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_RotVRad(i,j,k),xyz_RotV0rad(i,j,k)
                call MessageNotify('E','wtu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wtu_test_derivative7
