!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wtu_module �ƥ��ȥץ������
!
!     ���֥롼����Υƥ���
!       wu_Potential2Rotation
!
!����  2008/01/13  �ݹ�����  wu_test_derivative7.f90 ������
!      2008/07/05  ��������ʿ  �����������ѹ�
!
program wtu_test_derivative17

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=16, lmi=8        ! �����ȿ�������(���ư��, ��ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  real(8), dimension((nm+1)*(nm+1),0:lmi) :: wu_VTor  ! �ȥ�������ݥƥ󥷥��
  real(8), dimension((nm+1)*(nm+1),0:lmi) :: wu_VPol  ! �ݥ�������ݥƥ󥷥��

  real(8), dimension(0:im-1,1:jm,0:kmi)   :: xyr_RotVLon  ! ������(����)
  real(8), dimension(0:im-1,1:jm,0:kmi)   :: xyr_RotVLat  ! ������(����)
  real(8), dimension(0:im-1,1:jm,0:kmi)   :: xyr_RotVrad  ! ������(ư��)
  real(8), dimension(0:im-1,1:jm,0:kmi)   :: xyr_RotV0Lon  ! ������(����, ����)
  real(8), dimension(0:im-1,1:jm,0:kmi)   :: xyr_RotV0Lat  ! ������(����, ����)
  real(8), dimension(0:im-1,1:jm,0:kmi)   :: xyr_RotV0rad  ! ������(����,ư��)

  real(8), parameter :: eps = 1D-9

  integer :: i,j,k

  call MessageNotify('M','wtu_test_derivative7', &
       'wu_module derivative subroutine test #7')

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  write( 6,* ) 'Test for wu_Rotation'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- �� 1 --------------------
  ! ���β�ž��
  wu_VTor = wu_xyr(xyr_Rad * sin(xyr_Lat))
  wu_VPol = 0.0D0

  ! xyr_Vlon = xyr_Rad * cos(xyr_Lat)'
  ! xyr_Vlat = 0.0'
  ! xyr_VRad = 0.0'

  xyr_RotV0lon = 0.0D0
  xyr_RotV0lat = 2*cos(xyr_LAT)
  xyr_RotV0rad = 2*sin(xyr_LAT)

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xyr_Vlon = xyr_Rad*cos(xyr_LAT)'
  write(6,*)'    xyr_Vlat = 0.0'
  write(6,*)'    xyr_VRad = 0.0'

  call checkresult

! ----------------- �� 2 --------------------
  ! ���β�ž��(����ή)
  wu_VTor = wu_xyr(xyr_Rad * cos(xyr_Lat) * sin(xyr_Lon))
  wu_VPol = 0.0D0

  !  xyr_VLon = -xyr_Rad*sin(xyr_Lat)*sin(xyr_Lon)
  !  xyr_VLat = -xyr_Rad*cos(xyr_Lon)
  !  xyr_VRad = 0.0

  xyr_RotV0Lon = 2*cos(xyr_Lon)
  xyr_RotV0Lat = -2*sin(xyr_Lon)*sin(xyr_Lat)
  xyr_RotV0Rad = 2*sin(xyr_Lon)*cos(xyr_Lat)

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : rigid rotation'
  write(6,*)'    xyr_Vlon=xyr_Rad*sin(xyr_Lat)*sin(xyr_Lon)'
  write(6,*)'    xyr_Vlat=-xyr_Rad*cos(xyr_Lon)'
  write(6,*)'    xyr_VRad = 0.0'

  call checkresult

! ----------------- �� 3 --------------------
 ! ��̵����

  wu_VTor = 0.0D0
  wu_VPol = wu_xyr(xyr_Rad * sin(xyr_Lat))

  ! xyr_Vlon = 0
  ! xyr_Vlat = 2 * cos(xyr_Lat)
  ! xyr_Vrad = 2 * sin(xyr_Lat)

  xyr_RotV0lon = 0.0D0
  xyr_RotV0lat = 0.0D0
  xyr_RotV0rad = 0.0D0

  write(6,*)
  write(6,*)
  write(6,*)'Example 3 : no rotation'
  write(6,*)'    xyr_Vlon = 0.0'
  write(6,*)'    xyr_Vlat = 2 * cos(xyr_Lat)'
  write(6,*)'    xyr_Vrad = 2 * sin(xyr_Lat)'

  call checkresult

! ----------------- �� 4 --------------------
 ! �ݥ�������®�پ�

  wu_VTor = 0.0D0
  wu_VPol = wu_xyr(xyr_Rad **3 * cos(xyr_Lat)*sin(xyr_Lon))

  ! xyr_Vlon = 4 * xyr_Rad**2 * cos(xyr_Lon)
  ! xyr_Vlat = -4 * xyr_Rad**2 * sin(xyr_Lat) * sin(xyr_Lon)
  ! xyr_Vrad = 2 * xyr_Rad**2  * cos(xyr_Lat) * sin(xyr_Lon)

  xyr_RotV0lon = 10*xyr_Rad*sin(xyr_Lat)*sin(xyr_Lon)
  xyr_RotV0lat = 10*xyr_Rad*cos(xyr_Lon)
  xyr_RotV0rad = 0.0D0

  write(6,*)
  write(6,*)
  write(6,*)'Example 4 : poloidal field'
  write(6,*)'    xyr_Vlon = 4 * xyr_Rad**2 * cos(xyr_Lon)'
  write(6,*)'    xyr_Vlat =-4 * xyr_Rad**2 * sin(xyr_Lat) * sin(xyr_Lon)'
  write(6,*)'    xyr_VRad = 2 * xyr_Rad**2 * cos(xyr_Lat) * sin(xyr_Lon)'

  call checkresult

  call MessageNotify('M','wtu_test_derivative7', &
       'wu_module derivative function test #7 succeeded!')

  stop
contains

 !------- ������ -------
  subroutine checkresult

    call wu_Potential2Rotation(&
         xyr_RotVLon,xyr_RotVLat,xyr_RotVRad, wu_VTor, wu_VPol )

    write(6,*)
    write(6,*)'Checking RotV_Lon '
    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_RotVLon(i,j,k)-xyr_RotV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_RotVLon(i,j,k),xyr_RotV0Lon(i,j,k), &
                                  xyr_RotVLon(i,j,k)-xyr_RotV0Lon(i,j,k)
                call MessageNotify('E','wtu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Lat '
    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_RotVLat(i,j,k)-xyr_RotV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_RotVLat(i,j,k),xyr_RotV0Lat(i,j,k),&
                                  xyr_RotVLat(i,j,k)-xyr_RotV0Lat(i,j,k)
                call MessageNotify('E','wtu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking RotV_Rad '
    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_RotVRad(i,j,k)-xyr_RotV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_RotVRad(i,j,k),xyr_RotV0rad(i,j,k),&
                                  xyr_RotVRad(i,j,k)-xyr_RotV0rad(i,j,k)
                call MessageNotify('E','wtu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wtu_test_derivative17
