!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wu_module �ƥ��ȥץ������
!
!     ���֥롼����Υƥ���
!       wu_Potential2Vector
!
!����  2008/01/01  �ݹ�����
!      2008/06/28  ��������ʿ  �����������ѹ�
!
program wu_test_derivative6

  use dc_message, only : MessageNotify
  use wu_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=32  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=32         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ra=7.0D0/13.0D0      ! �⳰Ⱦ�� \eta=0.35

  real(8), dimension((nm+1)*(nm+1),0:lm) :: wu_VTor    ! �ȥ�������ݥƥ󥷥��
  real(8), dimension((nm+1)*(nm+1),0:lm) :: wu_VPol    ! �ݥ�������ݥƥ󥷥��

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLon  ! ®��(����)
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLat  ! ®��(����)
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Vrad  ! ®��(ư��)
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_V0Lon  ! ®��(����, ����)
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_V0Lat  ! ®��(����, ����)
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_V0rad  ! ®��(����,ư��)

  real(8), parameter :: eps = 1D-12

  integer :: i,j,k

  call MessageNotify('M','wu_test_derivative7', &
       'wu_module derivative subroutine test #7')

  call wu_Initial(im,jm,km,nm,lm,ra)

  write( 6,* ) 'Test for wu_Potential2Vector'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- �� 1 --------------------
  ! ���β�ž��
  wu_VTor = wu_xyr(xyr_Rad * sin(xyr_Lat))
  wu_VPol = 0.0D0

  xyr_V0lon = xyr_Rad * cos(xyr_Lat)
  xyr_V0lat = 0.0D0
  xyr_V0Rad = 0.0D0

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xyr_Vlon = xyr_Rad * cos(xyr_Lat)'
  write(6,*)'    xyr_Vlat = 0.0'
  write(6,*)'    xyr_VRad = 0.0'

  call checkresult

! ----------------- �� 2 --------------------
  ! ���β�ž��(����ή)
  wu_VTor = wu_xyr(xyr_Rad * cos(xyr_Lat) * sin(xyr_Lon))
  wu_VPol = 0.0D0

  xyr_V0Lon = -xyr_Rad*sin(xyr_Lat)*sin(xyr_Lon)
  xyr_V0Lat = -xyr_Rad*cos(xyr_Lon)
  xyr_V0Rad = 0.0D0

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
  wu_VPol = wu_xyr(xyr_Rad**3 * sin(xyr_Lat))

  xyr_V0lon = 0D0
  xyr_V0lat = 4 * xyr_Rad**2 * cos(xyr_Lat)
  xyr_V0rad = 2 * xyr_Rad**2 * sin(xyr_Lat)

  write(6,*)
  write(6,*)
  write(6,*)'Example 3 : no rotation'
  write(6,*)'    xyr_Vlon = 0.0'
  write(6,*)'    xyr_Vlat = 4 * xyr_Rad**2 * cos(xyr_Lat)'
  write(6,*)'    xyr_VRad = 2 * xyr_Rad**2 * sin(xyr_Lat)'

  call checkresult


! ----------------- �� 4 --------------------
 ! �ݥ�������®�پ�

  wu_VTor = 0.0D0
  wu_VPol = wu_xyr(xyr_Rad**5 * cos(xyr_Lat)*sin(xyr_Lon))

  xyr_V0lon = 6 * xyr_Rad**4 * cos(xyr_Lon)
  xyr_V0lat = - 6 * xyr_Rad**4 * sin(xyr_Lat) * sin(xyr_Lon)
  xyr_V0rad = 2 * xyr_Rad**4 * cos(xyr_Lat) * sin(xyr_Lon)

  write(6,*)
  write(6,*)
  write(6,*)'Example 4 : poloidal field'
  write(6,*)'    xyr_Vlon =  6 * xyr_Rad**4 * cos(xyr_Lon)'
  write(6,*)'    xyr_Vlat = - 6 * xyr_Rad**4 * sin(xyr_Lat) * sin(xyr_Lon)'
  write(6,*)'    xyr_VRad = 2 * xyr_Rad**4 * cos(xyr_Lat) * sin(xyr_Lon)'

  call checkresult

  call MessageNotify('M','wu_test_derivative6', &
       'wu_module derivative function test #6 succeeded!')

  stop
contains

 !------- ������ -------
  subroutine checkresult

    call wu_Potential2Vector(&
         xyr_VLon,xyr_VLat,xyr_VRad, wu_VTor, wu_VPol )

    write(6,*)
    write(6,*)'Checking V_Lon '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_VLon(i,j,k)-xyr_V0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_VLon(i,j,k),xyr_V0Lon(i,j,k),&
                                  xyr_VLon(i,j,k)-xyr_V0Lon(i,j,k)
                call MessageNotify('E','wu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking V_Lat '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_VLat(i,j,k)-xyr_V0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_VLat(i,j,k),xyr_V0Lat(i,j,k), &
                                  xyr_VLat(i,j,k)-xyr_V0Lat(i,j,k)
                call MessageNotify('E','wu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking V_Rad '
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_VRad(i,j,k)-xyr_V0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_VRad(i,j,k),xyr_V0rad(i,j,k),&
                                  xyr_VRad(i,j,k)-xyr_V0rad(i,j,k)
                call MessageNotify('E','wu_test_derivative7', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wu_test_derivative6