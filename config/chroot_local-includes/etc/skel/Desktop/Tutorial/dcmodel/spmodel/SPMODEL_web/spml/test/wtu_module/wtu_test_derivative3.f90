!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wtu_module �ƥ��ȥץ����
!
!      �ؿ��Υƥ���
!           wt_DivLon_xyz, wt_DivLat_xyz
!
!����  2008/01/13  �ݹ�����  wt_test_derivative3.f90 ������
!      2008/07/05  ��������ʿ  �����������ѹ�
!  
program wtu_test_derivative3

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=16, lmi=8        ! �����ȿ�������(���ư��, ��ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_VLon
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_VLat
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_DivLon
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_DivLat
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_Data

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  call MessageNotify('M','wtu_test_derivative3', &
       'wtu_module derivative function test #3')

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  write( 6,* ) 'Test for wt_DivLon_xyz, wt_DivLat_xyz'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- �� 1 --------------------
  xyz_VLon   = xyz_rad**n * cos(xyz_Lat)**2*sin(xyz_Lon)
  xyz_DivLon = xyz_rad**(n-1)*cos(xyz_Lat)*cos(xyz_Lon)

  xyz_VLat   = xyz_rad**n * cos(xyz_Lat)**2*sin(xyz_Lon)
  xyz_DivLat = -3*xyz_rad**(n-1)*cos(xyz_Lat)*sin(xyz_Lat)*sin(xyz_Lon)

  write(6,*)
  write(6,*)'Example 1'
  write(6,*)'  xyz_VLon=xyz_rad**n * cos(xyz_Lat)**2*sin(xyz_Lon)'
  write(6,*)'  xyz_VLat=xyz_rad**n * cos(xyz_Lat)**2*sin(xyz_Lon)'
  call checkresult

  call MessageNotify('M','wt_test_derivative3', &
       'wt_module derivative function test #3 succeeded!')

  stop
contains

  subroutine checkresult

    write(6,*)'Checking DivLon (1/r cos(Lat) dLon)'
    xyz_Data =  xyz_wt(wt_DivLon_xyz(xyz_VLon))

    do k=0,kmo
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_DivLon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_DivLon(i,j,k)
                call MessageNotify('E','wt_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyz_Data =  xyz_wt(wt_DivLat_xyz(xyz_VLat))
    write(6,*)'Checking DivLat (1/r cos(Lat) dLat cos(Lat))'
    do k=0,kmo
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_DivLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_DivLat(i,j,k)
                call MessageNotify('E','wt_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult


end program wtu_test_derivative3

