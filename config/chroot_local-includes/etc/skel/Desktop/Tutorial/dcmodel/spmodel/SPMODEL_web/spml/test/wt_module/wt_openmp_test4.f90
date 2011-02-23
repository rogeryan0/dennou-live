!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! �ؿ��Υƥ���
!    wt_DivLon_xyz, wt_DivLat_xyz
!  
program wttest4

  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=0.5, ro=1.5      ! �⳰Ⱦ��
  integer,parameter  :: np=8                ! OPENMP ���祹��åɿ�

  real(8), dimension(im,jm,0:km)     :: xyz_VLon
  real(8), dimension(im,jm,0:km)     :: xyz_VLat
  real(8), dimension(im,jm,0:km)     :: xyz_DivLon
  real(8), dimension(im,jm,0:km)     :: xyz_DivLat
  real(8), dimension(im,jm,0:km)     :: xyz_Data

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  call wt_Initial(im,jm,km,nm,lm,ri,ro,np)

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

  stop
contains

  subroutine checkresult

    write(6,*)'Checking DivLon (1/r cos(Lat) dLon)'
    xyz_Data =  xyz_wt(wt_DivLon_xyz(xyz_VLon))

    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_Data(i,j,k)-xyz_DivLon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_DivLon(i,j,k)
             endif
          end do
       end do
    end do

    xyz_Data =  xyz_wt(wt_DivLat_xyz(xyz_VLat))
    write(6,*)'Checking DivLat (1/r cos(Lat) dLat cos(Lat))'
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_Data(i,j,k)-xyz_DivLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_DivLat(i,j,k)
             endif
          end do
       end do
    end do
  end subroutine checkresult


end program wttest4

