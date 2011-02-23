!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!  wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz  �Υƥ���
!    r��(��xv), r��(��x��xv)
!  
program wttest6

  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=0.5, ro=1.5      ! �⳰Ⱦ��
  integer,parameter  :: np=8                ! OPENMP ���祹��åɿ�

  real(8), dimension(im,jm,0:km)     :: xyz_VLon
  real(8), dimension(im,jm,0:km)     :: xyz_VLat
  real(8), dimension(im,jm,0:km)     :: xyz_VRad
  real(8), dimension(im,jm,0:km)     :: xyz_Data
  real(8), dimension(im,jm,0:km)     :: xyz_RadRot
  real(8), dimension(im,jm,0:km)     :: xyz_RadRotRot
  real(8), dimension((nm+1)**2,0:lm) :: wt_Psi
  real(8), dimension((nm+1)**2,0:km) :: yg_Psi
  real(8), dimension(im,jm,0:km)     :: xyz_Psi

  real(8), parameter :: eps = 1D-8

  integer :: i,j,k

  write( 6,* ) 'Test for wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_initial(im,jm,km,nm,lm,ri,ro,np)

  xyz_VLon = 0 ; xyz_VLat = 0 ; xyz_VRad = xyz_Rad
  xyz_RadRot = 0 ; xyz_RadRotRot = 0
  write(6,*)
  write(6,*)'Simple example (v_r=r)'
  call checkresult

! ���β�ž����������(����ή��)
  xyz_VLon = 0
  xyz_VLat = xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)
  xyz_VRad = -xyz_Rad*cos(xyz_Lat)**2
  xyz_RadRot = 0 ; xyz_RadRotRot = 0
  write(6,*)
  write(6,*)'Rigid Rotation '
  call checkresult

! ���β�ž����������(����ή��)
  xyz_VLon = xyz_Rad*cos(xyz_Lat)*sin(xyz_Lon)*cos(xyz_Lon)
  xyz_VLat = -xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)*sin(xyz_Lon)**2
  xyz_VRad = -xyz_Rad*(sin(xyz_Lat)**2*sin(xyz_Lon)**2 + cos(xyz_Lon)**2)
  xyz_RadRot = 0 ; xyz_RadRotRot = 0

  write(6,*)
  write(6,*)'Rigid Rotation'
  call checkresult

! ��ľ���٤�ȼ���٥��ȥ��
  xyz_Psi = xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lon)   ! r**2 P_1^1
  !xyz_Psi = xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lat)*sin(xyz_Lon)   ! r**2 P_2^1

  xyz_VLon =   xyz_gRadLat_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VLat = - xyz_gRadLon_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VRad = 0
  xyz_RadRot = 2 * xyz_Psi                           ! r�����ߢ���(��r) = L_2��
  !xyz_RadRot = 6 * xyz_Psi                           ! r�����ߢ���(��r) = L_2��
  xyz_RadRotRot = 0

  write(6,*)
  write(6,*)'Vortical field'
  call checkresult

! ��ľ®�٤�ȼ���٥��ȥ��
  xyz_VRad = xyz_wt(wt_l2_wt(wt_xyz(xyz_Psi/xyz_Rad)))
  xyz_VLat = xyz_gRadLat_wt(wt_dRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))
  xyz_VLon = xyz_gRadLon_wt(wt_dRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))

  xyz_RadRot = 0
  xyz_RadRotRot = -xyz_wt(wt_l2_wt(wt_lapla_wt(wt_xyz(xyz_Psi))))
                 ! r�����ߢ��ߢ��ߢ���(��r) = -L_2��^2��
  write(6,*)
  write(6,*)'Non-Vortical field'
  call checkresult

  stop
contains

  subroutine checkresult
    write(6,*)'Checking r Rot v...'

    xyz_Data = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat))

    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_Data(i,j,k)-xyz_RadRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_RadRot(i,j,k)
             endif
          end do
       end do
    end do

    xyz_Data = xyz_wt(wt_RadRotRot_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad))

    write(6,*)'Checking r Rot Rot v...'
    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_Data(i,j,k)-xyz_RadRotRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_RadRotRot(i,j,k)
             endif
          end do
       end do
    end do
  end subroutine checkresult
end program wttest6
