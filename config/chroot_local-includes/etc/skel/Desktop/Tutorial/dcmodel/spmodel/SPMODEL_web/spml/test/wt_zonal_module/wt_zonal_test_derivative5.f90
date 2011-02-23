!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_zonal_module �ƥ��ȥץ������
!
!  wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz  �Υƥ���
!    r��(��xv), r��(��x��xv)
!
!����  2008/12/30  �ݹ�����
!  
program wt_zonal_test_derivative5

  use dc_message, only : MessageNotify
  use wt_zonal_module
  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16        ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=0.5, ro=1.5      ! �⳰Ⱦ��

  real(8), dimension(0:im-1,jm,0:km)     :: xyz_VLon
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_VLat
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_VRad
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_Data
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_RadRot
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_RadRotRot
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_Psi

  real(8), parameter :: eps = 1D-8

  integer :: i,j,k

  call MessageNotify('M','wt_zonal_test_derivative5', &
       'wt_zonal_module derivative function test #5')

  write( 6,* ) 'Test for wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_initial(im,jm,km,nm,lm,ri,ro)

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

! ��ľ���٤�ȼ���٥��ȥ��
  xyz_Psi = xyz_Rad**2 * sin(xyz_Lat)   ! r**2 P_1^0

  xyz_VLon =   xyz_GradLat_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VLat = - xyz_GradLon_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VRad = 0
  xyz_RadRot = 2 * xyz_Psi                           ! r�����ߢ���(��r) = L_2��
  xyz_RadRotRot = 0

  write(6,*)
  write(6,*)'Vortical field'
  call checkresult

! ��ľ®�٤�ȼ���٥��ȥ��
  xyz_VRad = xyz_wt(wt_L2_wt(wt_xyz(xyz_Psi/xyz_Rad)))
  xyz_VLat = xyz_GradLat_wt(wt_DRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))
  xyz_VLon = xyz_GradLon_wt(wt_DRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))

  xyz_RadRot = 0
  xyz_RadRotRot = -xyz_wt(wt_l2_wt(wt_Lapla_wt(wt_xyz(xyz_Psi))))
                 ! r�����ߢ��ߢ��ߢ���(��r) = -L_2��^2��
  write(6,*)
  write(6,*)'Non-Vortical field'
  call checkresult

  call MessageNotify('M','wt_zonal_test_derivative5', &
       'wt_zonal_module derivative function test #5 succeeded!')

  stop
contains

  subroutine checkresult
    write(6,*)'Checking r Rot v...'

    xyz_Data = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_RadRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_RadRot(i,j,k)
                call MessageNotify('E','wt_test_derivative5', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyz_Data = xyz_wt(wt_RadRotRot_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad))

    write(6,*)'Checking r Rot Rot v...'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_RadRotRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_RadRotRot(i,j,k)
                call MessageNotify('E','wt_test_derivative5', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult

end program wt_zonal_test_derivative5
