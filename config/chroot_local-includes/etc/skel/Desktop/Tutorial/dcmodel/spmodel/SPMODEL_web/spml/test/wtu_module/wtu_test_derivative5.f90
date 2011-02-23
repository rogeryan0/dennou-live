!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wt_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!
!  wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz  ¤Î¥Æ¥¹¥È
!    r¡¦(¢¦xv), r¡¦(¢¦x¢¦xv)
!
!ÍúÎò  2008/01/13  ÃÝ¹­¿¿°ì  wt_test_derivative5.f90 ¤è¤ê²þÊÑ
!      2008/07/05  º´¡¹ÌÚÍÎÊ¿  ÇÛÎó¤ÎÀë¸À¤òÊÑ¹¹
!  
program wtu_test_derivative5

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: kmo=16, kmi=8        ! ³Ê»ÒÅÀ¤ÎÀßÄê(µå³ÌÆ°·Â, µåÆ°·Â)
  integer,parameter  :: nm=10                ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿)
  integer,parameter  :: lmo=16, lmi=8       ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(µå³ÌÆ°·Â, µåÆ°·Â)
  real(8),parameter  :: ri=0.5, ro=1.5       ! Æâ³°È¾·Â

  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_VLon
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_VLat
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_VRad
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_Data
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_RadRot
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_RadRotRot
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_Psi

  real(8), parameter :: eps = 1D-8

  integer :: i,j,k

  call MessageNotify('M','wtu_test_derivative5', &
       'wtu_module derivative function test #5')

  write( 6,* ) 'Test for wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  xyz_VLon = 0.0D0 ; xyz_VLat = 0.0D0 ; xyz_VRad = xyz_Rad
  xyz_RadRot = 0.0D0 ; xyz_RadRotRot = 0.0D0
  write(6,*)
  write(6,*)'Simple example (v_r=r)'
  call checkresult

! ¹äÂÎ²óÅ¾²òÈóÀþ·Á¹à(ÅìÀ¾Î®¤ì)
  xyz_VLon = 0.0D0
  xyz_VLat = xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)
  xyz_VRad = -xyz_Rad*cos(xyz_Lat)**2
  xyz_RadRot = 0.0D0 ; xyz_RadRotRot = 0.0D0
  write(6,*)
  write(6,*)'Rigid Rotation '
  call checkresult

! ¹äÂÎ²óÅ¾²òÈóÀþ·Á¹à(ÆîËÌÎ®¤ì)
  xyz_VLon = xyz_Rad*cos(xyz_Lat)*sin(xyz_Lon)*cos(xyz_Lon)
  xyz_VLat = -xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)*sin(xyz_Lon)**2
  xyz_VRad = -xyz_Rad*(sin(xyz_Lat)**2*sin(xyz_Lon)**2 + cos(xyz_Lon)**2)
  xyz_RadRot = 0.0D0 ; xyz_RadRotRot = 0.0D0

  write(6,*)
  write(6,*)'Rigid Rotation'
  call checkresult

! ±ôÄ¾±²ÅÙ¤òÈ¼¤¦¥Ù¥¯¥È¥ë¾ì
  xyz_Psi = xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lon)   ! r**2 P_1^1
  !xyz_Psi = xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lat)*sin(xyz_Lon)   ! r**2 P_2^1

  xyz_VLon =   xyz_gRadLat_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VLat = - xyz_gRadLon_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VRad = 0.0D0
  xyz_RadRot = 2 * xyz_Psi                           ! r¡¦¢¦¡ß¢¦¡ß(¦×r) = L_2¦×
  !xyz_RadRot = 6 * xyz_Psi                           ! r¡¦¢¦¡ß¢¦¡ß(¦×r) = L_2¦×
  xyz_RadRotRot = 0.0D0

  write(6,*)
  write(6,*)'Vortical field'
  call checkresult

! ±ôÄ¾Â®ÅÙ¤òÈ¼¤¦¥Ù¥¯¥È¥ë¾ì
  xyz_VRad = xyz_wt(wt_l2_wt(wt_xyz(xyz_Psi/xyz_Rad)))
  xyz_VLat = xyz_gRadLat_wt(wt_dRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))
  xyz_VLon = xyz_gRadLon_wt(wt_dRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))

  xyz_RadRot = 0.0D0
  xyz_RadRotRot = -xyz_wt(wt_l2_wt(wt_lapla_wt(wt_xyz(xyz_Psi))))
                 ! r¡¦¢¦¡ß¢¦¡ß¢¦¡ß¢¦¡ß(¦×r) = -L_2¢¦^2¦×
  write(6,*)
  write(6,*)'Non-Vortical field'
  call checkresult

  call MessageNotify('M','wt_test_derivative5', &
       'wt_module derivative function test #5 succeeded!')

  stop
contains

  subroutine checkresult
    write(6,*)'Checking r Rot v...'

    xyz_Data = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat))

    do k=0,kmo
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
    do k=0,kmo
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

end program wtu_test_derivative5
