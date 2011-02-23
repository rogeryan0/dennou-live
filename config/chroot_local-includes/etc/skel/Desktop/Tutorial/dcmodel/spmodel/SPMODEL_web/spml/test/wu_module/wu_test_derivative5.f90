!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wu_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!
!  wu_RadRot_xyz_xyz, wu_RadRotRot_xyz_xyz_xyz  ¤Î¥Æ¥¹¥È
!    r¡¦(¢¦xv), r¡¦(¢¦x¢¦xv)
!
!ÍúÎò  2008/01/02  ÃÝ¹­¿¿°ì
!      2008/06/28  º´¡¹ÌÚÍÎÊ¿  ÇÛÎó¤ÎÀë¸À¤òÊÑ¹¹
!  
program wu_test_derivative5

  use dc_message, only : MessageNotify
  use wu_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: nm=10, lm=16         ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿, Æ°·Â)
  real(8),parameter  :: ra=1.5               ! µåÈ¾·Â

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VRad
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Data
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_RadRot
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_RadRotRot
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Torvel
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Polvel

  real(8), parameter :: eps = 1D-11

  integer :: i,j,k

  call MessageNotify('M','wu_test_derivative5', &
       'wu_module derivative function test #5')

  write( 6,* ) 'Test for wu_RadRot_xyr_xyr, wu_RadRotRot_xyr_xyr_xyr'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wu_initial(im,jm,km,nm,lm,ra)

  xyr_VLon = 0.0D0 ; xyr_VLat = 0.0D0 ; xyr_VRad = xyr_Rad
  xyr_RadRot = 0.0D0 ; xyr_RadRotRot = 0.0D0
  write(6,*)
  write(6,*)'Simple example (v_r=r)'
  call checkresult

! ¹äÂÎ²óÅ¾²òÈóÀþ·Á¹à(ÅìÀ¾Î®¤ì)

  xyr_VLon = 0.0D0
  xyr_VLat = xyr_Rad*sin(xyr_Lat)*cos(xyr_Lat)
  xyr_VRad = -xyr_Rad*cos(xyr_Lat)**2
  xyr_RadRot = 0.0D0 ; xyr_RadRotRot = 0.0D0
  write(6,*)
  write(6,*)'Rigid Rotation '
  call checkresult

! ¹äÂÎ²óÅ¾²òÈóÀþ·Á¹à(ÆîËÌÎ®¤ì)
  xyr_VLon = xyr_Rad*cos(xyr_Lat)*sin(xyr_Lon)*cos(xyr_Lon)
  xyr_VLat = -xyr_Rad*sin(xyr_Lat)*cos(xyr_Lat)*sin(xyr_Lon)**2
  xyr_VRad = -xyr_Rad*(sin(xyr_Lat)**2*sin(xyr_Lon)**2 + cos(xyr_Lon)**2)
  xyr_RadRot = 0.0D0 ; xyr_RadRotRot = 0.0D0

  write(6,*)
  write(6,*)'Rigid Rotation'
  call checkresult

! ±ôÄ¾±²ÅÙ¤òÈ¼¤¦¥Ù¥¯¥È¥ë¾ì
  xyr_Polvel = xyr_Rad**3 * cos(xyr_Lat)*sin(xyr_Lon)   ! r**3 P_1^1
  !xyr_Psi = xyr_Rad**3 * cos(xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)   ! r**2 P_2^1

  xyr_VLon =   xyr_GradLat_wu(wu_xyr(xyr_Polvel*xyr_Rad))
  xyr_VLat = - xyr_GradLon_wu(wu_xyr(xyr_Polvel*xyr_Rad))
  xyr_VRad = 0.0D0
  xyr_RadRot = 2 * xyr_Polvel                       ! r¡¦¢¦¡ß¢¦¡ß(¦×r) = L_2¦×
  !xyr_RadRot = 6 * xyr_Polvel                      ! r¡¦¢¦¡ß¢¦¡ß(¦×r) = L_2¦×
  xyr_RadRotRot = 0.0D0

  write(6,*)
  write(6,*)'Vortical field'
  call checkresult

! ±ôÄ¾Â®ÅÙ¤òÈ¼¤¦¥Ù¥¯¥È¥ë¾ì
  xyr_Torvel = 0.0D0
  xyr_Polvel = xyr_Rad**3 * cos(xyr_Lat)*sin(xyr_Lon)   ! r**2 P_1^1
  call wu_Potential2Vector(&
         xyr_VLon,xyr_VLat,xyr_VRad, wu_xyr(xyr_Torvel),wu_xyr(xyr_Polvel) )

!!$  xyr_VRad = xyr_wu(wu_L2_wu(wu_xyr(xyr_Psi/xyr_Rad)))
!!$  xyr_VLat = xyr_GradLat_wu(wu_wr(wr_DRad_wu(wu_xyr(xyr_Psi*xyr_Rad))))
!!$  xyr_VLon = xyr_GradLon_wu(wu_wr(wr_DRad_wu(wu_xyr(xyr_Psi*xyr_Rad))))

  xyr_RadRot = 0.0D0
  xyr_RadRotRot = -xyr_wu(wu_L2_wu(wu_wr(wr_Lapla_wu(wu_xyr(xyr_Polvel)))))
                 ! r¡¦¢¦¡ß¢¦¡ß¢¦¡ß¢¦¡ß(¦×r) = -L_2¢¦^2¦×
!!$  xyr_RadRotRot = -8 * cos(xyr_Lat)*sin(xyr_Lon)

  write(6,*)
  write(6,*)'Non-Vortical field'
  call checkresult

  call MessageNotify('M','wu_test_derivative5', &
       'wu_module derivative function test #5 succeeded!')

  stop
contains

  subroutine checkresult
    write(6,*)'Checking r Rot v...'

    xyr_Data = xyr_wu(wu_RadRot_xyr_xyr(xyr_VLon,xyr_VLat))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_RadRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_RadRot(i,j,k)
                call MessageNotify('E','wu_test_derivative5', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyr_Data = xyr_wr(wr_RadRotRot_xyr_xyr_xyr(xyr_VLon,xyr_VLat,xyr_VRad))

    write(6,*)'Checking r Rot Rot v...'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_RadRotRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_RadRotRot(i,j,k)
                call MessageNotify('E','wu_test_derivative5', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult

end program wu_test_derivative5
