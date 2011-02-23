!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wt_mpi_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!
!  wt_RadRot_xvz_xvz, wt_RadRotRot_xvz_xvz_xvz  ¤Î¥Æ¥¹¥È
!    r¡¦(¢¦xv), r¡¦(¢¦x¢¦xv)
!
!ÍúÎò  2002/05/30  ÃÝ¹­¿¿°ì
!  
program wt_mpi_derivative_test5

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=16  ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: nm=10, lm=16         ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿, Æ°·Â)
  real(8),parameter  :: ri=0.5, ro=1.5      ! Æâ³°È¾·Â

  real(8), allocatable  :: xvz_VLon(:,:,:)
  real(8), allocatable  :: xvz_VLat(:,:,:)
  real(8), allocatable  :: xvz_VRad(:,:,:)
  real(8), allocatable  :: xvz_Data(:,:,:)
  real(8), allocatable  :: xvz_RadRot(:,:,:)
  real(8), allocatable  :: xvz_RadRotRot(:,:,:)
  real(8), allocatable  :: xvz_Psi(:,:,:)

  real(8), parameter :: eps = 1D-8

  integer :: i,j,k

  integer :: iproc, np, ierr

 !---------------- MPI ¥¹¥¿¡¼¥È ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test5', &
       'wt_module derivative function test #5')

  write( 6,* ) 'Test for wt_RadRot_xvz_xvz, wt_RadRotRot_xvz_xvz_xvz'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_mpi_initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_VLon(0:im-1,jc,0:km))
  allocate(xvz_VLat(0:im-1,jc,0:km))
  allocate(xvz_VRad(0:im-1,jc,0:km))
  allocate(xvz_Data(0:im-1,jc,0:km))
  allocate(xvz_RadRot(0:im-1,jc,0:km))
  allocate(xvz_RadRotRot(0:im-1,jc,0:km))
  allocate(xvz_Psi(0:im-1,jc,0:km))

  xvz_VLon = 0 ; xvz_VLat = 0 ; xvz_VRad = xvz_Rad
  xvz_RadRot = 0 ; xvz_RadRotRot = 0
  write(6,*)
  write(6,*)'Simple example (v_r=r)'
  call checkresult

! ¹äÂÎ²óÅ¾²òÈóÀþ·Á¹à(ÅìÀ¾Î®¤ì)
  xvz_VLon = 0
  xvz_VLat = xvz_Rad*sin(xvz_Lat)*cos(xvz_Lat)
  xvz_VRad = -xvz_Rad*cos(xvz_Lat)**2
  xvz_RadRot = 0 ; xvz_RadRotRot = 0
  write(6,*)
  write(6,*)'Rigid Rotation '
  call checkresult

! ¹äÂÎ²óÅ¾²òÈóÀþ·Á¹à(ÆîËÌÎ®¤ì)
  xvz_VLon = xvz_Rad*cos(xvz_Lat)*sin(xvz_Lon)*cos(xvz_Lon)
  xvz_VLat = -xvz_Rad*sin(xvz_Lat)*cos(xvz_Lat)*sin(xvz_Lon)**2
  xvz_VRad = -xvz_Rad*(sin(xvz_Lat)**2*sin(xvz_Lon)**2 + cos(xvz_Lon)**2)
  xvz_RadRot = 0 ; xvz_RadRotRot = 0

  write(6,*)
  write(6,*)'Rigid Rotation'
  call checkresult

! ±ôÄ¾±²ÅÙ¤òÈ¼¤¦¥Ù¥¯¥È¥ë¾ì
  xvz_Psi = xvz_Rad**2 * cos(xvz_Lat)*sin(xvz_Lon)   ! r**2 P_1^1
  !xvz_Psi = xvz_Rad**2 * cos(xvz_Lat)*sin(xvz_Lat)*sin(xvz_Lon)   ! r**2 P_2^1

  xvz_VLon =   xvz_gRadLat_wt(wt_xvz(xvz_Psi*xvz_Rad))
  xvz_VLat = - xvz_gRadLon_wt(wt_xvz(xvz_Psi*xvz_Rad))
  xvz_VRad = 0
  xvz_RadRot = 2 * xvz_Psi                           ! r¡¦¢¦¡ß¢¦¡ß(¦×r) = L_2¦×
  !xvz_RadRot = 6 * xvz_Psi                           ! r¡¦¢¦¡ß¢¦¡ß(¦×r) = L_2¦×
  xvz_RadRotRot = 0

  write(6,*)
  write(6,*)'Vortical field'
  call checkresult

! ±ôÄ¾Â®ÅÙ¤òÈ¼¤¦¥Ù¥¯¥È¥ë¾ì
  xvz_VRad = xvz_wt(wt_l2_wt(wt_xvz(xvz_Psi/xvz_Rad)))
  xvz_VLat = xvz_gRadLat_wt(wt_dRad_wt(wt_xvz(xvz_Psi*xvz_Rad)))
  xvz_VLon = xvz_gRadLon_wt(wt_dRad_wt(wt_xvz(xvz_Psi*xvz_Rad)))

  xvz_RadRot = 0
  xvz_RadRotRot = -xvz_wt(wt_l2_wt(wt_lapla_wt(wt_xvz(xvz_Psi))))
                 ! r¡¦¢¦¡ß¢¦¡ß¢¦¡ß¢¦¡ß(¦×r) = -L_2¢¦^2¦×
  write(6,*)
  write(6,*)'Non-Vortical field'
  call checkresult

  call MessageNotify('M','wt_mpi_derivative_test5', &
       'wt_mpi_module derivative function test #5 succeeded!')

 !------ MPI¤Î½ªÎ» ------

  call MPI_FINALIZE(IERR)

  stop
contains

  subroutine checkresult
    write(6,*)'Checking r Rot v...'

    xvz_Data = xvz_wt(wt_RadRot_xvz_xvz(xvz_VLon,xvz_VLat))

    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_RadRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_RadRot(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test5', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xvz_Data = xvz_wt(wt_RadRotRot_xvz_xvz_xvz(xvz_VLon,xvz_VLat,xvz_VRad))

    write(6,*)'Checking r Rot Rot v...'
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_Data(i,j,k)-xvz_RadRotRot(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_Data(i,j,k), xvz_RadRotRot(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test5', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult

end program wt_mpi_derivative_test5
