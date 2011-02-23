!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wt_mpi_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!
!      ¥µ¥Ö¥ë¡¼¥Á¥ó¤Î¥Æ¥¹¥È
!        wt_VGradV
!  
!ÍúÎò  2008/05/30  ÃÝ¹­¿¿°ì
!
program wt_mpi_derivative_test6

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=8  ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: nm=10, lm=8         ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿, Æ°·Â)
  real(8),parameter  :: ri=7.0D0/13.0D0      ! Æâ³°È¾·Â \eta=0.35
  real(8),parameter  :: ro=20.0D0/13.0D0     ! Æâ³°È¾·Â \eta=0.35

  real(8), allocatable  :: xvz_VLon(:,:,:)        ! Â®ÅÙ(·ÐÅÙ)
  real(8), allocatable  :: xvz_VLat(:,:,:)        ! Â®ÅÙ(°ÞÅÙ)
  real(8), allocatable  :: xvz_Vrad(:,:,:)        ! Â®ÅÙ(Æ°·Â)
  real(8), allocatable  :: xvz_VGradVLon(:,:,:)   ! ´·À­¹à(·ÐÅÙ)
  real(8), allocatable  :: xvz_VGradVLat(:,:,:)   ! ´·À­¹à(°ÞÅÙ)
  real(8), allocatable  :: xvz_VGradVrad(:,:,:)   ! ´·À­¹à(Æ°·Â)
  real(8), allocatable  :: xvz_VGradV0Lon(:,:,:)  ! ´·À­¹à(Àµ²ò, ·ÐÅÙ)
  real(8), allocatable  :: xvz_VGradV0Lat(:,:,:)  ! ´·À­¹à(Àµ²ò, °ÞÅÙ)
  real(8), allocatable  :: xvz_VGradV0rad(:,:,:)  ! ´·À­¹à(Àµ²ò,Æ°·Â)

  real(8), parameter :: eps = 1D-10
  real(8), parameter :: pi=3.1415926535897932385D0

  integer :: i,j,k

  integer :: iproc, np, ierr

 !---------------- MPI ¥¹¥¿¡¼¥È ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wt_mpi_derivative_test6', &
       'wt_module derivative subroutine test #6')

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_VLon(0:im-1,jc,0:km))        ! Â®ÅÙ(·ÐÅÙ)
  allocate(xvz_VLat(0:im-1,jc,0:km))        ! Â®ÅÙ(°ÞÅÙ)
  allocate(xvz_Vrad(0:im-1,jc,0:km))        ! Â®ÅÙ(Æ°·Â)
  allocate(xvz_VGradVLon(0:im-1,jc,0:km))   ! ´·À­¹à(·ÐÅÙ)
  allocate(xvz_VGradVLat(0:im-1,jc,0:km))   ! ´·À­¹à(°ÞÅÙ)
  allocate(xvz_VGradVrad(0:im-1,jc,0:km))   ! ´·À­¹à(Æ°·Â)
  allocate(xvz_VGradV0Lon(0:im-1,jc,0:km))  ! ´·À­¹à(Àµ²ò, ·ÐÅÙ)
  allocate(xvz_VGradV0Lat(0:im-1,jc,0:km))  ! ´·À­¹à(Àµ²ò, °ÞÅÙ)
  allocate(xvz_VGradV0rad(0:im-1,jc,0:km))  ! ´·À­¹à(Àµ²ò,Æ°·Â)

  write( 6,* ) 'Test for VGradV'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- Îã 1 --------------------
  ! ¹äÂÎ²óÅ¾¾ì
  xvz_Vlon = xvz_Rad * cos(xvz_Lat)
  xvz_Vlat = 0.0
  xvz_Vrad = 0.0

  xvz_VGradV0lon = 0.0
  xvz_VGradV0lat = xvz_Rad*sin(xvz_LAT)*cos(xvz_LAT)
  xvz_VGradV0rad = -xvz_Rad*cos(xvz_LAT)**2

  write(6,*)
  write(6,*)
  write(6,*)'Example 1 : rigid rotation'
  write(6,*)'    xvz_Vlon = xvz_Rad * cos(xvz_Lat)'
  write(6,*)'    xvz_Vlat = 0.0'
  write(6,*)'    xvz_VRad = 0.0'

  call checkresult

! ----------------- Îã 2 --------------------
  ! ¹äÂÎ²óÅ¾¾ì(ÆîËÌÎ®)
  xvz_VLon = -xvz_Rad*sin(xvz_Lat)*sin(xvz_Lon)
  xvz_VLat = -xvz_Rad*cos(xvz_Lon)
  xvz_VRad = 0.0

  xvz_VGradV0Lon = xvz_Rad*cos(xvz_Lat)*sin(xvz_Lon)*cos(xvz_Lon)
  xvz_VGradV0Lat = -xvz_Rad*sin(xvz_Lat)*cos(xvz_Lat)*sin(xvz_Lon)**2
  xvz_VGradV0Rad = -xvz_Rad*(sin(xvz_Lat)**2*sin(xvz_Lon)**2 + cos(xvz_lon)**2)

  write(6,*)
  write(6,*)
  write(6,*)'Example 2 : rigid rotation'
  write(6,*)'    xvz_Vlon=xvz_Rad*xvz_Rad*sin(xvz_Lat)*sin(xvz_Lon)'
  write(6,*)'    xvz_Vlat=-xvz_Rad*cos(xvz_Lon)'
  write(6,*)'    xvz_VRad = 0.0'

  call checkresult

!!$! ----------------- Îã 3 --------------------
!!$ ! ¥Ù¥ó¥Á¥Þ¡¼¥¯ case1 ¤Î½é´ü¼§¾ì
!!$  xvz_Vrad = 5.0D0/8.0D0 * ( 8*ro - 6*xvz_Rad - 2*ri**4/xvz_Rad**3 ) &
!!$               * cos(pi/2-xvz_Lat)
!!$  xvz_Vlat = - 5.0D0/8.0D0 * ( 9*xvz_Rad - 8*ro - ri**4/xvz_Rad**3 ) &
!!$               * sin(pi/2-xvz_Lat)
!!$  xvz_Vlon = 5 * sin(pi*(xvz_Rad-ri)) * sin(2*(pi/2-xvz_Lat))

  call MessageNotify('M','wt_mpi_derivative_test6', &
       'wt_module derivative function test #6 succeeded!')

 !------ MPI¤Î½ªÎ» ------

  call MPI_FINALIZE(IERR)
  stop
contains

 !------- ·ë²ÌÈæ³Ó -------
  subroutine checkresult

!!$    call wt_VGradV3(xvz_VGradVLon,xvz_VGradVLat,xvz_VGradVRad, &
!!$                    xvz_Vlon,xvz_Vlat,xvz_Vrad )
    call wt_VGradVMPI(xvz_VGradVLon,xvz_VGradVLat,xvz_VGradVRad, &
                      xvz_Vlon,xvz_Vlat,xvz_Vrad )

    write(6,*)
    write(6,*)'Checking VGradV_Lon '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_VGradVLon(i,j,k)-xvz_VGradV0Lon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_VGradVLon(i,j,k),xvz_VGradV0Lon(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test6', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking VGradV_Lat '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_VGradVLat(i,j,k)-xvz_VGradV0Lat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_VGradVLat(i,j,k),xvz_VGradV0Lat(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test6', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)
    write(6,*)'Checking VGradV_Rad '
    do k=0,km
       do j=1,jc
          do i=0,im-1
             if ( abs(xvz_VGradVRad(i,j,k)-xvz_VGradV0Rad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xvz_VGradVRad(i,j,k),xvz_VGradV0rad(i,j,k)
                call MessageNotify('E','wt_mpi_derivative_test6', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

 !------- VGradV ·×»»(¤½¤Î0) -------
  subroutine wt_VGradV0(xvz_VGRADV_LON,xvz_VGRADV_LAT,xvz_VGRADV_RAD, &
                       xvz_VLON,xvz_VLAT,xvz_VRAD )

    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LON
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LAT
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_RAD
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLON
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLAT
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VRAD

    xvz_VGRADV_LON = xvz_wt(    &
            wt_Div_xvz_xvz_xvz( &
                xvz_VLON * xvz_VLON, xvz_VLON*xvz_VLAT, xvz_VLON*xvz_VRAD )) &
          + xvz_VLON*xvz_VRAD/xvz_RAD              &
          - xvz_VLON*xvz_VLAT*tan(xvz_LAT)/xvz_RAD 

    xvz_VGRADV_LAT = xvz_wt(    &
            wt_Div_xvz_xvz_xvz( &
                xvz_VLAT*xvz_VLON, xvz_VLAT*xvz_VLAT, xvz_VLAT*xvz_VRAD )) &
          + xvz_VLAT*xvz_VRAD/xvz_RAD        &
          + xvz_VLON**2*tan(xvz_LAT)/xvz_RAD 

    xvz_VGRADV_RAD = xvz_wt(    &
            wt_Div_xvz_xvz_xvz( &
                xvz_VRAD*xvz_VLON, xvz_VRAD*xvz_VLAT, xvz_VRAD*xvz_VRAD )) &
          - (xvz_VLON**2 + xvz_VLAT**2)/xvz_RAD 

  end subroutine wt_VGradV0

 !------- VGradV ·×»»(¤½¤Î1) -------
  subroutine wt_VGradV1(xvz_VGRADV_LON,xvz_VGRADV_LAT,xvz_VGRADV_RAD, &
                       xvz_VLON,xvz_VLAT,xvz_VRAD )

    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LON
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LAT
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_RAD
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLON
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLAT
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VRAD

    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_VLon
    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_VLat
    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_VRad

    wt_VLon=wt_xvz(xvz_Vlon)
    wt_VLat=wt_xvz(xvz_Vlat)
    wt_VRad=wt_xvz(xvz_VRad)

    xvz_VGRADV_LON = xvz_VLon*xvz_GradLon_wt(wt_VLon)     &
                   + xvz_VLat*xvz_GradLat_wt(wt_VLon)     &
                   + xvz_VRad*xvz_wt(wt_Drad_wt(wt_VLon)) &
                   + xvz_VLON*xvz_VRAD/xvz_RAD            &
                   - xvz_VLON*xvz_VLAT*tan(xvz_LAT)/xvz_Rad 

    xvz_VGRADV_LAT = xvz_VLon*xvz_GradLon_wt(wt_VLat)     &
                   + xvz_VLat*xvz_GradLat_wt(wt_VLat)     &
                   + xvz_VRad*xvz_wt(wt_Drad_wt(wt_VLat)) &
                   + xvz_VLAT*xvz_VRAD/xvz_RAD            &
                   + xvz_VLON**2*tan(xvz_LAT)/xvz_RAD 

    xvz_VGRADV_RAD = xvz_VLon*xvz_GradLon_wt(wt_VRad)     &
                   + xvz_VLat*xvz_GradLat_wt(wt_VRad)     &
                   + xvz_VRad*xvz_wt(wt_Drad_wt(wt_VLat)) &
                   - (xvz_VLON**2 + xvz_VLAT**2)/xvz_RAD 

  end subroutine wt_VGradV1

 !------- VGradV ·×»»(¤½¤Î2) -------
  subroutine wt_VGradV2(xvz_VGRADV_LON,xvz_VGRADV_LAT,xvz_VGRADV_RAD, &
                       xvz_VLON,xvz_VLAT,xvz_VRAD )
   ! V¡¦¢¦V =  ¢¦(1/2V**2) - Vx¢¦xV

    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LON
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LAT
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_RAD
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLON
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLAT
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VRAD

    real(8), dimension(0:im-1,jc,0:km)           :: xvz_RotVlon
    real(8), dimension(0:im-1,jc,0:km)           :: xvz_RotVlat
    real(8), dimension(0:im-1,jc,0:km)           :: xvz_RotVrad
    real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_Ke

    wt_Ke = wt_xvz(1/2.0*(xvz_VLon**2 + xvz_VLat**2 + xvz_VRad**2))

    xvz_RotVLon=xvz_GradLat_wt(wt_xvz(xvz_VRad))-xvz_wt(wt_RotRad_wt(wt_xvz(xvz_VLat)))
    xvz_RotVLat=xvz_wt(wt_RotRad_wt(wt_xvz(xvz_VLon))-wt_DivLon_xvz(xvz_VRad))

    xvz_RotVRad=xvz_wt(wt_DivLon_xvz(xvz_VLat)-wt_DivLat_xvz(xvz_VLon))

    xvz_VGRADV_LON = xvz_GradLon_wt(wt_Ke) &
                     - (xvz_VLat*xvz_RotVRad- xvz_VRad*xvz_RotVLat)

    xvz_VGRADV_LAT = xvz_GradLat_wt(wt_Ke) &
                     - (xvz_VRad*xvz_RotVLon- xvz_VLon*xvz_RotVRad)

    xvz_VGRADV_RAD = xvz_wt(wt_DRad_wt(wt_Ke)) &
                     - (xvz_VLon*xvz_RotVLat- xvz_VLat*xvz_RotVLon)

  end subroutine wt_VGradV2

  subroutine wt_VGradV3(xvz_VGRADV_LON,xvz_VGRADV_LAT,xvz_VGRADV_RAD, &
                        xvz_VLON,xvz_VLAT,xvz_VRAD )

    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LON
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LAT
    real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_RAD
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLON
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLAT
    real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VRAD

    xvz_VGRADV_LON = &
            xvz_Div_xvz_xvz_xvz( &
                xvz_VLON * xvz_VLON, xvz_VLON*xvz_VLAT, xvz_VLON*xvz_VRAD ) &
          + xvz_VLON*xvz_VRAD/xvz_RAD              &
          - xvz_VLON*xvz_VLAT*tan(xvz_LAT)/xvz_RAD 

    xvz_VGRADV_LAT = &
            xvz_Div_xvz_xvz_xvz( &
                xvz_VLAT*xvz_VLON, xvz_VLAT*xvz_VLAT, xvz_VLAT*xvz_VRAD ) &
          + xvz_VLAT*xvz_VRAD/xvz_RAD        &
          + xvz_VLON**2*tan(xvz_LAT)/xvz_RAD 

    xvz_VGRADV_RAD = &
            xvz_Div_xvz_xvz_xvz( &
                xvz_VRAD*xvz_VLON, xvz_VRAD*xvz_VLAT, xvz_VRAD*xvz_VRAD ) &
          - (xvz_VLON**2 + xvz_VLAT**2)/xvz_RAD 

  end subroutine wt_VGradV3

end program wt_mpi_derivative_test6

