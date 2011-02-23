!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wtu_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!
!      ´Ø¿ô¤Î¥Æ¥¹¥È
!           wr_DivLon_xyr, wr_DivLat_xyr
!
!ÍúÎò  2008/01/13  ÃÝ¹­¿¿°ì  wu_test_derivative3.f90 ¤è¤ê²þÊÑ
!      2008/07/05  º´¡¹ÌÚÍÎÊ¿  ÇÛÎó¤ÎÀë¸À¤òÊÑ¹¹
!  
program wtu_test_derivative13

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: kmo=16, kmi=8        ! ³Ê»ÒÅÀ¤ÎÀßÄê(µå³ÌÆ°·Â, µåÆ°·Â)
  integer,parameter  :: nm=10                ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿)
  integer,parameter  :: lmo=16, lmi=8        ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(µå³ÌÆ°·Â, µåÆ°·Â)
  real(8),parameter  :: ri=0.5, ro=1.5       ! Æâ³°È¾·Â

  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_VLon
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_VLat
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_DivLon
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_DivLat
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_Data

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  call MessageNotify('M','wtu_test_derivative3', &
       'wu_module derivative function test #3')

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  write( 6,* ) 'Test for wu_DivLon_xyr, wu_DivLat_xyr'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- Îã 1 --------------------
  xyr_VLon   = xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)
  xyr_DivLon = xyr_rad**(n-1)*cos(xyr_Lat)*cos(xyr_Lon)

  xyr_VLat   = xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)
  xyr_DivLat = -3*xyr_rad**(n-1)*cos(xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)

  write(6,*)
  write(6,*)'Example 1'
  write(6,*)'  xyr_VLon=xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)'
  write(6,*)'  xyr_VLat=xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)'
  call checkresult

  call MessageNotify('M','wtu_test_derivative3', &
       'wu_module derivative function test #3 succeeded!')

  stop
contains

  subroutine checkresult

    write(6,*)'Checking DivLon (1/r cos(Lat) dLon)'
    xyr_Data =  xyr_wr(wr_DivLon_xyr(xyr_VLon))

    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_DivLon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_DivLon(i,j,k)
                call MessageNotify('E','wtu_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyr_Data =  xyr_wr(wr_DivLat_xyr(xyr_VLat))
    write(6,*)'Checking DivLat (1/r cos(Lat) dLat cos(Lat))'
    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_DivLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_DivLat(i,j,k)
                call MessageNotify('E','wtu_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult


end program wtu_test_derivative13

