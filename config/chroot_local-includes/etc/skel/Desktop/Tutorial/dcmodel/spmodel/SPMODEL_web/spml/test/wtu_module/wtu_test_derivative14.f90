!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wtu_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!
!   wt_KxRGrad_wt, xyr_KGrad_wt, wt_QOperator_wt ¤Î¥Æ¥¹¥È
!
!ÍúÎò  2008/01/13  ÃÝ¹­¿¿°ì  wu_test_derivative4.f90 ¤è¤ê²þÊÑ
!      2008/07/05  º´¡¹ÌÚÍÎÊ¿  ÇÛÎó¤ÎÀë¸À¤òÊÑ¹¹
!  
program wtu_test_derivative14

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: kmo=16, kmi=8        ! ³Ê»ÒÅÀ¤ÎÀßÄê(µå³ÌÆ°·Â, µåÆ°·Â)
  integer,parameter  :: nm=10                ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿)
  integer,parameter  :: lmo=16, lmi=8        ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(µå³ÌÆ°·Â, µåÆ°·Â)
  real(8),parameter  :: ri=0.5, ro=1.5       ! Æâ³°È¾·Â

  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_Data
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_Psi
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_KxRGrad
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_KGrad
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_QOperator


  integer, parameter :: n=3
  real(8), parameter :: eps = 1D-10

  integer :: i,j,k

  call MessageNotify('M','wu_test_derivative4', &
       'wu_module derivative function test #4')

  write( 6,* ) 'Test for wu_KxRGrad_wu, xyr_KGrad_wu, wu_QOperator_wu'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)


! ----------------- Îã 1 --------------------
  xyr_Psi = xyr_rad**n * cos(xyr_lat)*sin(xyr_lon)   ! r**2 Y_1^1

  xyr_KxRGrad = xyr_Rad**n * cos(xyr_lat)*cos(xyr_lon)

  ! k ¡¦¢¦ r**n Y_1^1 = (n-1)*r**(n-1)* Y_2^1
  xyr_KGrad = (n-1)*xyr_rad**(n-1)* cos(xyr_lat)*sin(xyr_lat)*sin(xyr_lon) 

  ! Q r**n Y_1^1 = -3*(n-1)*r**(n-1)* Y_2^1
  xyr_QOperator = - 3*(n-1)*xyr_rad**(n-1)* cos(xyr_lat)*sin(xyr_lat)*sin(xyr_lon) 

  write(6,*)
  write(6,*)'Y_1^1 field'
  call checkresult

! ----------------- Îã 2 --------------------
  xyr_Psi = 3*cos(xyr_lat)*sin(xyr_lat) * sin(xyr_lon) * xyr_Rad**2 ! Y_2^1

  xyr_KxRGrad = 3*cos(xyr_lat)*sin(xyr_lat) * cos(xyr_lon) * xyr_Rad**2 ! Y_2^1

  ! k¡¦¢¦r^2 Y_2^1 = 3r Y_1^1 
  xyr_KGrad = 3*xyr_Rad*cos(2*xyr_Lat)*cos(xyr_Lat)*sin(xyr_Lon) &
             +3*xyr_Rad*sin(2*xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)

  xyr_QOperator = -9*cos(xyr_lat)*sin(xyr_lon)*xyr_rad
!!$  xyr_QOperator = (12*sin(xyr_lat)**2 - 69.0/10.0)*cos(xyr_lat)*sin(xyr_lon)*xyr_rad

  write(6,*)
  write(6,*)'Y_2^1 field'
  call checkresult

  call MessageNotify('M','wu_test_derivative4', &
       'wu_module derivative function test #4 succeeded!')

  stop
contains

  subroutine checkresult
    write(6,*)'Checking k x r grad '
    xyr_Data = xyr_wu(wu_KxRGrad_wu(wu_xyr(xyr_Psi)))

    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_KxRGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_KxRGrad(i,j,k)
                call MessageNotify('E','wu_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking k grad '
    xyr_Data = xyr_KGrad_wu(wu_xyr(xyr_Psi))

    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_KGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_KGrad(i,j,k)
                call MessageNotify('E','wu_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking Q operator '
    xyr_Data = xyr_wu(wu_QOperator_wu(wu_xyr(xyr_Psi)))

    do k=0,kmi
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_QOperator(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_QOperator(i,j,k)
                call MessageNotify('E','wu_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult

end program wtu_test_derivative14

