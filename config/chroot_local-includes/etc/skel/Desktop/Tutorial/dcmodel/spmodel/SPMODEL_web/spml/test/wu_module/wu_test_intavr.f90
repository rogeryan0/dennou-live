!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wu_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!      ÀÑÊ¬¡¦Ê¿¶Ñ·×»»
!
!ÍúÎò  2008/01/01  ÃÝ¹­¿¿°ì
!      2008/06/28  º´¡¹ÌÚÍÎÊ¿  ÇÛÎó¤ÎÀë¸À¤òÊÑ¹¹
!
program wu_test_intavr

  use dc_message, only : MessageNotify
  use wu_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: nm=10, lm=16         ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿, Æ°·Â)
  real(8),parameter  :: ra=1.5               ! µåÈ¾·Â
  real(8), dimension(0:im-1,1:jm,0:km)           :: xyr_Data

  real(8), dimension(0:im-1,1:jm)                :: xy_Data
  real(8), dimension(1:jm,0:km)              :: yr_Data
  real(8), dimension(0:im-1,0:km)              :: xr_Data
  real(8), dimension(0:im-1)                   :: x_Data
  real(8), dimension(1:jm)                   :: y_Data
  real(8), dimension(0:km)                 :: r_Data

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps = 1D-4


!=============================== ÀÑÊ¬ =================================

  call MessageNotify('M','wu_test_intavr', &
       'wu_module integration function tests')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wu_Initial(im,jm,km,nm,lm,ra)

 ! Äê¿ô
  write(6,*) '---- Constat field ----'
  write(6,*) 'Results (calculated,exact)'
  xyr_Data = 1.0D0

  yr_Data = yr_IntLon_xyr(xyr_Data)
  write(6,*) 'IntLon : ', yr_Data(1,1), 2*pi
  if ( maxval(abs(yr_Data - 2*pi )) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  xr_Data = xr_IntLat_xyr(xyr_Data)
  write(6,*) 'IntLat : ', xr_Data(1,1), 2.0D0
  if ( maxval(abs(xr_Data - 2.0D0 )) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  xy_Data = xy_IntRad_xyr(xyr_Data)
  write(6,*) 'IntRad : ', xy_Data(1,1), 1/3.0D0*ra**3
  if ( maxval(abs(xy_Data - 1/3.0D0*ra**3 )) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  r_Data = r_IntLonLat_xyr(xyr_Data)
  write(6,*) 'IntLonLat : ', r_Data(1), 4*pi
  write(6,*) maxval(abs(r_Data - 4*pi ))
  if ( maxval(abs(r_Data - 4*pi)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xyr(xyr_Data)
  write(6,*) 'IntLatRad : ', x_Data(1), 2/3.0D0*ra**3
  if ( maxval(abs(x_Data - 2/3.0D0*ra**3 )) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  y_Data = y_IntLonRad_xyr(xyr_Data)
  write(6,*) 'IntLonRad : ', y_Data(1), 2*pi/3.0D0*ra**3
  if ( maxval(abs(y_Data - 2*pi/3.0D0*ra**3 )) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyr(xyr_Data), 4*pi/3*ra**3
  if ( abs(IntLonLatRad_xyr(xyr_Data) - 4*pi/3*ra**3) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

 ! 
  write(6,*)
  write(6,*) '---- r ----'
  write(6,*) 'Results (calculated-exact)'

  xyr_Data = xyr_Rad 

  yr_Data = yr_IntLon_xyr(xyr_Data) - 2.0D0 * pi * xyr_Rad(1,:,:)
  write(6,*) 'IntLon : ', yr_Data(1,1) 
  if ( maxval(abs(yr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  xr_Data = xr_IntLat_xyr(xyr_Data) - 2.0D0 * xyr_Data(:,1,:)
  write(6,*) 'IntLat : ', xr_Data(1,1)
  if ( maxval(abs(xr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  xy_Data = xy_IntRad_xyr(xyr_Data) - 1/4.0D0 *ra**4
  write(6,*) 'IntRad : ', xy_Data(1,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  r_Data = r_IntLonLat_xyr(xyr_Data) - 4.0D0 *pi * xyr_Data(1,1,:)
  write(6,*) 'IntLonLat : ', r_Data(1)
  if ( maxval(abs(r_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xyr(xyr_Data) - 1/2.0D0*ra**4
  write(6,*) 'IntLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  y_Data = y_IntLonRad_xyr(xyr_Data) - pi/2.0D0*ra**4
  write(6,*) 'IntLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyr(xyr_Data) - pi*ra**4
  if ( abs(IntLonLatRad_xyr(xyr_Data) - pi*ra**4) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*)
  write(6,*) '---- sin(phi)**2  ----'
  write(6,*) 'Results (calculated-exact)'

  xyr_Data = sin(xyr_Lat)**2

  yr_Data = yr_IntLon_xyr(xyr_Data) - 2*pi* xyr_Data(1,:,:)
  write(6,*) 'IntLon : ', yr_Data(1,1) 
  if ( maxval(abs(yr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  xr_Data = xr_IntLat_xyr(xyr_Data) - 2.0D0/3.0D0
  write(6,*) 'IntLat : ', xr_Data(1,1)
  if ( maxval(abs(xr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  xy_Data = xy_IntRad_xyr(xyr_Data) - 1/3.0D0*ra**3 * xyr_Data(:,:,1)
  write(6,*) 'IntRad : ', xy_Data(1,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  r_Data = r_IntLonLat_xyr(xyr_Data) - 4*pi/3.0
  write(6,*) 'IntLonLat : ', r_Data(1)
  if ( maxval(abs(r_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xyr(xyr_Data) - 2/9.0D0*ra**3
  write(6,*) 'IntLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  y_Data = y_IntLonRad_xyr(xyr_Data) - 2*pi/3.0D0*ra**3 * xyr_Data(1,:,1)

  write(6,*) 'IntLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyr(xyr_Data) - 4*pi/9*ra**3
  if ( abs(IntLonLatRad_xyr(xyr_Data) - 4*pi/9*ra**3) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Integration error too large.')
  endif

!=============================== Ê¿¶Ñ =================================

  write( 6,* )
  call MessageNotify('M','wu_test_intavr', &
       'wu_module averaging function tests')

  write( 6,* )
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

 ! Äê¿ô
  write(6,*) '---- Constat field ----'
  write(6,*) 'Results (calculated,exact)'
  xyr_Data = 1.0D0

  yr_Data = yr_AvrLon_xyr(xyr_Data)
  write(6,*) 'AvrLon : ', yr_Data(1,1), 1.0D0
  if ( maxval(abs(yr_Data - 1.0D0 )) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  xr_Data = xr_AvrLat_xyr(xyr_Data)
  write(6,*) 'AvrLat : ', xr_Data(1,1), 1.0D0
  if ( maxval(abs(xr_Data - 1.0D0 )) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  xy_Data = xy_AvrRad_xyr(xyr_Data)
  write(6,*) 'AvrRad : ', xy_Data(1,1), 1.0D0
  if ( maxval(abs(xy_Data - 1.0D0))  > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  r_Data = r_AvrLonLat_xyr(xyr_Data)
  write(6,*) 'AvrLonLat : ', r_Data(1), 1.0D0
  write(6,*) maxval(abs(r_Data - 1.0D0 ))
  if ( maxval(abs(r_Data - 1.0D0)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xyr(xyr_Data)
  write(6,*) 'AvrLatRad : ', x_Data(1), 1.0D0
  if ( maxval(abs(x_Data - 1.0D0))  > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  y_Data = y_AvrLonRad_xyr(xyr_Data)
  write(6,*) 'AvrLonRad : ', y_Data(1), 1.0D0
  if ( maxval(abs(y_Data - 1.0D0)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', AvrLonLatRad_xyr(xyr_Data), 1.0D0
  if ( abs(AvrLonLatRad_xyr(xyr_Data) - 1.0D0) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

 ! 
  write(6,*)
  write(6,*) '---- r ----'
  write(6,*) 'Results (calculated-exact)'

  xyr_Data = xyr_Rad 

  yr_Data = yr_AvrLon_xyr(xyr_Data) - xyr_Rad(1,:,:)
  write(6,*) 'AvrLon : ', yr_Data(1,1) 
  if ( maxval(abs(yr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  xr_Data = xr_AvrLat_xyr(xyr_Data) - xyr_Data(:,1,:)
  write(6,*) 'AvrLat : ', xr_Data(1,1)
  if ( maxval(abs(xr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  xy_Data = xy_AvrRad_xyr(xyr_Data) &
             - 1.0D0/4.0D0*ra**4/(1/3.0D0*ra**3)
  write(6,*) 'AvrRad : ', xy_Data(1,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  r_Data = r_AvrLonLat_xyr(xyr_Data) - xyr_Data(1,1,:)
  write(6,*) 'AvrLonLat : ', r_Data(1)
  write(6,*) maxval(abs(r_Data))
  if ( maxval(abs(r_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xyr(xyr_Data) &
            - 1/2.0D0*ra**4/(2/3.0D0*ra**3)
  write(6,*) 'AvrLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  y_Data = y_AvrLonRad_xyr(xyr_Data) &
       - pi/2.0D0*ra**4/(2*pi/3.0D0*ra**3)
  write(6,*) 'AvrLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', &
       AvrLonLatRad_xyr(xyr_Data) - pi*ra**4/(4*pi/3*ra**3)
  if ( abs(AvrLonLatRad_xyr(xyr_Data) &
            - pi*ra**4/(4*pi/3*ra**3)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  write(6,*)
  write(6,*) '---- sin(phi)**2  ----'
  write(6,*) 'Results (calculated-exact)'

  xyr_Data = sin(xyr_Lat)**2

  yr_Data = yr_AvrLon_xyr(xyr_Data) - xyr_Data(1,:,:)
  write(6,*) 'AvrLon : ', yr_Data(1,1) 
  if ( maxval(abs(yr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  xr_Data = xr_AvrLat_xyr(xyr_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLat : ', xr_Data(1,1)
  if ( maxval(abs(xr_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  xy_Data = xy_AvrRad_xyr(xyr_Data) - xyr_Data(:,:,1)
  write(6,*) 'AvrRad : ', xy_Data(1,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  r_Data = r_AvrLonLat_xyr(xyr_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLonLat : ', r_Data(1)
  write(6,*) maxval(abs(r_Data))
  if ( maxval(abs(r_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xyr(xyr_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  y_Data = y_AvrLonRad_xyr(xyr_Data) - xyr_Data(1,:,1)

  write(6,*) 'AvrLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', AvrLonLatRad_xyr(xyr_Data) - 1.0D0/3D0
  if ( abs(AvrLonLatRad_xyr(xyr_Data) - 1.0D0/3.0D0) > eps ) then
     call MessageNotify('E','wu_test_intavr', &
          'Average error too large.')
  endif

  call MessageNotify('M','wu_test_intavr', &
       'wu_module integration/averaging function tests succeeded!')

end program wu_test_intavr
