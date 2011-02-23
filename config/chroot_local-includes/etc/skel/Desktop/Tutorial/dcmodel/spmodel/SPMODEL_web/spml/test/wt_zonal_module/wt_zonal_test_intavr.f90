!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module テストプログラム
!      積分・平均計算
!
!履歴  2008/12/30  竹広真一
!
program wt_zonal_test_intavr

  use dc_message, only : MessageNotify
  use wt_zonal_module

  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16        ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Data

  real(8), dimension(0:im-1,1:jm)          :: xy_Data
  real(8), dimension(1:jm,0:km)            :: yz_Data
  real(8), dimension(0:im-1,0:km)          :: xz_Data
  real(8), dimension(0:im-1)               :: x_Data
  real(8), dimension(1:jm)                 :: y_Data
  real(8), dimension(0:km)                 :: z_Data

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10


!=============================== 積分 =================================

  call MessageNotify('M','wt_zonal_test_intavr', &
       'wt_zonal_module integration function tests')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 ! 定数
  write(6,*) '---- Constat field ----'
  write(6,*) 'Results (calculated,exact)'
  xyz_Data = 1.0D0

  yz_Data = yz_IntLon_xyz(xyz_Data)
  write(6,*) 'IntLon : ', yz_Data(1,1), 2*pi
  if ( maxval(abs(yz_Data - 2*pi )) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  xz_Data = xz_IntLat_xyz(xyz_Data)
  write(6,*) 'IntLat : ', xz_Data(0,1), 2.0D0
  if ( maxval(abs(xz_Data - 2.0D0 )) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  xy_Data = xy_IntRad_xyz(xyz_Data)
  write(6,*) 'IntRad : ', xy_Data(0,1), 1/3.0D0*(ro**3-ri**3)
  if ( maxval(abs(xy_Data - 1/3.0D0*(ro**3-ri**3) )) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  z_Data = z_IntLonLat_xyz(xyz_Data)
  write(6,*) 'IntLonLat : ', z_Data(1), 4*pi
!  write(6,*) maxval(abs(z_Data - 4*pi ))
  if ( maxval(abs(z_Data - 4*pi)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xyz(xyz_Data)
  write(6,*) 'IntLatRad : ', x_Data(0), 2/3.0D0*(ro**3-ri**3)
  if ( maxval(abs(x_Data - 2/3.0D0*(ro**3-ri**3) )) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  y_Data = y_IntLonRad_xyz(xyz_Data)
  write(6,*) 'IntLonRad : ', y_Data(1), 2*pi/3.0D0*(ro**3-ri**3)
  if ( maxval(abs(y_Data - 2*pi/3.0D0*(ro**3-ri**3) )) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyz(xyz_Data), 4*pi/3*(ro**3-ri**3)
  if ( abs(IntLonLatRad_xyz(xyz_Data) - 4*pi/3*(ro**3-ri**3)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

 ! 
  write(6,*)
  write(6,*) '---- r ----'
  write(6,*) 'Results (calculated-exact)'

  xyz_Data = xyz_Rad 

  yz_Data = yz_IntLon_xyz(xyz_Data) - 2*pi* xyz_Rad(0,:,:)
  write(6,*) 'IntLon : ', yz_Data(1,1) 
  if ( maxval(abs(yz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  xz_Data = xz_IntLat_xyz(xyz_Data) - 2.0D0 * xyz_Data(:,1,:)
  write(6,*) 'IntLat : ', xz_Data(0,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  xy_Data = xy_IntRad_xyz(xyz_Data) - 1/4.0D0*(ro**4-ri**4) 
  write(6,*) 'IntRad : ', xy_Data(0,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  z_Data = z_IntLonLat_xyz(xyz_Data) - 4*pi * xyz_Data(0,1,:)
  write(6,*) 'IntLonLat : ', z_Data(1)
  if ( maxval(abs(z_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xyz(xyz_Data) - 1/2.0D0*(ro**4-ri**4)
  write(6,*) 'IntLatRad : ', x_Data(0)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  y_Data = y_IntLonRad_xyz(xyz_Data) - pi/2.0D0*(ro**4-ri**4)
  write(6,*) 'IntLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyz(xyz_Data) - pi*(ro**4-ri**4)
  if ( abs(IntLonLatRad_xyz(xyz_Data) - pi*(ro**4-ri**4)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*)
  write(6,*) '---- sin(phi)**2  ----'
  write(6,*) 'Results (calculated-exact)'

  xyz_Data = sin(xyz_Lat)**2

  yz_Data = yz_IntLon_xyz(xyz_Data) - 2*pi* xyz_Data(0,:,:)
  write(6,*) 'IntLon : ', yz_Data(1,1) 
  if ( maxval(abs(yz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  xz_Data = xz_IntLat_xyz(xyz_Data) - 2.0D0/3.0D0
  write(6,*) 'IntLat : ', xz_Data(0,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  xy_Data = xy_IntRad_xyz(xyz_Data) - 1/3.0D0*(ro**3-ri**3) * xyz_Data(:,:,1)
  write(6,*) 'IntRad : ', xy_Data(0,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  z_Data = z_IntLonLat_xyz(xyz_Data) - 4*pi/3.0
  write(6,*) 'IntLonLat : ', z_Data(1)
  if ( maxval(abs(z_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xyz(xyz_Data) - 2/9.0D0*(ro**3-ri**3)
  write(6,*) 'IntLatRad : ', x_Data(0)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  y_Data = y_IntLonRad_xyz(xyz_Data) - 2*pi/3.0D0*(ro**3-ri**3) * xyz_Data(0,:,1)

  write(6,*) 'IntLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyz(xyz_Data) - 4*pi/9*(ro**3-ri**3)
  if ( abs(IntLonLatRad_xyz(xyz_Data) - 4*pi/9*(ro**3-ri**3)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Integration error too large.')
  endif

!=============================== 平均 =================================

  write( 6,* )
  call MessageNotify('M','wt_zonal_test_intavr', &
       'wt_module averaging function tests')

  write( 6,* )
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

 ! 定数
  write(6,*) '---- Constat field ----'
  write(6,*) 'Results (calculated,exact)'
  xyz_Data = 1.0D0

  yz_Data = yz_AvrLon_xyz(xyz_Data)
  write(6,*) 'AvrLon : ', yz_Data(1,1), 1.0D0
  if ( maxval(abs(yz_Data - 1.0D0 )) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  xz_Data = xz_AvrLat_xyz(xyz_Data)
  write(6,*) 'AvrLat : ', xz_Data(0,1), 1.0D0
  if ( maxval(abs(xz_Data - 1.0D0 )) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  xy_Data = xy_AvrRad_xyz(xyz_Data)
  write(6,*) 'AvrRad : ', xy_Data(0,1), 1.0D0
  if ( maxval(abs(xy_Data - 1.0D0))  > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  z_Data = z_AvrLonLat_xyz(xyz_Data)
  write(6,*) 'AvrLonLat : ', z_Data(1), 1.0D0
!  write(6,*) maxval(abs(z_Data - 1.0D0 ))
  if ( maxval(abs(z_Data - 1.0D0)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xyz(xyz_Data)
  write(6,*) 'AvrLatRad : ', x_Data(0), 1.0D0
  if ( maxval(abs(x_Data - 1.0D0))  > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  y_Data = y_AvrLonRad_xyz(xyz_Data)
  write(6,*) 'AvrLonRad : ', y_Data(1), 1.0D0
  if ( maxval(abs(y_Data - 1.0D0)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', AvrLonLatRad_xyz(xyz_Data), 1.0D0
  if ( abs(AvrLonLatRad_xyz(xyz_Data) - 1.0D0) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

 ! 
  write(6,*)
  write(6,*) '---- r ----'
  write(6,*) 'Results (calculated-exact)'

  xyz_Data = xyz_Rad 

  yz_Data = yz_AvrLon_xyz(xyz_Data) - xyz_Rad(0,:,:)
  write(6,*) 'AvrLon : ', yz_Data(1,1) 
  if ( maxval(abs(yz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  xz_Data = xz_AvrLat_xyz(xyz_Data) - xyz_Data(:,1,:)
  write(6,*) 'AvrLat : ', xz_Data(0,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  xy_Data = xy_AvrRad_xyz(xyz_Data) &
             - 1.0D0/4.0D0*(ro**4-ri**4)/(1/3.0D0*(ro**3-ri**3))
  write(6,*) 'AvrRad : ', xy_Data(0,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  z_Data = z_AvrLonLat_xyz(xyz_Data) - xyz_Data(0,1,:)
  write(6,*) 'AvrLonLat : ', z_Data(1)
!  write(6,*) maxval(abs(z_Data))
  if ( maxval(abs(z_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xyz(xyz_Data) &
            - 1/2.0D0*(ro**4-ri**4)/(2/3.0D0*(ro**3-ri**3))
  write(6,*) 'AvrLatRad : ', x_Data(0)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  y_Data = y_AvrLonRad_xyz(xyz_Data) &
       - pi/2.0D0*(ro**4-ri**4)/(2*pi/3.0D0*(ro**3-ri**3))
  write(6,*) 'AvrLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', &
       AvrLonLatRad_xyz(xyz_Data) - pi*(ro**4-ri**4)/(4*pi/3*(ro**3-ri**3))
  if ( abs(AvrLonLatRad_xyz(xyz_Data) &
            - pi*(ro**4-ri**4)/(4*pi/3*(ro**3-ri**3))) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  write(6,*)
  write(6,*) '---- sin(phi)**2  ----'
  write(6,*) 'Results (calculated-exact)'

  xyz_Data = sin(xyz_Lat)**2

  yz_Data = yz_AvrLon_xyz(xyz_Data) - xyz_Data(0,:,:)
  write(6,*) 'AvrLon : ', yz_Data(1,1) 
  if ( maxval(abs(yz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  xz_Data = xz_AvrLat_xyz(xyz_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLat : ', xz_Data(0,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  xy_Data = xy_AvrRad_xyz(xyz_Data) - xyz_Data(:,:,1)
  write(6,*) 'AvrRad : ', xy_Data(0,1)
  if ( maxval(abs(xy_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  z_Data = z_AvrLonLat_xyz(xyz_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLonLat : ', z_Data(1)
!  write(6,*) maxval(abs(z_Data))
 if ( maxval(abs(z_Data)) > eps ) then
   call MessageNotify('E','wt_zonal_test_intavr', &
     'Average error too large.')
 endif

  x_Data = x_AvrLatRad_xyz(xyz_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLatRad : ', x_Data(0)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  y_Data = y_AvrLonRad_xyz(xyz_Data) - xyz_Data(0,:,1)

  write(6,*) 'AvrLonRad : ', y_Data(1)
  if ( maxval(abs(y_Data)) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', AvrLonLatRad_xyz(xyz_Data) - 1.0D0/3D0
  if ( abs(AvrLonLatRad_xyz(xyz_Data) - 1.0D0/3.0D0) > eps ) then
     call MessageNotify('E','wt_zonal_test_intavr', &
          'Average error too large.')
  endif

  call MessageNotify('M','wt_zonal_test_intavr', &
       'wt_module integration/averaging function tests succeeded!')

end program wt_zonal_test_intavr
