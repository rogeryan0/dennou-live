!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPCPACK/ISPACK-F90 テストプログラム
!      積分・平均計算
!
!履歴  2002/09/05  竹広真一
!
program wttest_intavr

  use wt_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数

  real(8), dimension(im,jm,0:km)           :: xyz_Data

  real(8), dimension(im,jm)                :: xy_Data
  real(8), dimension(jm,0:km)              :: yz_Data
  real(8), dimension(im,0:km)              :: xz_Data
  real(8), dimension(im)                   :: x_Data
  real(8), dimension(jm)                   :: y_Data
  real(8), dimension(0:km)                 :: z_Data

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: i,j 

  write( 6,* ) 'Test for integration and averaging'
!  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_Initial(im,jm,km,nm,lm,ri,ro,np)

 ! 定数
  write(6,*) '---- Constat field ----'
  write(6,*) 'Results (calculated,exact)'
  xyz_Data = 1.0

  yz_Data = yz_IntLon_xyz(xyz_Data)
  write(6,*) 'IntLon : ', yz_Data(1,1), 2*pi

  xz_Data = xz_IntLat_xyz(xyz_Data)
  write(6,*) 'IntLat : ', xz_Data(1,1), 2.0D0

  xy_Data = xy_IntRad_xyz(xyz_Data)
  write(6,*) 'IntRad : ', xy_Data(1,1), 1/3.0D0*(ro**3-ri**3)

  z_Data = z_IntLonLat_xyz(xyz_Data)
  write(6,*) 'IntLonLat : ', z_Data(1), 4*pi

  x_Data = x_IntLatRad_xyz(xyz_Data)
  write(6,*) 'IntLatRad : ', x_Data(1), 2/3.0D0*(ro**3-ri**3)

  y_Data = y_IntLonRad_xyz(xyz_Data)
  write(6,*) 'IntLonRad : ', y_Data(1), 2*pi/3.0D0*(ro**3-ri**3)

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyz(xyz_Data), 4*pi/3*(ro**3-ri**3)

 ! 
  write(6,*)
  write(6,*) '---- r ----'
  write(6,*) 'Results (calculated-exact)'

  xyz_Data = xyz_Rad 

  yz_Data = yz_IntLon_xyz(xyz_Data) - 2*pi* xyz_Rad(1,:,:)
  write(6,*) 'IntLon : ', yz_Data(1,1) 

  xz_Data = xz_IntLat_xyz(xyz_Data) - 2.0D0 * xyz_Data(:,1,:)
  write(6,*) 'IntLat : ', xz_Data(1,1)

  xy_Data = xy_IntRad_xyz(xyz_Data) - 1/4.0D0*(ro**4-ri**4) 
  write(6,*) 'IntRad : ', xy_Data(1,1)

  z_Data = z_IntLonLat_xyz(xyz_Data) - 4*pi * xyz_Data(1,1,:)
  write(6,*) 'IntLonLat : ', z_Data(1)

  x_Data = x_IntLatRad_xyz(xyz_Data) - 1/2.0D0*(ro**4-ri**4)
  write(6,*) 'IntLatRad : ', x_Data(1)

  y_Data = y_IntLonRad_xyz(xyz_Data) - pi/2.0D0*(ro**4-ri**4)
  write(6,*) 'IntLonRad : ', y_Data(1)

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyz(xyz_Data) - pi*(ro**4-ri**4)

  write(6,*)
  write(6,*) '---- sin(phi)**2  ----'
  write(6,*) 'Results (calculated-exact)'

  xyz_Data = sin(xyz_Lat)**2

  yz_Data = yz_IntLon_xyz(xyz_Data) - 2*pi* xyz_Data(1,:,:)
  write(6,*) 'IntLon : ', yz_Data(1,1) 

  xz_Data = xz_IntLat_xyz(xyz_Data) - 2.0D0/3.0D0
  write(6,*) 'IntLat : ', xz_Data(1,1)

  xy_Data = xy_IntRad_xyz(xyz_Data) - 1/3.0D0*(ro**3-ri**3) * xyz_Data(:,:,1)
  write(6,*) 'IntRad : ', xy_Data(1,1)

  z_Data = z_IntLonLat_xyz(xyz_Data) - 4*pi/3.0
  write(6,*) 'IntLonLat : ', z_Data(1)

  x_Data = x_IntLatRad_xyz(xyz_Data) - 2/9.0D0*(ro**3-ri**3)
  write(6,*) 'IntLatRad : ', x_Data(1)

  y_Data = y_IntLonRad_xyz(xyz_Data) - 2*pi/3.0D0*(ro**3-ri**3) * xyz_Data(1,:,1)
  write(6,*) 'IntLonRad : ', y_Data(1)

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xyz(xyz_Data) - 4*pi/9*(ro**3-ri**3)

end program wttest_intavr
