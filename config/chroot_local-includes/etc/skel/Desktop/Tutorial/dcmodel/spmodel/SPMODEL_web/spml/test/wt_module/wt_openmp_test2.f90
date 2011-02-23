!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!  wt_DivRad_wt のテスト (1/r^2 d/dr r^2)
!  
program wttest2

  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数

  real(8), dimension(im,jm,0:km)     :: xyz_Data
  real(8), dimension(im,jm,0:km)     :: xyz_Data1
  real(8), dimension((nm+1)**2,0:lm) :: wt_Data

  integer :: i,j,k

  integer :: n=1

  write( 6,* ) 'Test for wt_Divrat_wt'
  write(6,*) 'exponent N, of Radial dependence for r^N?'

  write(6,*) 'n?'
  read(5,*) n

  call wt_Initial(im,jm,km,nm,lm,ri,ro,np)

  xyz_Data = xyz_Rad**n
  xyz_Data1 = (n+2)*xyz_Rad**(n-1)

  xyz_Data = xyz_wt(wt_DivRad_wt(wt_xyz(xyz_Data)))

  write( 6,* ) 'Error is...'
  write( 6,* ) maxval(abs(xyz_Data-xyz_Data1))

end program wttest2
