!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!  wt_xyz, xyz_wt のテスト
!  
program wt_test1

  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数

  real(8), dimension(im,jm,0:km)     :: xyz_Data
  real(8), dimension((nm+1)**2,0:lm) :: wt_Data

  real(8), parameter :: eps=1D-10

  integer :: l
  integer :: n=1

  write( 6,* ) 'Test for wt_xyz'

  write(6,*) 'exponent N, of radial dependence for r^N?'
  read(5,*) n

  call wt_Initial(im,jm,km,nm,lm,ri,ro,np)

  !xyz_Data = xyz_Rad**n * sin(xyz_Lat)                  ! (n,m)=(1,0)
  xyz_Data = xyz_Rad**n * sin(xyz_Lon)*cos(xyz_Lat)    ! (n,m)=(1,-1)
  !xyz_Data = xyz_Rad**n * cos(xyz_Lon)*cos(xyz_Lat)    ! (n,m)=(1,1)

  wt_Data = wt_xyz(xyz_Data)

  write( 6,* ) 'Non-zero component of wt_Data(n,l)'
  write( 6,* ) 'n, nm_l(n),l, Data'
  do l=0,lm
     do n=1,(nm+1)**2
        if ( abs(wt_Data(n,l)) > eps ) then
           write( 6,* ) n,nm_l(n),l,wt_Data(n,l)
        end if
     enddo
  enddo

end program wt_test1
