!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPCPACK/ISPACK-F90 テストプログラム
!      トロイダルポテンシャルの境界値問題
!
!履歴  2002/06/10  竹広真一
!
program wttest_TorBoundariesGrid

  use wt_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数

  real(8), dimension(im,jm,0:km)           :: xyz_TorVel0
  real(8), dimension(im,jm,0:km)           :: xyz_TorVel
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_TorVel
  character(len=2),parameter               :: BCond ='FF'

  real(8), dimension(im,jm,0:km)           :: xyz_True

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: i,j,k

  write( 6,* ) 'Test for wt_TorBoundariesGrid'

  call wt_initial(im,jm,km,nm,lm,ri,ro,np)

  ! P_10
  !xyz_TorVel = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
  ! P_1_1
  !xyz_TorVel = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
  xyz_TorVel = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

  xyz_TorVel0 = xyz_TorVel
  wt_TorVel = wt_xyz(xyz_TorVel)
  call wt_TorBoundariesGrid(wt_TorVel,cond=BCond)
  xyz_TorVel = xyz_wt(wt_TorVel)


! 内部チェック
  xyz_True = xyz_TorVel - xyz_TorVel0

  do k=1,km-1
     do j=1,jm
        do i=1,im
           if ( abs(xyz_True(i,j,k)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyz_True(i,j,k)
           endif
        enddo
     enddo
  enddo

! 上端境界チェック
  if( BCond(1:1) == 'F' ) then
     xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
  else
     xyz_True = xyz_TorVel
  endif

  do j=1,jm
     do i=1,im
        if ( abs(xyz_True(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyz_True(i,j,0)
        endif
     enddo
  enddo

! 下端境界チェック
  if( BCond(2:2) == 'F' ) then
     xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
  else
     xyz_True = xyz_TorVel
  endif

  do j=1,jm
     do i=1,im
        if ( abs(xyz_True(i,j,km)) > eps ) then
           write(6,*) 'Bottom B.C. : ', i,j,xyz_True(i,j,km)
        endif
     enddo
  enddo

end program wttest_TorBoundariesGrid

