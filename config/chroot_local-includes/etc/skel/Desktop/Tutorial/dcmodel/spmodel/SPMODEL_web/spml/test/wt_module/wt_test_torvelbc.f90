!----------------------------------------------------------------------
!     Copyright (c) 2002--2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module テストプログラム
!
!      トロイダルポテンシャルの境界値問題
!
!履歴  2002/06/10  竹広真一
!      2007/11/11  竹広真一  エラーメッセージ追加
!      2008/06/28  佐々木洋平  配列の宣言を変更
!
program wt_test_torvelbc

  use dc_message, only : MessageNotify
  use wt_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_TorVel0
  real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_TorVel
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_TorVel
  character(len=2), dimension(4),parameter :: BCond=(/'FF','FR','RF','RR'/)

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_True

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: i,j,k,l

  call MessageNotify('M','wt_test_torvelbc', &
       'wt_module  wt_TorgBoundariesGrid subroutine test')

  call wt_initial(im,jm,km,nm,lm,ri,ro)

  do l=1,4

     ! P_10
     !xyz_TorVel = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
     ! P_1_1
     !xyz_TorVel = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
     xyz_TorVel = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

     xyz_TorVel0 = xyz_TorVel
     wt_TorVel = wt_xyz(xyz_TorVel)
     call wt_TorBoundariesGrid(wt_TorVel,cond=BCond(l),new=.true.)
     xyz_TorVel = xyz_wt(wt_TorVel)


     ! 内部チェック
     xyz_True = xyz_TorVel - xyz_TorVel0

     do k=1,km-1
        do j=1,jm
           do i=0,im-1
              if ( abs(xyz_True(i,j,k)) > eps ) then
                 write(6,*) 'internal value. : ', i,j,xyz_True(i,j,k)
                 call MessageNotify('E','wt_test_torvelbc',&
                              'internal value error too large')
              endif
           enddo
        enddo
     enddo

     ! 上端境界チェック
     if( BCond(l)(1:1) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
     else
        xyz_True = xyz_TorVel
     endif

     do j=1,jm
        do i=0,im-1
           if ( abs(xyz_True(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyz_True(i,j,0)
              call MessageNotify('E','wt_test_torvelbc',&
                              'Top B.C. error too large')
           endif
        enddo
     enddo
     call MessageNotify('M','wt_test_torvelbc', &
                        BCond(l)//'-Top B.C. test succeeded!')

     ! 下端境界チェック
     if( BCond(l)(2:2) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
     else
        xyz_True = xyz_TorVel
     endif

     do j=1,jm
        do i=0,im-1
           if ( abs(xyz_True(i,j,km)) > eps ) then
              write(6,*) 'Bottom B.C. : ', i,j,xyz_True(i,j,km)
              call MessageNotify('E','wt_test_torvelbc',&
                              'Bottom B.C. error too large')
           endif
        enddo
     enddo

     call MessageNotify('M','wt_test_torvelbc', &
                        BCond(l)//'-Bottom B.C. test succeeded!')
  enddo

  call MessageNotify('M','wt_test_torvelbc', &
       'wt_module  wt_TorgBoundariesGrid subroutine test succeeded!')

end program wt_test_torvelbc

