!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module_sjpack テストプログラム
!
!      トロイダルポテンシャルの境界値問題
!
!履歴  2009/09/26  竹広真一   wt_module_sjpack_torvelbc_test.f90 より改造
!
program wt_zonal_module_sjpack_torvelbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_zonal_module_sjpack

  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16        ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_TorVel0
  real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_TorVel
  real(8), dimension(nm+1,0:lm)            :: wt_TorVel
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Zero = 0.0D0
  character(len=2), dimension(4),parameter :: BCond=(/'FF','FR','RF','RR'/)

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_True

  ! 判定誤差設定
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  real(8), parameter  :: pi=3.1415926535897932385D0

  integer :: l

  call MessageNotify('M','wt_zonal_module_sjpack_torvelbc_test', &
       'wt_zonal_module_sjpack wt_TorgBoundariesGrid subroutine test')

  call wt_initial(im,jm,km,nm,lm,ri,ro)

  do l=1,4

     ! P_10
     !xyz_TorVel = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
     xyz_TorVel = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

     xyz_TorVel0 = xyz_TorVel
     wt_TorVel = wt_xyz(xyz_TorVel)
     call wt_TorBoundariesGrid(wt_TorVel,cond=BCond(l),new=.true.)
     xyz_TorVel = xyz_wt(wt_TorVel)

     ! 内部チェック
     call AssertEqual(&
          message='wt_TorBoundariesGrid_wt (intenal value)',            &
          answer = xyz_Torvel(:,:,1:km-1),                              &
          check = xyz_Torvel0(:,:,1:km-1),                              &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     ! 上端境界チェック
     if( BCond(l)(1:1) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
     else
        xyz_True = xyz_TorVel
     endif

     call AssertEqual(&
          message='wt_TorvelBoundariesGrid_wt ('//BCond(l)//'-Top B.C)',&
          answer = xyz_Zero(:,:,0),                                     &
          check = xyz_True(:,:,0),                                      &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     ! 下端境界チェック
     if( BCond(l)(2:2) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
     else
        xyz_True = xyz_TorVel
     endif

     call AssertEqual(&
          message='wt_TorvelBoundariesGrid_wt ('//BCond(l)//'-Bottom B.C)',&
          answer = xyz_Zero(:,:,km),                                    &
          check = xyz_True(:,:,km),                                     &
          significant_digits = check_digits, ignore_digits = ignore     &
          )
  enddo

  call MessageNotify('M','wt_zonal_module_sjpack_torvelbc_test', &
       'wt_zonal_module_sjpack wt_TorvelBoundariesGrid subroutine test succeeded!')

end program wt_zonal_module_sjpack_torvelbc_test

