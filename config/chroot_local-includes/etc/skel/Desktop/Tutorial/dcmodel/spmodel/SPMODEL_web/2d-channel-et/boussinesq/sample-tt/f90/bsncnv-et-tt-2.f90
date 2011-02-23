!----------------------------------------------------------------------
!  Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPMODEL 2D-CHANNEL ブシネスク対流モデル
!
! Solving 2-D Boussinesq fluid system (TT problem)
!     d\zeta/dt + J(\psi,\zeta) = PrRa dT/dx + Pr\nabla\zeta
!     dT/dt + J(\psi,T) - d\psi/dx = \nabla T
!     \nabla\psi = \zeta
!     psi = zeta = T = 0 at y=0,1
!
!履歴  2002/01/25  竹広真一
!      2002/04/15  竹広真一
!      2002/08/19  竹広真一
!      2004/01/26  小高正嗣: gt4f90io に対応
!      2005/03/15  竹広真一  xy -> yx に変更
!
program bsncnv1

  use et_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter  :: im=128, jm=16      ! 格子点の設定(X,Y)
  integer, parameter  :: km=42, lm=16       ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter  :: xmin=0, xmax=8     ! X 座標範囲
  real(8), parameter  :: ymin=0, ymax=1     ! Y 座標範囲

 !---- 変数 ----
  real(8)             :: yx_psi(0:jm,0:im-1)    ! 格子データ(流線)
  real(8)             :: yx_Temp(0:jm,0:im-1)   ! 格子データ(温度)
  real(8)             :: yx_Zeta(0:jm,0:im-1)   ! 格子データ(渦度)

  real(8)             :: et_Psi(-km:km,0:lm)    ! スペクトルデータ(流線)
  real(8)             :: et_Temp(-km:km,0:lm)   ! スペクトルデータ(温度)
  real(8)             :: et_Zeta(-km:km,0:lm)   ! スペクトルデータ(渦度)

  real(8)             :: x_TempBndry(0:im-1,2)  ! 境界値
  real(8)             :: e_TempBndry(-km:km,2)  ! 境界値

  real(8)             :: PsiBndry(2)=(/0.0,0.0/)  ! 境界値

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-5                 ! 時間ステップ間隔
  integer, parameter :: nt=50000, ndisp=2500    ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  character(len=2), parameter :: TempBC='DD'    ! 温度境界条件(DD/DN/ND/NN)
  character(len=2), parameter :: PsiBC='FF'     ! 流れ境界条件(RR/RF/FR/FF)
  real(8), parameter :: Ra=1.0e4                ! レイリー数
  real(8), parameter :: Pr=1.0                  ! プランドル数

 !---- その他 ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)

 !------------------- 初期値設定 ----------------------
  yx_Temp = 1 -yx_y
  yx_Temp(jm/2,im/2) = yx_Temp(jm/2,im/2) + 0.01        ! 温度擾乱場
  yx_Psi  = 0.0
  yx_Zeta = 0.0

  et_Temp = et_yx(yx_Temp)
  et_Psi  = et_yx(yx_Psi)
  et_Zeta = et_yx(yx_Zeta)

 !------------------- 境界値設定 ----------------------

  x_TempBndry(:,1) = 0
  x_TempBndry(:,2) = 1
  e_TempBndry(:,1) = e_x(x_TempBndry(:,1))
  e_TempBndry(:,2) = e_x(x_TempBndry(:,2))

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     et_Temp = et_Temp + &
          dt*( -et_Jacobian_et_et(et_Psi,et_Temp) &
               + et_Lapla_et(et_Temp) )

     call et_Boundaries(et_Temp,e_TempBndry,cond=TempBC)

     et_Zeta = et_Zeta + &
          dt*( - et_Jacobian_et_et(et_Psi,et_Zeta) &
               + Pr*Ra*et_Dx_et(et_Temp) + Pr*et_Lapla_et(et_Zeta)    )

     et_Psi = at_ay(ey_Vor2Strm_ey(ay_at(et_Zeta),PsiBndry,PsiBC))
     et_Zeta = et_Lapla_et(et_Psi)
 
     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        yx_Zeta = yx_et(et_Zeta)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

contains

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='bsncnv_1_et_ff.nc', title='Boussinesq convection (TT)', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! 変数出力
      call HistoryAddattr('x','topology','circular')      ! 周期属性
      call HistoryAddattr('x','modulo',xmax-xmin)         ! 周期属性
      call HistoryPut('y',y_Y)                            ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='psi', dims=(/'x','y','t'/), & 
           longname='stream function', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='voticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      yx_Temp = yx_et(et_Temp)
      yx_Psi  = yx_et(et_Psi)
      yx_Zeta = yx_et(et_Zeta)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('temp',transpose(yx_Temp))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program bsncnv1
