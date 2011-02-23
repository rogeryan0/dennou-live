!----------------------------------------------------------------------
!     Copyright (c) 2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPMODEL 2 次元水路領域ブシネスク対流モデル
!      チェビシェフガラーキン法による計算(温度固定条件)
!
!      時間積分 : Euler 法
!
! Solving 2-D Diffusioon equation (DD problem)
!     dT/dt = \nabla T
!     T = 0 at y=0,1
!
!履歴  2002/01/31  竹広真一
!
program et_galerkin_DD_euler_diff

  use lumatrix
  use et_module
  use et_galerkin_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter  :: im=64, jm=16      ! 格子点の設定(X,Y)
  integer, parameter  :: km=21, lm=10      ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter  :: xmin=0, xmax=2     ! X 座標範囲
  real(8), parameter  :: ymin=0, ymax=1     ! Y 座標範囲

 !---- 変数 ----
  real(8)             :: yx_Temp(0:jm,0:im-1)     ! 格子データ(温度)
  real(8)             :: ed_Temp(-km:km,2:lm)     ! ガラーキンデータ(温度)

  real(8)             :: yx_TempInit(0:jm,0:im-1) ! 格子データ(温度, 初期値)
  real(8)             :: yx_TempSol(0:jm,0:im-1)  ! 格子データ(温度, 解析解)
  real(8)             :: yx_TempError(0:jm,0:im-1)! 格子データ(温度, 誤差)

 !---- 時間積分パラメター ----

  real(8), parameter :: dt=1e-4                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter          :: Kappa=2.0      ! 拡散係数

 !---- その他 ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: k, l

 !---------------- 座標値の設定 ---------------------
  call et_galerkin_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax,DD=.true.)

 !------------------- 初期値設定 ----------------------
  write(6,*) 'k,l?'
  read(5,*)  k,l

  if ( k .ge. 0 ) then
     yx_Temp = cos(2*k*Pi*yx_X/(xmax-xmin)) * sin(l*Pi*yx_Y/(ymax-ymin))
  else
     yx_Temp = sin(2*k*Pi*yx_X/(xmax-xmin)) * sin(l*Pi*yx_Y/(ymax-ymin))
  endif
  yx_TempInit = yx_Temp
  
  ed_Temp = ed_et(et_yx(yx_Temp))

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt               ! Adams-Bashfort & Crank-Nicolson 法による時間積分
     time =it*dt

     ed_Temp = ed_Temp + dt * ed_et(kappa * et_Lapla_et(et_ed(ed_Temp)))

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

  contains

   !------------------- 出力 ----------------------
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='et_galerkin_DD_euler_diff.nc', &
           title='Diffusion equation (DD)', &
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
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='tempsol', dims=(/'x','y','t'/), & 
           longname='temperature(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='temperror', dims=(/'x','y','t'/), & 
           longname='temperature(Error)', units='1', xtype='double')

     !---- 実験パラメターを属性として定義, 出力(Global 属性) ----
      call HistoryAddAttr('x','+kappa', kappa )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      yx_Temp = yx_et(et_ed(ed_Temp))

      yx_TempSol = yx_TempInit &
           * exp( - kappa * (  (2*PI*k/(xmax-xmin))**2 &
                              +(PI*l/(ymax-ymin))**2     )*time )

      yx_TempError = yx_Temp-yx_TempSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(time))
      call HistoryPut('temp',transpose(yx_Temp))
      call HistoryPut('tempsol',transpose(yx_TempSol))
      call HistoryPut('temperror',transpose(yx_TempError))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program et_galerkin_DD_euler_diff
