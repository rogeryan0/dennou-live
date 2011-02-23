!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPMODEL 2 次元水路領域ブシネスク対流モデル
!      チェビシェフガラーキン法による計算(温度固定条件)
!
!      時間積分 : Crank Nicolson 法
!
! Solving 2-D Diffusioon equation (NN problem)
!     dφ/dt = \nabla φ
!     dφ/dy = 0 at y=-d/2,d/2
!
!      計算結果比較のための解析解については解説文書
!
!         「2 次元水路領域での拡散型方程式の解析解」
!
!      (et_diffusion.pdf)を参照のこと. 
!
!履歴  2009/09/16  竹広真一  et_galerkin_NN_cn_diff.f90 を fftj 用に改造
!
program et_galerkin_module_fftj_NN_cn_diff
  !
  ! SPMODEL 2 次元水路領域ブシネスク対流モデル
  !      チェビシェフガラーキン法による計算(温度固定条件)
  !
  !      時間積分 : Crank Nicolson 法
  !
  ! Solving 2-D Diffusioon equation (NN problem)
  !     dφ/dt = \nabla φ
  !     dφ/dy = 0 at y=-d/2,d/2
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「2 次元水路領域での拡散型方程式の解析解」
  !
  !      (et_diffusion.pdf)を参照のこと. 
  !
  use lumatrix
  use et_galerkin_module_fftj
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter  :: im=64, jm=16      ! 格子点の設定(X,Y)
  integer, parameter  :: km=21, lm=10      ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)             :: yx_Phi(0:jm,0:im-1)     ! 格子データ(温度)
  real(8)             :: ev_Phi(-km:km,2:lm)     ! ガラーキンデータ(温度)

  real(8)             :: yx_PhiInit(0:jm,0:im-1) ! 格子データ(温度, 初期値)
  real(8)             :: yx_PhiSol(0:jm,0:im-1)  ! 格子データ(温度, 解析解)
  real(8)             :: yx_PhiError(0:jm,0:im-1)! 格子データ(温度, 誤差)

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=500, ndisp=50        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter          :: XD=2.0         ! y 方向領域の大きさ
  real(8), parameter          :: YD=1.0         ! y 方向領域の大きさ
  real(8), parameter          :: Kappa=2.0      ! 拡散係数

 !---- 座標変数など ----
  real(8), parameter  :: xmin=0, xmax=XD        ! X 座標範囲
  real(8), parameter  :: ymin=-YD/2, ymax=YD/2  ! Y 座標範囲


 !--- Crank Nicholson 陰的計算用作業変数 ---
  real(8) :: DiffLUMT_NN(-km:km,2:lm,2:lm)
                                    ! Crank Nicholson 陰的計算用行列
  integer :: kpivot_NN(-km:km,2:lm) ! Crank Nicholson 用行列ピボット情報

 !---- その他 ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: k, l

 !---------------- 座標値の設定 ---------------------
  call et_galerkin_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax,NN=.true.)

  call CNDiffusionMatrixNN( kappa, dt, DiffLUMT_NN, kpivot_NN )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Positive k:cos(kx), negative k: sin(kx)'
  write(6,*) 'Positvie l:cos(lx), negative l: sin(lx)'
  write(6,*) 'k,l?'
  read(5,*)  k,l

  if ( k .ge. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Phi = cos(2*k*Pi*yx_X/XD) * cos(2*l*Pi*yx_Y/YD)
  else if ( k .lt. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : sin(kx)*cos(ly)'
     yx_Phi = sin(2*k*Pi*yx_X/XD) * cos(2*l*Pi*yx_Y/YD)
  else if ( k .ge. 0 .AND. l .lt. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Phi = cos(2*k*Pi*yx_X/XD) * sin((2*l+1)*Pi*yx_Y/YD)
  else 
     write(6,*) 'Type of the initial value : sin(kx)*sin(ly)'
     yx_Phi = sin(2*k*Pi*yx_X/XD) * sin((2*l+1)*Pi*yx_Y/YD)
  endif
  yx_PhiInit = yx_Phi
  
  ev_Phi = ev_et(et_yx(yx_Phi))

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt               ! Adams-Bashfort & Crank-Nicolson 法による時間積分
     time =it*dt

     ev_Phi = ev_Phi + dt/2 * ev_et(kappa * et_Lapla_et(et_ev(ev_Phi)))
     ev_Phi = LuSolve(DiffLUMT_NN,kpivot_NN,ev_Phi)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

  contains

   !------------------- 拡散項 ----------------------
    subroutine CNDiffusionMatrixNN( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (1-D dt/2▽^2, LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
      real(8), intent(OUT)  :: DiffLUMatrix(-km:km,2:lm,2:lm)
      integer, intent(OUT)  :: kpivot(-km:km,2:lm)            ! ピボット情報

      real(8)               :: ev_I(-km:km,2:lm)              ! 作業用変数

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         ev_I = 0.0 ; ev_I(:,l) = 1.0             ! 各波数成分独立
         DiffLUMatrix(:,:,l) &
              = ev_I - Diffc * dt/2.0 * ev_et(et_Lapla_et(et_ev(ev_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixNN

   !------------------- 出力 ----------------------
    subroutine output_gtool4_init
      !
      ! ヒストリー出力初期化ルーチン
      !
      !   ファイル作成
      !   変数定義
      !
      call HistoryCreate( &                               ! ヒストリー作成
           file='et_galerkin_module_fftj_NN_cn_diff.nc', &
           title='Diffusion equation (NN)', &
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
           varname='phi', dims=(/'x','y','t'/), & 
           longname='diffusive element', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='phisol', dims=(/'x','y','t'/), & 
           longname='diffusive element(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='phierror', dims=(/'x','y','t'/), & 
           longname='diffusive element(Error)', units='1', xtype='double')

     !---- 実験パラメターを属性として定義, 出力(Global 属性) ----
      call HistoryAddAttr('x','+kappa', kappa )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      !
      ! ヒストリー出力
      !
      !   各物理量の gtool4 ファイルへの出力を行う
      !
      yx_Phi = yx_et(et_ev(ev_Phi))

      if ( l .ge. 0 ) then
         yx_PhiSol = yx_PhiInit &
              * exp( - kappa * (  (2*PI*k/XD)**2 &
                                 +(2*PI*l/YD)**2     )*time )
      else
         yx_PhiSol = yx_PhiInit &
              * exp( - kappa * (   (2*PI*k/XD)**2 &
                                 +((2*l+1)*PI/YD)**2   )*time )
      endif

      yx_PhiError = yx_Phi-yx_PhiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(time))
      call HistoryPut('phi',transpose(yx_Phi))
      call HistoryPut('phisol',transpose(yx_PhiSol))
      call HistoryPut('phierror',transpose(yx_PhiError))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! ヒストリー出力終了処理
      !
      call HistoryClose
    end subroutine output_gtool4_close

end program et_galerkin_module_fftj_NN_cn_diff
