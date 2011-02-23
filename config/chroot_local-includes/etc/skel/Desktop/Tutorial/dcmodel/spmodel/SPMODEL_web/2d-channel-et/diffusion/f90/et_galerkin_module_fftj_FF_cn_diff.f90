!----------------------------------------------------------------------
!     Copyright (c) 2009  Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPMODEL 2 次元水路領域非圧縮流体モデル
!      チェビシェフガラーキン法による計算
!
!      時間積分 : Crank-Nicolson 法
!      両端自由すべり条件
!
! Solving 2-D impompressible fluid system
!     d\zeta/dt =  Pr\nabla\zeta
!     \nabla\psi = \zeta
!     psi = psi'' = 0 at y=-d/2,d/2
!
!      計算結果比較のための解析解については解説文書
!
!         「2 次元水路領域での拡散型方程式の解析解」
!
!      (et_diffusion.pdf)を参照のこと. 
!
!履歴  2009/09/16  竹広真一  et_galerkin_SS_cn_diff.f90 を fftj 用に改造
!
program et_galerkin_module_fftj_FF_cn_diff
  !
  ! SPMODEL 2 次元水路領域非圧縮流体モデル
  !      チェビシェフガラーキン法による計算
  !
  !      時間積分 : Crank-Nicolson 法
  !      両端自由すべり条件
  !
  ! Solving 2-D impompressible fluid system
  !     d\zeta/dt =  Pr\nabla\zeta
  !     \nabla\psi = \zeta
  !     psi = psi'' = 0 at y=-d/2,d/2
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
  integer, parameter  :: im=32, jm=32      ! 格子点の設定(X,Y)
  integer, parameter  :: km=10, lm=20      ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter  :: XD=8.0                   ! x 方向領域の大きさ
  real(8), parameter  :: YD=1.0                   ! y 方向領域の大きさ
  real(8), parameter  :: xmin=0,     xmax=XD      ! X 座標範囲
  real(8), parameter  :: ymin=-YD/2, ymax=YD/2    ! Y 座標範囲

 !---- 変数 ----
  real(8)             :: yx_Psi(0:jm,0:im-1)      ! 格子データ(流線)
  real(8)             :: yx_Zeta(0:jm,0:im-1)     ! 格子データ(渦度)

  real(8)             :: et_Psi(-km:km,0:lm)      ! スペクトルデータ(流線)
  real(8)             :: et_Zeta(-km:km,0:lm)     ! スペクトルデータ(渦度)

  real(8)             :: eq_Psi(-km:km,4:lm)      ! ガラーキンデータ(流線)

  real(8)             :: yx_PsiInit(0:jm,0:im-1)  ! 格子データ(温度, 初期値)
  real(8)             :: yx_PsiSol(0:jm,0:im-1)   ! 格子データ(温度, 解析解)
  real(8)             :: yx_PsiError(0:jm,0:im-1) ! 格子データ(温度, 誤差)

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10   ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter          :: Pr=1.0         ! プランドル数
  character(len=2), parameter :: VelBC='FF'     ! 速度場境界条件(RR/FF/RF/FR)

 !--- Crank Nicholson 陰的計算用作業変数 ---
  real(8) :: DifLUMT_Psi(-km:km,4:lm,4:lm)
                                     ! Crank Nicholson 陰的計算用行列
  integer :: kpivot_Psi(-km:km,4:lm) ! Crank Nicholson 用行列ピボット情報

 !---- その他 ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: k, l

 !---------------- 座標値の設定 ---------------------
  call et_galerkin_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax,VelBC=VelBC)

  call CNDiffusionMatrixStrm( Pr, dt, DifLUMT_Psi, kpivot_Psi )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Positive k:cos(kx), negative k: sin(kx)'
  write(6,*) 'Positvie l:cos(lx), negative l: sin(lx)'
  write(6,*) 'k,l?'
  read(5,*)  k,l

  if ( k .ge. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Psi = cos(2*k*Pi*yx_X/XD) * cos((2*l+1)*Pi*yx_Y/YD)
  else if ( k .lt. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : sin(kx)*cos(ly)'
     yx_Psi = sin(2*k*Pi*yx_X/XD) * cos((2*l+1)*Pi*yx_Y/YD)
  else if ( k .ge. 0 .AND. l .lt. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Psi = cos(2*k*Pi*yx_X/XD) * sin(2*l*Pi*yx_Y/YD)
  else 
     write(6,*) 'Type of the initial value : sin(kx)*sin(ly)'
     yx_Psi = sin(2*k*Pi*yx_X/XD) * sin(2*l*Pi*yx_Y/YD)
  endif
  yx_PsiInit = yx_Psi
  
  eq_Psi = eq_et(et_yx(yx_Psi))

  et_Psi  = et_yx(yx_Psi)
  et_Zeta = et_Lapla_et(et_eq(eq_Psi))

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Cn 法による時間積分
     time =it*dt
     et_Zeta = et_Zeta + dt/2 *( Pr*et_Lapla_et(et_Zeta)  )
     eq_Psi = eq_Vor2Strm_et(et_Zeta)
     eq_Psi = LuSolve(DifLUMT_Psi,kpivot_Psi,eq_Psi)
     et_Zeta = et_Lapla_et(et_eq(eq_Psi))

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

  contains

   !------------------- 拡散項(流線関数用) ----------------------
    subroutine CNDiffusionMatrixStrm( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (流線関数用, ▽^{-2}[▽^2-D dt/2▽^2▽^2], LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 拡散陰的計算用行列(▽^{-2}(▽^2-D (dt/2)▽^4, LU 分解)
      real(8), intent(OUT)  :: DiffLUMatrix(-km:km,4:lm,4:lm)
      integer, intent(OUT)  :: kpivot(-km:km,4:lm)             ! ピボット情報

      real(8)               :: eq_I(-km:km,4:lm)               ! 作業用変数
      real(8)               :: et_I(-km:km,0:lm)               ! 作業用変数

      integer :: l

      DiffLUMatrix = 0.0
      do l=4,lm
         eq_I = 0.0 ; eq_I(:,l) = 1.0             ! 各波数成分独立
         et_I = et_eq(eq_I)
         et_I = et_Lapla_et(et_I)
         et_I = et_I - Diffc * dt/2.0 * et_Lapla_et(et_I)
         DiffLUMatrix(:,:,l) = eq_Vor2Strm_et(et_I)
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixStrm

   !------------------- 出力 ----------------------
    subroutine output_gtool4_init
      !
      ! ヒストリー出力初期化ルーチン
      !
      !   ファイル作成
      !   変数定義
      !
      call HistoryCreate( &                               ! ヒストリー作成
           file='et_galerkin_module_fftj_FF_cn_diff.nc', &
           title='2dim. imcompressible fluid', &
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
           longname='vorticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='psisol', dims=(/'x','y','t'/), & 
           longname='stream function(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='psierror', dims=(/'x','y','t'/), & 
           longname='stream function(Error)', units='1', xtype='double')

     !---- 実験パラメターを属性として定義, 出力(Global 属性) ----
      call HistoryAddAttr('x','+VelBC', VelBC )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      !
      ! ヒストリー出力
      !
      !   各物理量の gtool4 ファイルへの出力を行う
      !
      yx_Psi  = yx_et(et_eq(eq_Psi))
      yx_Zeta = yx_et(et_Zeta)

      if ( l .ge. 0 ) then
         yx_PsiSol = yx_PsiInit &
              * exp( - Pr * (   (2*PI*k/XD)**2 &
                                 +((2*l+1)*PI/YD)**2   )*time )
      else
         yx_PsiSol = yx_PsiInit &
              * exp( - Pr * (  (2*PI*k/XD)**2 &
                                 +(2*PI*l/YD)**2     )*time )
      endif

      yx_PsiError = yx_Psi-yx_PsiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('psisol',transpose(yx_PsiSol))
      call HistoryPut('psierror',transpose(yx_PsiError))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! ヒストリー出力終了処理
      !
      call HistoryClose
    end subroutine output_gtool4_close

end program et_galerkin_module_fftj_FF_cn_diff
