!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPMODEL 3 次元平板領域ブシネスク対流モデル
!      チェビシェフ法による計算(温度固定条件)
!
!      時間積分 : Crank Nicolson 法
!
!      計算結果比較のための解析解については解説文書
!
!         「2 次元水路領域での拡散型方程式の解析解」
!
!      (et_diffusion.pdf)を参照のこと. 
!
! Solving 3-D Diffusioon equation (DD problem)
!     dφ/dt = \nabla φ
!     φ = 0 at y=-d/2,d/2
!
!履歴  2009/12/25  竹広真一: et_DD_cn_diff.f90 より改造
!
program tee_module_DD_cn_diff
  !
  ! SPMODEL 3 次元平板領域ブシネスク対流モデル
  !      チェビシェフガラーキン法による計算(温度固定条件)
  !
  !      時間積分 : Crank Nicolson 法
  !
  ! Solving 3-D Diffusioon equation (DD problem)
  !     dφ/dt = \nabla φ
  !     φ = 0 at y=-d/2,d/2
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「2 次元水路領域での拡散型方程式の解析解」
  !
  !      (et_diffusion.pdf)を参照のこと. 
  !
  use lumatrix
  use tee_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter  :: im=32, jm=32, km=16      ! 格子点の設定(X,Y,Z)
  integer, parameter  :: lm=10, mm=10, nm=10      ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8)             :: zyx_Phi(0:km,0:jm-1,0:im-1)   ! 格子データ
  real(8)             :: tee_Phi(0:nm,-mm:mm,-lm:lm)   ! スペクトルデータ(温度)
  
  real(8)             :: zyx_PhiInit(0:km,0:jm-1,0:im-1)  ! 格子データ(初期値)
  real(8)             :: zyx_PhiSol(0:km,0:jm-1,0:im-1)   ! 格子データ(解析解)
  real(8)             :: zyx_PhiError(0:km,0:jm-1,0:im-1) ! 格子データ(誤差)

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter          :: XD=2.0         ! x 方向領域の大きさ
  real(8), parameter          :: YD=2.0         ! y 方向領域の大きさ
  real(8), parameter          :: ZD=1.0         ! y 方向領域の大きさ
  real(8), parameter          :: Kappa=1.0      ! 拡散係数
  character(len=2)            :: BC='DD'        ! 境界条件(DD/DN/ND/NN)

 !---- 座標変数など ----
  real(8), parameter  :: xmin=0, xmax=XD        ! X 座標範囲
  real(8), parameter  :: ymin=0, ymax=YD        ! Y 座標範囲
  real(8), parameter  :: zmin=-ZD/2, zmax=ZD/2  ! Z 座標範囲


 !--- Crank Nicholson 陰的計算用作業変数 ---
  real(8) :: DiffLUMT((2*lm+1)*(2*mm+1),0:nm,0:nm)     
                                    ! Crank Nicholson 陰的計算用行列
  integer :: kpivot((2*lm+1)*(2*mm+1),0:nm)

 !---- その他 ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: l,m,n

 !---------------- 座標値の設定 ---------------------
  call tee_Initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax)

  call CNDiffusionMatrix( kappa, dt, DiffLUMT, kpivot )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Positive l:cos(kx), negative l: sin(kx)'
  write(6,*) 'Positvie m:cos(my), negative m: sin(my)'
  write(6,*) 'Positvie n:cos(nz), negative n: sin(nz)'
  write(6,*) 'l,m,n?'
  read(5,*)  l,m,n

  if ( l .ge. 0 .AND. m .ge. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*cos(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .ge. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*cos(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else   if ( l .ge. 0 .AND. m .lt. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*sin(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .lt. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*sin(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else if ( l .ge. 0 .AND. m .ge. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*cos(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*sin(2*n*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .ge. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*cos(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*sin(2*Pi*zyx_Z/ZD)
  else   if ( l .ge. 0 .AND. m .lt. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*sin(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*sin(2*n*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .lt. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*sin(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*sin(2*n*Pi*zyx_Z/ZD)
  end if

  zyx_PhiInit = zyx_Phi
  
  tee_Phi = tee_zyx(zyx_Phi)

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt               ! Adams-Bashfort & Crank-Nicolson 法による時間積分
     time =it*dt

     tee_Phi = tee_Phi + dt/2 * kappa * tee_Lapla_tee(tee_Phi)
     tee_Phi = aee_e2a(LuSolve(DiffLUMT,kpivot,e2a_aee(tee_Phi)))

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

  contains

   !------------------- 拡散項 ----------------------
    subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (1-D dt/2▽^2, LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
      real(8), intent(OUT) :: DiffLUMatrix((2*lm+1)*(2*mm+1),0:nm,0:nm)     
      integer, intent(OUt) :: kpivot((2*lm+1)*(2*mm+1),0:nm)

      real(8)               :: e2t_I((2*lm+1)*(2*mm+1),0:nm)   ! 作業用変数
      real(8)               :: tee_DI(0:nm,-mm:mm,-lm:lm)      ! 作業用変数

      integer :: n

      DiffLUMatrix = 0.0
      do n=0,nm
         e2t_I = 0.0 ; e2t_I(:,n) = 1.0d0             ! 各波数成分独立
         tee_DI =  - Diffc * dt/2.0 * tee_Lapla_tee(aee_e2a(e2t_I))
         call tee_Boundaries(tee_DI,cond=BC)
         DiffLUMatrix(:,:,n) = e2t_I + e2a_aee(tee_DI)
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrix

   !------------------- 出力 ----------------------
    subroutine output_gtool4_init
      !
      ! ヒストリー出力初期化ルーチン
      !
      !   ファイル作成
      !   変数定義
      !
      call HistoryCreate( &                               ! ヒストリー作成
           file='tee_module_DD_cn_diff.nc', &
           title='Diffusion equation', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','z','t'/), dimsizes=(/im,jm,km+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate',&
                       'Z-coordinate','time        '/),&
           units=(/'1','1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! 変数出力
      call HistoryAddattr('x','topology','circular')      ! 周期属性
      call HistoryAddattr('x','modulo',xmax-xmin)         ! 周期属性
      call HistoryPut('y',y_Y)                            ! 変数出力
      call HistoryAddattr('y','topology','circular')      ! 周期属性
      call HistoryAddattr('y','modulo',ymax-ymin)         ! 周期属性
      call HistoryPut('z',z_Z)                            ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='phi', dims=(/'z','y','x','t'/), & 
           longname='diffusive element', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='phisol', dims=(/'z','y','x','t'/), & 
           longname='diffusive element(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='phierror', dims=(/'z','y','x','t'/), & 
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
      zyx_Phi = zyx_tee(tee_Phi)

      if ( n .ge. 0 ) then
         zyx_PhiSol = zyx_PhiInit &
              * exp( - kappa * (   (2*PI*l/XD)**2 + (2*PI*m/YD)**2 &
                                 +((2*n+1)*PI/ZD)**2   )*time )
      else
         zyx_PhiSol = zyx_PhiInit &
              * exp( - kappa * (  (2*PI*l/XD)**2 + (2*PI*m/YD)**2 &
                                 +(2*PI*n/ZD)**2     )*time )
      endif

      zyx_PhiError = zyx_Phi-zyx_PhiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(time))
      call HistoryPut('phi',zyx_Phi)
      call HistoryPut('phisol',zyx_PhiSol)
      call HistoryPut('phierror',zyx_PhiError)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! ヒストリー出力終了処理
      !
      call HistoryClose
    end subroutine output_gtool4_close

end program tee_module_DD_cn_diff
