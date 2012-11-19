!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPMODEL 3 次元平板領域非圧縮流体モデル
!      チェビシェフ選点法による計算
!
!      時間積分 : Crank-Nicolson 法
!      上側粘着条件, 下側自由すべり条件
!
! Solving 3-D impompressible fluid system
!     d\nabla^2\phi/dt =  Pr\nabla^2\nabla^2\phi
!     phi = phi' = 0 at z=d/2
!     phi = phi''= 0 at z=-d/2
!
!      計算結果比較のための解析解については解説文書
!
!         「2 次元水路領域での拡散型方程式の解析解」
!
!      (et_diffusion.pdf)を参照のこと. 
!
!履歴  2009/12/29  竹広真一: et_RF_cn_diff.f90 より改造
!      
!
program tee_module_polvel_RF_cn_diff
  !
  ! SPMODEL 3 次元平板領域非圧縮流体モデル
  !      チェビシェフ選点法による計算
  !
  !      時間積分 : Crank-Nicolson 法
  !      上側粘着条件, 下側自由すべり条件
  !
  ! Solving 3-D impompressible fluid system
  !     d\nabla^2\phi/dt =  Pr\nabla^2\nabla^2\phi
  !     phi = phi' = 0 at z=d/2
  !     phi = phi''= 0 at z=-d/2
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
  use dc_message
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter  :: im=32, jm=32, km=16      ! 格子点の設定(X,Y,Z)
  integer, parameter  :: lm=10, mm=10, nm=16      ! 切断波数の設定(X,Y,Z)

 !---- 物理パラメター ----
  real(8), parameter          :: XD=2.0         ! x 方向領域の大きさ
  real(8), parameter          :: YD=2.0         ! y 方向領域の大きさ
  real(8), parameter          :: ZD=1.0         ! y 方向領域の大きさ

 !---- 座標変数など ----
  real(8), parameter  :: xmin=0, xmax=XD        ! X 座標範囲
  real(8), parameter  :: ymin=0, ymax=YD        ! Y 座標範囲
  real(8), parameter  :: zmin=-ZD/2, zmax=ZD/2  ! Z 座標範囲

 !---- 変数 ----
  real(8)             :: zyx_Phi(0:km,0:jm-1,0:im-1)  ! ポロイダルポテンシャル
  real(8)             :: zyx_LaplaPhi(0:km,0:jm-1,0:im-1) ! 

  real(8)             :: tee_Phi(0:nm,-mm:mm,-lm:lm)      ! スペクトルデータ
  real(8)             :: tee_LaplaPhi(0:nm,-mm:mm,-lm:lm) ! スペクトルデータ

  real(8)             :: zyx_PhiInit(0:km,0:jm-1,0:im-1)  ! 格子データ(初期値)
  real(8)             :: zyx_PhiSol(0:km,0:jm-1,0:im-1)   ! 格子データ(解析解)
  real(8)             :: zyx_PhiError(0:km,0:jm-1,0:im-1) ! 格子データ(誤差)

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10   ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter          :: Pr=1.0         ! プランドル数
  character(len=2), parameter :: VelBC='RF'     ! 速度場境界条件(RR/FF/RF/FR)

 !--- Crank Nicholson 陰的計算用作業変数 ---
  real(8) :: DifLUMT_Phi((2*lm+1)*(2*mm+1),0:nm,0:nm)
                                     ! Crank Nicholson 陰的計算用行列
  integer :: kpivot_Phi((2*lm+1)*(2*mm+1),0:nm) 
                                     ! Crank Nicholson 用行列ピボット情報

 !---- その他 ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  real(8) :: rk, rn
  integer :: l,m,n

 !---------------- 座標値の設定 ---------------------
  call tee_Initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax)

  call CNDiffusionMatrixPol( Pr, dt, DifLUMT_Phi, kpivot_Phi )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Positive l:cos(lx), negative l: sin(lx)'
  write(6,*) 'Positive m:cos(my), negative m: sin(my)'
  write(6,*) 'n: sinh(nz)+sin(nz)'
  write(6,*) 'l,m,n?'
  read(5,*)  l,m,n

  if ( l .ge. 0 .AND. m .ge. 0 ) then
     write(6,*) 'Type of the initial value : cos(lx)*cos(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = cos(2*l*PI/XD*zyx_X) * cos(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  else if ( l .lt. 0 .AND. m .ge. 0 ) then
     write(6,*) 'Type of the initial value : sin(lx)*cos(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = sin(2*l*PI/XD*zyx_X) * cos(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  else if ( l .ge. 0 .AND. m .lt. 0 ) then
     write(6,*) 'Type of the initial value : cos(lx)*sin(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = cos(2*l*PI/XD*zyx_X) * sin(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  else if ( l .lt. 0 .AND. m .lt. 0 ) then
     write(6,*) 'Type of the initial value : sin(lx)*sin(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = sin(2*l*PI/XD*zyx_X) * sin(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  endif

  zyx_PhiInit = zyx_Phi
  
  tee_Phi  = tee_zyx(zyx_Phi)
  tee_LaplaPhi = tee_Lapla_tee(tee_Phi)

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Cn 法による時間積分
     time =it*dt

     tee_LaplaPhi = tee_LaplaPhi + dt/2 *( Pr*tee_Lapla_tee(tee_LaplaPhi)  )
     tee_Phi = tee_LaplaPol2PolGrid_tee(tee_LaplaPhi,cond=VelBC)
     tee_Phi = aee_e2a(LUSolve(DifLUMT_Phi,kpivot_Phi,e2a_aee(tee_Phi)))
     tee_LaplaPhi = tee_Lapla_tee(tee_Phi)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

  contains

    function InvXcotX(val,n)
      !
      ! x*cot(x)=val(>0) の解を求める
      !
      real(8), intent(IN) :: val                ! x*cot(x)=val > 0
      integer, intent(IN) :: n                  ! 求める解の番号
      real(8)             :: InvXcotX
      real(8), parameter  :: eps = 1.0D-14     ! 解の精度

      real(8) :: PI
      real(8) :: xs, xl, xm
      real(8) :: ValS, ValL, ValM

      PI = atan(1.0D0)*4.0D0

      xs=(n+1)*PI  + eps
      xl=PI/2.0D0  + (n+1)*PI

      ValS = xs/tan(xs)-val ; ValL = xl/tan(xl)-val
      if ( ValS * ValL .GT. 0.0D0 ) &
           call MessageNotify('E','InvXcotX',&
           'Initial values of ValS and ValL are the same sign.')
!!$          write(6,*) 'vals, vall',vals, vall
1000  xm = (xs + xl)/2.0
      ValM = xm/tan(xm) - val
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=xs/tan(xs)-val
      else
         xl = xm ; ValL=xl/tan(xl)-val
      endif

      if ( abs(xl-xs) .lt. eps ) then
         InvXcotX = xm
         goto 99
      endif

      goto 1000

99    continue
    end function InvXcotX

   !------------------- 拡散項(流線関数用) ----------------------
    subroutine CNDiffusionMatrixPol( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (流線関数用, ▽^{-2}[▽^2-D dt/2▽^2▽^2], LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 拡散陰的計算用行列(▽^{-2}(▽^2-D (dt/2)▽^4, LU 分解)
      real(8), intent(OUT) :: DiffLUMatrix((2*lm+1)*(2*mm+1),0:nm,0:nm)     
      integer, intent(OUt) :: kpivot((2*lm+1)*(2*mm+1),0:nm)

      real(8)               :: e2t_I((2*lm+1)*(2*mm+1),0:nm)   ! 作業用変数
      real(8)               :: tee_DI(0:nm,-mm:mm,-lm:lm)      ! 作業用変数

      integer :: n

      DiffLUMatrix = 0.0
      do n=0,nm
         e2t_I = 0.0 ; e2t_I(:,n) = 1.0             ! 各波数成分独立
         tee_DI = - Diffc * dt/2.0 * tee_Lapla_tee(tee_Lapla_tee(aee_e2a(e2t_I)))
         tee_DI = tee_LaplaPol2PolGrid_tee(tee_DI,cond=VelBC)
         DiffLUMatrix(:,:,n) = e2t_I + e2a_aee(tee_DI)
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixPol

   !------------------- 出力 ----------------------
    subroutine output_gtool4_init
      !
      ! ヒストリー出力初期化ルーチン
      !
      !   ファイル作成
      !   変数定義
      !
      call HistoryCreate( &                               ! ヒストリー作成
           file='tee_module_polvel_RF_cn_diff.nc', &
           title='3dim. imcompressible fluid', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','z','t'/), dimsizes=(/im,jm,km+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate',  &
                       'Z-coordinate','time        '/),&
           units=(/'1','1','1','1'/),                  &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! 変数出力
      call HistoryAddattr('x','topology','circular')      ! 周期属性
      call HistoryAddattr('x','modulo',xmax-xmin)         ! 周期属性
      call HistoryPut('y',y_Y)                            ! 変数出力
      call HistoryAddattr('y','topology','circular')      ! 周期属性
      call HistoryAddattr('y','modulo',xmax-xmin)         ! 周期属性
      call HistoryPut('z',z_Z)                            ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='phi', dims=(/'z','y','x','t'/), & 
           longname='Poloidal potential', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='laplaphi', dims=(/'z','y','x','t'/), & 
           longname='Laplacian of Poloidal potential', &
           units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='phisol', dims=(/'z','y','x','t'/), & 
           longname='Poloidal potential(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='phierror', dims=(/'z','y','x','t'/), & 
           longname='Poloidal potential(Error)', units='1', xtype='double')

     !---- 実験パラメターを属性として定義, 出力(Global 属性) ----
      call HistoryAddAttr('x','+VelBC', VelBC )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      !
      ! ヒストリー出力
      !
      !   各物理量の gtool4 ファイルへの出力を行う
      !
      zyx_Phi  = zyx_tee(tee_Phi)
      zyx_LaplaPhi = zyx_tee(tee_LaplaPhi)

      zyx_PhiSol = zyx_PhiInit &
           * exp( - Pr * ( rk**2 + rn**2 )*time )

      zyx_PhiError = zyx_Phi-zyx_PhiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('phi',zyx_Phi)
      call HistoryPut('laplaphi',zyx_LaplaPhi)
      call HistoryPut('phisol',zyx_PhiSol)
      call HistoryPut('phierror',zyx_PhiError)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! ヒストリー出力終了処理
      !
      call HistoryClose
    end subroutine output_gtool4_close
    
end program tee_module_polvel_RF_cn_diff
