!----------------------------------------------------------------------
!     Copyright (c) 2004--2005 YAMADA YUKIKO. All rights reserved.
!----------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)   
!
! 
! Solving equatorial beta plane wave-CISK equations
!
! history: 2004/06/23 YAMADA, Y. 作成
!          2004/06/27 YAMADA, Y. 改造中…
!

  program equatorial_beta_wavecisk
    use esc_module
    use gt4_history
    use gt4f90io
    implicit none

 !---- 空間解像度設定 ----
    integer, parameter :: km=16,   lm=16, mm = 3     ! 切断波数の設定(X,Y,Z)
    integer, parameter :: im=64,   jm=32, vm = mm+1  ! 格子点の設定(X,Y,Z)

!    integer, parameter :: km=64,   lm=64, mm = 3  ! 切断波数の設定(X,Y,Z)
!    integer, parameter :: im=200,   jm=100, vm = mm+1  ! 格子点の設定(X,Y,Z)

 !---- 加熱のパラメタリゼーション設定 ----
    
    ! wave-CISK パラメタリゼーション: TRUE=上昇加熱型, FALSE=線形加熱型
    logical            :: positive_only_heat_parameta = .FALSE.

 !---- 変数 ----
    real(8)            :: xyz_U(0:jm,0:im-1,vm)     ! 格子データ(U(t+Δt))
    real(8)            :: xyz_V(0:jm,0:im-1,vm)     ! 格子データ(V(t+Δt))
    real(8)            :: xyz_H(0:jm,0:im-1,vm)     ! 格子データ(H(t+Δt))
    real(8)            :: xym_U(0:jm,0:im-1,mm)     ! 格子データ(U(t+Δt))
    real(8)            :: xym_V(0:jm,0:im-1,mm)     ! 格子データ(V(t+Δt))
    real(8)            :: xym_H(0:jm,0:im-1,mm)     ! 格子データ(H(t+Δt))
    real(8)            :: ecm_U(-km:km,0:lm,mm)   ! スペクトル(U(t))
    real(8)            :: esm_V(-km:km,lm,mm)     ! スペクトル(V(t))
    real(8)            :: ecm_H(-km:km,0:lm,mm)   ! スペクトル(H(t))
    real(8)            :: ecm_U_B(-km:km,0:lm,mm) ! スペクトル(U(t-Δt))
    real(8)            :: esm_V_B(-km:km,lm,mm)   ! スペクトル(V(t-Δt))
    real(8)            :: ecm_H_B(-km:km,0:lm,mm) ! スペクトル(H(t-Δt))
    real(8)            :: ec_QH(-km:km,0:lm)      ! スペクトル(熱源分布)
    real(8)            :: ec_QH_A(-km:km,0:lm)    ! スペクトル(熱源分布)
    real(8)            :: gamma(mm)               ! 鉛直モード固有値^0.5 
    real(8)            :: eig(mm,0:vm)            ! 鉛直モード固有関数
    real(8)            :: eta(mm)               ! 加熱の鉛直分布

 !---- 座標変数など ----
    real(8), parameter :: xmin=-10.0, xmax=10.0  ! 領域範囲(X 方向)
!    real(8), parameter :: xmin=-25.0, xmax=25.0  ! 領域範囲(X 方向)
    real(8), parameter :: ymin=-5.0, ymax=5.0   ! 領域範囲(Y 方向) 

 !---- 物理パラメター ----
    real(8), parameter :: Grav=1.0              ! 重力加速度
    real(8), parameter :: Beta=1.0              ! 赤道βパラメタ
    real(8), parameter :: H0 = 1.0              ! 平均の深さ
    real(8), parameter :: Rm = 0.0              ! レイリー摩擦係数
    real(8), parameter :: Rt = 0.0              ! 平均ニュートン冷却係数
    real(8), parameter :: Num = 0.02            ! 粘性拡散係数
    real(8), parameter :: Nut = 0.02            ! 熱拡散係数
    real(8), parameter :: Q0=1.0                ! 初期熱源分布の振幅

    real(8), parameter :: AX0=0.5                ! 初期熱源分布の半値幅
!    real(8), parameter :: AX0=0.5*4*2.5              ! 初期熱源分布の半値幅

    real(8), parameter :: AY0=0.5              ! 初期熱源分布の半値幅

 !---- 時間積分パラメタ ----
    real(8), parameter :: dt=2e-2               ! 時間ステップ間隔  
    real(8), parameter :: tfil=0.05             ! 時間フィルター係数
    integer, parameter :: nt=1500, ndisp=20     ! 時間積分数, 表示ステップ
!    integer, parameter :: nt=3000, ndisp=20     ! 時間積分数, 表示ステップ

 !---- 作業配列 ----
    integer :: i, j, it, m

 !---- スペクトル設定 ----
    
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax) ! ISPACK 初期化 
    call vmode_initial( eig, gamma ) ! 鉛直モード固有値固有関数の設定
    
 !-------- 初期値設定 -----------

    ! 初期値は静止流体
!    xyz_H = H0   ! 平均の深さ
    xyz_H = 0.0   ! 平均の深さ
    xyz_U = 0.0
    xyz_V = 0.0

!    xym_H = H0   ! 平均の深さ
    xym_H = 0.0   ! 平均の深さ
    xym_U = 0.0
    xym_V = 0.0


    ! 加熱の鉛直モードの設定
    eta(1) = 1.5 ;  eta(2) = -1.5               
    do m=1,mm  
       eta(m) = eta(m) / eig(m,1) * gamma(m) / vm  * (2**0.5)  
    enddo

    ! 初期熱源分布 (第一モードのみ加熱を与える)
    do i=0,im-1
       do j=0,jm
          xym_H(j,i,1) = - Q0          &
               * exp( -xy_Y(j,i)**2/AY0 )   &
               * exp( -xy_X(j,i)**2/AX0 )   
       enddo
    enddo

    ! 水平スペクトル変換
    do m = 1, mm 
       ecm_U(:,:,m) = ec_xy(xym_U(:,:,m) )
       esm_V(:,:,m) = es_xy(xym_V(:,:,m) )
       ecm_H(:,:,m) = ec_xy(xym_H(:,:,m) )
    enddo

 !------------------------------------------------------------

    call output_gtool4_init                 ! ヒストリー初期化
    call output_gtool4                      ! 初期値出力

 !----------------------- 積分 main ----------------------------

    ! 1 ステップ目はオイラー法で時間積分
    ecm_H_B = ecm_H
    ecm_U_B = ecm_U
    esm_V_B = esm_V

    do it=1,nt

       ! 鉛直流 (加熱項) 初期化
       ec_QH_A = 0.0

       do m = 1, mm 

          ! 時間積分
          call time_integral( gamma(m), eta(m), ec_QH,         &
               ecm_U(:,:,m),   esm_V(:,:,m),   ecm_H(:,:,m),   &
               ecm_U_B(:,:,m), esm_V_B(:,:,m), ecm_H_B(:,:,m)  )

          ! 鉛直流 (加熱項) の計算
          ec_QH_A = ec_QH_A                                    &
               - (ec_Dx_ec(ecm_U(:,:,m)) + ec_Dy_es(esm_V(:,:,m)))

       enddo

       ec_QH   = ec_QH_A

       ! 上昇流域のみ加熱の場合
       if (positive_only_heat_parameta .AND. .TRUE. )  then 
          call positive_only_heat(ec_QH)
       endif

       ! 出力
       if(mod(it,ndisp) .eq. 0) then 
          call output_gtool4
       endif

    enddo

    call output_gtool4_close
    stop

  contains

 !----------- 時間積分[leapflog and timefilter] ------------

    subroutine time_integral(  gamma_m, eta_m, ec_QH, &
         ec_U,es_V,ec_H,ec_U_B,es_V_B,ec_H_B)
      
      real(8)              :: ec_U_A(-km:km,0:lm)   ! スペクトル(U(t+Δt))
      real(8)              :: es_V_A(-km:km,lm)     ! スペクトル(V(t+Δt))
      real(8)              :: ec_H_A(-km:km,0:lm)   ! スペクトル(H(t+Δt))
      real(8),intent(inout):: ec_U(-km:km,0:lm)     ! スペクトル(U(t))
      real(8),intent(inout):: es_V(-km:km,lm)       ! スペクトル(V(t))
      real(8),intent(inout):: ec_H(-km:km,0:lm)     ! スペクトル(H(t))
      real(8),intent(inout):: ec_U_B(-km:km,0:lm)   ! スペクトル(U(t-Δt))
      real(8),intent(inout):: es_V_B(-km:km,lm)     ! スペクトル(V(t-Δt))
      real(8),intent(inout):: ec_H_B(-km:km,0:lm)   ! スペクトル(H(t-Δt))
      real(8),intent(in)   :: gamma_m                 ! 鉛直モード固有値^0.5
      real(8),intent(in)   :: eta_m                 ! 鉛直加熱分布
      real(8),intent(in)   :: ec_QH(-km:km,0:lm)     ! スペクトル(熱源分布)


       ec_U_A = ec_U_B                                         &
&           + dt * 2.0 * (                                     &
!&                  - ec_xy(xy_ec(ec_U)*xy_ec(ec_Dx_ec(ec_U)))  &
!&                  - ec_xy(xy_es(es_V)*xy_es(es_Dy_ec(ec_U)))  &
&                  + ec_xy( Beta * xy_Y * (xy_es(es_V)))       &
&                  - Grav * ec_dx_ec(ec_H)                     &
&                  + Num * ( ec_Dx_ec(ec_Dx_ec(ec_U_B))        &
&                          + ec_Dy_es(es_Dy_ec(ec_U_B)) )      &
&                  - Rm * ec_U                                 &
&                    )

       es_V_A = es_V_B                                         &
&           + dt * 2.0 * (                                     &
!&                  - es_xy(xy_ec(ec_U)*xy_es(es_Dx_es(es_V)))  & 
!&                  - es_xy(xy_es(es_V)*xy_ec(ec_Dy_es(es_V)))  &
&                  - es_xy( Beta * xy_Y * (xy_ec(ec_U)))       &
&                  - Grav * es_dy_ec(ec_H)                     &
&                  + Num * ( es_Dx_es(es_Dx_es(es_V_B))        &
&                          + es_Dy_ec(ec_Dy_es(es_V_B)) )      &
&                  -Rm * es_V                                  &
&                   )

       ec_H_A = ec_H_B                                         &
&           + dt * 2.0 * (                                     &
&                  + 1.0 / gamma_m **2 * (                     &
&                      - H0*(ec_dx_ec(ec_U) + ec_dy_es(es_V))  &
!&                    - ec_dx_ec(ec_xy(xy_ec(ec_H)*xy_ec(ec_U))) &
!&                    - ec_dy_es(es_xy(xy_ec(ec_H)*xy_es(es_V))) &
&                    - eta_m * ec_QH                           &
&                                    )                         &
&                  + Nut  ( ec_Dx_ec(ec_Dx_ec(ec_H_B))         &
&                         + ec_Dy_es(es_Dy_ec(ec_H_B)) )       &
&                  - Rt * ec_H                                 &
&                    )


       ! 時間フィルター
       ec_U_B = (1.0 - 2.0 * tfil) * ec_U + tfil * (ec_U_B + ec_U_A)
       ec_U = ec_U_A
       
       ec_H_B = (1.0 - 2.0 * tfil) * ec_H + tfil * (ec_H_B + ec_H_A)
       ec_H = ec_H_A
       
       es_V_B = (1.0 - 2.0 * tfil) * es_V + tfil * (es_V_B + es_V_A)
       es_V = es_V_A
       

     end subroutine time_integral


     ! 上昇流域のみ加熱の場合
     subroutine positive_only_heat(ec_QH)
       
       real(8),intent(inout):: ec_QH(-km:km,0:lm)      ! スペクトル(熱源分布)
       real(8)              :: xy_QH(0:jm,0:im-1)      ! 格子データ(熱源分布)
       
       xy_QH = xy_ec(ec_QH)
       
       do i=0,im-1
          do j=0,jm
             xy_QH(j,i) = max(0,xy_QH(j,i))
          enddo
       enddo
       
       ec_QH   = ec_xy(xy_QH)
     end subroutine positive_only_heat


     ! 鉛直モードの固有値, 固有関数の設定
     subroutine vmode_initial( eig, gamma )
       
       real(8),intent(out):: gamma(mm)         ! 鉛直モード固有値^0.5 
       real(8),intent(out):: eig(mm,0:vm)      ! 鉛直モード固有関数
       
       integer :: j, m
       real(8) :: pi                                 ! 円周率
       pi = atan(1.0D0)*4.0D0
       
       ! 固有関数 上下端で 0
       eig(:,0) = 0.0 ; eig(:,vm) = 0.0 
       do m=1,mm 
          ! 固有値^0.5 の計算
          gamma(m) = vm * ( 2 - 2 * cos( m*pi/(mm+1) ) )**0.5
          
          ! 固有関数 (規格化してない) 計算
          do j=1,vm-1
             eig(m,j)   = sin ( (m*j*pi)/(mm+1) )
          enddo
          
       enddo
       
     end subroutine vmode_initial
     
     ! 鉛直モード逆変換 (u,v,phi)
     subroutine inv_vmode_transpote(xym_data, xyz_data)
       
       real(8),intent(in) :: xym_data(0:jm,0:im-1,mm)  ! 鉛直モードデータ
       real(8),intent(out):: xyz_data(0:jm,0:im-1,vm)  ! 鉛直格子データ
      
       xyz_data = 0.0 
       do j=1,vm
          do m=1,mm   
             xyz_data(:,:,j) = xyz_data(:,:,j)                  &
                  + ( eig(m,j) - eig(m,j-1) )                   &
                  * xym_data(:,:,m) * vm / gamma(m) / (2**0.5)       
          enddo
       enddo
      
     end subroutine inv_vmode_transpote


 !----------------------- gtool4 設定 ----------------------------
    subroutine output_gtool4_init

      real(8)  :: vm_ary(vm)  ! 鉛直層の番号付
      
      do j = 1,vm
         vm_ary(j) = j * 1.0
      enddo

      ! ヒストリー作成
      call HistoryCreate(                                                 &
           file='wavecisk_test.nc',                                       & 
           title='Equatorial beta-plane wave-CISK model ',                &
           source='A sample program of ISPACK and gtool_history/gtool4',  &
           institution='GFD_Dennou Club spmodel project',                 &
           dims=(/'x','y','z','t'/), dimsizes=(/im,jm+1,vm,0/),           &
           longnames=(/'X-coordinate','Y-coordinate',                     &
           'Z-level     ','time        '/),                               &
           units=(/'1','1','1','1'/),                                     &
           origin=0.0, interval=real( (ndisp)*dt  )                       &
           ) 

      call HistoryPut('x',x_X)                                   ! 変数出力
      call HistoryAddattr('x','topology','circular')             ! 周期属性
      call HistoryAddattr('x','modulo',xmax-xmin)                ! 周期属性
      call HistoryPut('y',y_Y)                                   ! 変数出力
      call HistoryPut('z',vm_ary)                           ! 変数出力

      call HistoryAddVariable( &                                 ! 変数定義
           varname='uvel', dims=(/'x','y','z','t'/), & 
           longname='X-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! 変数定義
           varname='vvel', dims=(/'x','y','z','t'/), & 
           longname='Y-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! 変数定義
           varname='height', dims=(/'x','y','z','t'/), & 
           longname='height', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! 変数定義
           varname='wvel', dims=(/'x','y','t'/), & 
           longname='Z-velocity_(3/2)', units='1', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4

      real(8)  :: xyz_tr_U(0:im-1,0:jm,vm) ! 格子データ (X-Y 軸入れ換え)
      real(8)  :: xyz_tr_V(0:im-1,0:jm,vm) ! 格子データ (X-Y 軸入れ換え)
      real(8)  :: xyz_tr_H(0:im-1,0:jm,vm) ! 格子データ (X-Y 軸入れ換え)
      
      write(6,*) 'it = ',it

      ! 水平スペクトル逆変換
      do m = 1, mm 
         xym_U(:,:,m) = xy_ec(ecm_U(:,:,m)) 
         xym_V(:,:,m) = xy_es(esm_V(:,:,m)) 
         xym_H(:,:,m) = xy_ec(ecm_H(:,:,m)) 
      enddo

      ! 鉛直モード逆変換
      call inv_vmode_transpote(xym_U, xyz_U)
      call inv_vmode_transpote(xym_V, xyz_V)
      call inv_vmode_transpote(xym_H, xyz_H)

      ! X-Y 軸入れ換え
      do m = 1, vm 
         xyz_tr_U(:,:,m) = transpose(xyz_U(:,:,m))
         xyz_tr_V(:,:,m) = transpose(xyz_V(:,:,m))
         xyz_tr_H(:,:,m) = transpose(xyz_H(:,:,m))
      enddo

      call HistoryPut('t',real( it*dt ) )
      call HistoryPut('uvel', xyz_tr_U )
      call HistoryPut('vvel', xyz_tr_V )
      call HistoryPut('height', xyz_tr_H )
      call HistoryPut('wvel', transpose( xy_ec(ec_QH_A)) )

    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program equatorial_beta_wavecisk
