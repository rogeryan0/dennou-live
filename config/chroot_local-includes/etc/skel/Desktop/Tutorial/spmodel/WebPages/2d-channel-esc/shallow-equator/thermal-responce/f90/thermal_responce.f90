!----------------------------------------------------------------------
!     Copyright (c) 2004--2005 YAMADA YUKIKO. All rights reserved.
!----------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)
!
! 
! Solving equatorial beta plane non-linear shallow water equations
!
! history: 2003/06/02 YAMADA, Y. 
!          2004/08/26 YAMADA, Y. 
!          2005/03/16 Takehiro, S.
!

  program equatorial_beta_nonlinear_shallow_water
    use esc_module
    use gt4_history
    use gt4f90io
    implicit none

 !---- 空間解像度設定 ----
    integer, parameter :: km=16,   lm=16          ! 切断波数の設定(X,Y)
    integer, parameter :: im=64,   jm=32          ! 格子点の設定(X,Y)

 !---- 変数 ----
    real(8)            :: yx_U(0:jm,0:im-1)        ! 格子データ(U(t+Δt))
    real(8)            :: yx_V(0:jm,0:im-1)        ! 格子データ(V(t+Δt))
    real(8)            :: yx_H(0:jm,0:im-1)        ! 格子データ(H(t+Δt))
    real(8)            :: ec_U_A(-km:km,0:lm)      ! スペクトル(U(t+Δt))
    real(8)            :: es_V_A(-km:km,lm)        ! スペクトル(V(t+Δt))
    real(8)            :: ec_H_A(-km:km,0:lm)      ! スペクトル(H(t+Δt))
    real(8)            :: ec_U(-km:km,0:lm)     ! スペクトル(U(t))
    real(8)            :: es_V(-km:km,lm)       ! スペクトル(V(t))
    real(8)            :: ec_H(-km:km,0:lm)     ! スペクトル(H(t))
    real(8)            :: ec_U_B(-km:km,0:lm)   ! スペクトル(U(t-Δt))
    real(8)            :: es_V_B(-km:km,lm)     ! スペクトル(V(t-Δt))
    real(8)            :: ec_H_B(-km:km,0:lm)   ! スペクトル(H(t-Δt))
    real(8)            :: yx_QH(0:jm,0:im-1)     ! 格子データ(熱源分布)
    real(8)            :: ec_QH(-km:km,0:lm)     ! スペクトル(熱源分布)

 !---- 座標変数など ----
    real(8), parameter :: xmin=-6.0, xmax=12.0  ! 領域範囲(X 方向)
    real(8), parameter :: ymin=-4.5, ymax=4.5   ! 領域範囲(Y 方向) 

 !---- 物理パラメター ----
    real(8), parameter :: Grav=1.0              ! 重力加速度
    real(8), parameter :: Beta=1.0              ! 赤道βパラメタ
    real(8), parameter :: H0 = 1.0              ! 平均の深さ
    real(8), parameter :: Rm = 0.1              ! レイリー摩擦係数
    real(8), parameter :: Rh = 0.1              ! 平均ニュートン冷却係数
    real(8), parameter :: Num = 0.02            ! 粘性拡散係数
    real(8), parameter :: Nuh = 0.02            ! 熱拡散係数
    real(8), parameter :: Q0=0.01               ! 熱源分布の振幅
    real(8), parameter :: A0=2.0                ! 熱源分布の Y 方向の幅
    real(8), parameter :: Xrange=2.0            ! 熱源分布の X 方向の幅

 !---- 時間積分パラメター ----
    real(8), parameter :: dt=2e-2               ! 時間ステップ間隔  
    real(8), parameter :: tfil=0.05             ! 時間フィルター係数
    integer, parameter :: nt=1000, ndisp=10     ! 時間積分数, 表示ステップ

 !---- 作業配列 ----
    integer :: i, j, it

    real(8) :: pi                                 ! 円周率
    pi = atan(1.0D0)*4.0D0

    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax) ! ISPACK 初期化 


 !--------------- 熱強制/初期変位 分布 -----------------------

    do i=0,im-1
       do j=0,jm
          if ( abs(yx_X(j,i)) .le. Xrange ) then
             yx_QH(j,i) = Q0 * exp( -yx_Y(j,i)**2/A0 )   &
                             * cos(pi*yx_X(j,i)/(2*Xrange))
          else
             yx_QH(j,i) = 0.0
          endif
       enddo
    enddo

 !------------------- 初期値設定 ----------------------------

    ! 初期値は静止流体, 強制あり
    yx_H = 0.0   ! 平均の深さ
    yx_U = 0.0
    yx_V = 0.0

    ! 初期値にガウス分布, 強制なし
!    yx_H = yx_QH + H0
!    yx_U = 0.0
!    yx_V = 0.0
!    yx_QH = 0.0
    
 !---------- スペクトル変換 / 1 ステップ目設定 ---------------

    ec_U_A = ec_yx(yx_U)
    es_V_A = es_yx(yx_V)
    ec_H_A = ec_yx(yx_H)
    ec_QH  = ec_yx(yx_QH)

    ! 1 ステップ目はオイラー法で時間積分
    ec_H_B = ec_H_A ; ec_H = ec_H_A
    ec_U_B = ec_U_A ; ec_U = ec_U_A
    es_V_B = es_V_A ; es_V = es_V_A

 !------------------------------------------------------------

    call output_gtool4_init                       ! ヒストリー初期化
    call output_gtool4                            ! 初期値出力

 !----------- 時間積分[leapflog and timefilter] ------------

    do it=1,nt

       ec_U_A = ec_U_B                                         &
&           + 2.0 * dt * (                                     &
&                  - ec_yx(yx_ec(ec_U)*yx_ec(ec_Dx_ec(ec_U)))  &
&                  - ec_yx(yx_es(es_V)*yx_es(es_Dy_ec(ec_U)))  &
&                  + ec_yx( Beta * yx_Y * (yx_es(es_V)))       &
&                  - Grav * ec_dx_ec(ec_H)                     &
&                  + Num * ( ec_Dx_ec(ec_Dx_ec(ec_U_B))        &
&                          + ec_Dy_es(es_Dy_ec(ec_U_B)) )      &
&                  - Rm * ec_U_B                               &
&                    )

       es_V_A = es_V_B                                         &
&           + 2.0 * dt * (                                     &
&                  - es_yx(yx_ec(ec_U)*yx_es(es_Dx_es(es_V)))  & 
&                  - es_yx(yx_es(es_V)*yx_ec(ec_Dy_es(es_V)))  &
&                  - es_yx( Beta * yx_Y * (yx_ec(ec_U)))       &
&                  - Grav * es_dy_ec(ec_H)                     &
&                  + Num * ( es_Dx_es(es_Dx_es(es_V_B))        &
&                          + es_Dy_ec(ec_Dy_es(es_V_B)) )      &
&                  -Rm * es_V_B                                &
&                   )

       ec_H_A = ec_H_B                                         &
&           + 2.0 * dt * (                                     &
!                  - (ec_dx_ec(ec_U) + ec_dy_es(es_V))  &
&                  - ec_dx_ec(ec_yx((yx_ec(ec_H)+H0)*yx_ec(ec_U)))  &
&                  - ec_dy_es(es_yx((yx_ec(ec_H)+H0)*yx_es(es_V)))  &
&                  - ec_QH                                     &
&                  + Nuh * ( ec_Dx_ec(ec_Dx_ec(ec_H_B))        &
&                          + ec_Dy_es(es_Dy_ec(ec_H_B)) )      &
&                  - Rh * ec_H_B                               &
&                    )


       ! 時間フィルター
       ec_U_B = (1.0 - 2.0 * tfil) * ec_U + tfil * (ec_U_B + ec_U_A)
       ec_U   = ec_U_A
       
       ec_H_B = (1.0 - 2.0 * tfil) * ec_H + tfil * (ec_H_B + ec_H_A)
       ec_H   = ec_H_A
       
       es_V_B = (1.0 - 2.0 * tfil) * es_V + tfil * (es_V_B + es_V_A)
       es_V   = es_V_A
       

       ! 出力
       if(mod(it,ndisp) .eq. 0)then  
          call output_gtool4
       endif

    enddo

    call output_gtool4_close
    stop

  contains

 !----------------------- gtool4 設定 ----------------------------
    subroutine output_gtool4_init
      call HistoryCreate( &                             ! ヒストリー作成
           file='thermal_responce.nc', &
           title='Equatorial beta-plane non-linear shallow water model ', &
           source='A sample program of ISPACK and gtool_history/gtool4',  &
           institution='GFD_Dennou Club spmodel project',                 &
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),                  &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),    &
           units=(/'-','-','-'/),                                         &
           origin=0.0, interval=real( (ndisp)*dt  )      &
                         ) 

      call HistoryPut('x',x_X)                                   ! 変数出力
      call HistoryAddattr('x','topology','circular')             ! 周期属性
      call HistoryAddattr('x','modulo',xmax-xmin)                ! 周期属性
      call HistoryPut('y',y_Y)                                   ! 変数出力

      call HistoryAddVariable( &                                 ! 変数定義
           varname='uvel', dims=(/'x','y','t'/), & 
           longname='X-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! 変数定義
           varname='vvel', dims=(/'x','y','t'/), & 
           longname='Y-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! 変数定義
           varname='height', dims=(/'x','y','t'/), & 
           longname='height', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! 変数定義
           varname='wvel', dims=(/'x','y','t'/), & 
           longname='Z-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! 変数定義
           varname='hdiv', dims=(/'x','y','t'/), & 
           longname='Horizontal divergence', units='1', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4
      write(6,*) 'it = ',it
      yx_U   = yx_ec(ec_U_A)
      yx_V   = yx_es(es_V_A)
      yx_H   = yx_ec(ec_H_A)

      call HistoryPut('t',real( it*dt ) )
      call HistoryPut('uvel',transpose(yx_U))
      call HistoryPut('vvel',transpose(yx_V))
      call HistoryPut('height',transpose(yx_H))
      call HistoryPut('wvel', &
           transpose(yx_ec(-H0*(ec_dx_ec(ec_U_A)+ec_dy_es(es_V_A))-ec_QH)))
      call HistoryPut('hdiv', &
           transpose(yx_ec(ec_dx_ec(ec_U_A)+ec_dy_es(es_V_A))))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program equatorial_beta_nonlinear_shallow_water
