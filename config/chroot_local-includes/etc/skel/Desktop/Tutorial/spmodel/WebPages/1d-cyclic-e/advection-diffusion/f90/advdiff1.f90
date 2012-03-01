!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/gt4f90io and ISPACK   2002/09/03 S.Takehiro
!                                                      2004/01/26 M.Odaka 
! 
! Solving advection-diffusion equation
!     d\zeta/dt  = -u d\zeta/dx + D d^2\zeta/dx^2
!
! Time integration is performed by the Euler scheme. 
!
program advdiff1

  use ae_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=64              ! 格子点の設定(X)
  integer, parameter :: km=31              ! 切断波数の設定(X)

 !---- 変数 ----
  real(8)            :: g_Zeta(0:im-1)     ! 格子データ
  real(8)            :: e_Zeta(-km:km)    ! スペクトルデータ (IO用)
  real(8)            :: e_ZetaA(-km:km)    ! スペクトルデータ (t+Δt)
  real(8)            :: e_ZetaB(-km:km)    ! スペクトルデータ (t
  real(8)            :: e_ZetaC(-km:km)    ! スペクトルデータ (t-Δt)
  real(8)            :: e_dZeta(-km:km)    ! スペクトルデータ(1ステップ増分)

  real(8)            :: g_Zeta_exact(0:im-1)     ! 解析解

 !---- 座標変数など ----
  real(8), parameter :: xmin=0.0, xmax=3.0

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-4               ! 時間ステップ間隔
  integer, parameter :: nt=1000, ndisp=100    ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: U = 10.0              ! 移流速度
  real(8), parameter :: D = 1.0               ! 拡散係数

  real(8), parameter :: X0=(xmax+xmin)/4.0      ! 初期分布 X 座標
  real(8), parameter :: Zeta0=1.0               ! 初期分布の振幅
  real(8), parameter :: T0=1e-2                 ! 初期分布の幅

  real(8) :: pi                                 ! 円周率
  real(8) :: Time                               ! 時間
  integer :: it                                 ! DO 変数

  pi = 4.0D0*atan(1.0D0)

  call ae_Initial(im,km,xmin,xmax)                ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  g_Zeta= Zeta0 * exp( - (g_X-X0)**2/(4*D*T0) ) / ( 2*sqrt(pi*D*T0) )
  g_Zeta_exact = g_Zeta
  e_Zeta = e_g(g_Zeta)

  e_ZetaA = e_Zeta ; e_ZetaB = e_Zeta ; e_ZetaC = e_Zeta

  it = 0                                             ! DO 変数初期化

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------

  call get_df_eu(dt, e_ZetaC, e_dZeta) ! オイラー法で 1 ステップ計算

  e_ZetaB = e_ZetaC + e_dZeta

  do it=2,nt
     Time = it * dt

     call get_df_ab(dt, e_ZetaB, e_ZetaC, e_dZeta) 
                                         ! 2 level Adams-Bashforth 法

     e_ZetaA = e_ZetaB + e_dZeta

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        g_Zeta = g_e(e_ZetaA)
        g_Zeta_exact = Zeta0 * exp( - (g_X-X0-U*Time)**2/(4*D*(T0+Time)) ) &
                               / ( 2*sqrt(pi*D*(T0+Time)) )
        call output_gtool4
     endif

     e_ZetaC = e_ZetaB ; e_ZetaB = e_ZetaA
     
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

contains
  subroutine get_df_ab(dt, e_ZetaB, e_ZetaC, e_dZeta)  
                                             ! 2-level Adams-Bashforth 法
    real(8),intent(in) :: dt
    real(8),intent(in) :: e_ZetaB(-km:km)    ! スペクトルデータ
    real(8),intent(in) :: e_ZetaC(-km:km)    ! スペクトルデータ
    real(8),intent(out):: e_dZeta(-km:km)    ! スペクトルデータ(1ステップ増分)

    e_dZeta = dt * (1.5 * e_dZeta_e(e_ZetaB) - 0.5 * e_dZeta_e(e_ZetaC))
     
  end subroutine get_df_ab

  subroutine get_df_eu(dt, e_Zeta, e_dZeta)  ! Euler 法による時間積分

    real(8),intent(in) :: dt
    real(8),intent(in) :: e_Zeta(-km:km)     ! スペクトルデータ
    real(8),intent(out):: e_dZeta(-km:km)    ! スペクトルデータ(1ステップ増分)

    e_dZeta = dt * e_dZeta_e(e_Zeta) 
     
  end subroutine get_df_eu

  function e_dZeta_e(e_Zeta)                  ! 1 ステップ増分計算

    real(8),intent(in) :: e_Zeta(-km:km)        ! スペクトルデータ
    real(8)            :: e_dZeta_e(size(e_Zeta))

    e_dZeta_e =  - U * e_Dx_e(e_Zeta) + D * e_Dx_e(e_Dx_e(e_Zeta))

  end function e_dZeta_e 

  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='advdiff1.nc', &
           title='Adv. Diff. equation model (Euler scheme)',&
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club spmodel/davis project',     &
           dims=(/'x','t'/), dimsizes=(/im,0/),             &
           longnames=(/'X-coordinate','time        '/),     &
           units=(/'1','1'/),                               &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('x',g_X)                                  ! 変数出力
    call HistoryAddattr('x','topology','circular')            ! 周期属性
    call HistoryAddattr('x','modulo',xmax-xmin)               ! 周期属性

    call HistoryAddVariable( &                                ! 変数定義
           varname='zeta', dims=(/'x','t'/), & 
           longname='displacement', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='zeta_exact', dims=(/'x','t'/), & 
           longname='displacement(exact)', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(Time))
    call HistoryPut('zeta',g_Zeta)
    call HistoryPut('zeta_exact',g_Zeta_exact)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program advdiff1

