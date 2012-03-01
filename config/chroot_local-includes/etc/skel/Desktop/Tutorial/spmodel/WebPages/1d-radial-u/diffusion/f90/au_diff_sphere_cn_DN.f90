!----------------------------------------------------------------------
!   Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module サンプルプログラム
!
!      拡散方程式 d temp/dt = kappa 1/r^2 d/dr (r^2 d temp/dr)
!
!      Crank-Nicolson スキーム
!
!履歴  2007/12/28  竹広真一
!      2007/12/31  竹広真一  解析解との比較
!      2008/01/01  竹広真一  ノイマン条件解追加
!
program au_diff_sphere_cn_DN

  use lumatrix
  use dc_message
  use au_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32                ! 格子点の設定
  integer, parameter :: km=32               ! 切断波数の設定

 !---- 座標変数など ----
  real(8), parameter :: ra=2.0D0            ! 半径

 !---- 変数 ----
  real(8), dimension(0:im)  :: g_temp
  real(8), dimension(0:km)  :: u_temp

  real(8), dimension(0:im)  :: g_temp_analytic    ! 解析解

 ! Crank Nicholson 陰的計算用拡散行列(温度)
  real(8)             :: DifLUMT_Temp(0:km,0:km)
  integer             :: kpivot_Temp(0:km)        ! ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                   ! 時間ステップ間隔
  integer, parameter :: nt=1000, ndisp=100    ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8)            :: tempbndry=0.0           ! 境界値
  character(len=1)   :: tempbc='D'              ! 境界条件(D/N)

  real(8), parameter :: kappa=1.0               ! 拡散係数
  integer :: norder=2                           ! 固有関数の次数
 
  real(8) :: time, pi, alpha
  integer :: it=0

  pi = 4.0D0 * ATAN(1.0D0)

 !---------------- 座標値の設定 ---------------------
  call au_initial(im,km,ra,(/0/))

 !------------------- 初期値設定 ----------------------
  write(6,*)'BC? (D)richlet or (N)euman'
  read(5,*) tempbc

  write(6,*)'BC value?'
  read(5,*) tempbndry

  write(6,*)'norder?'
  read(5,*)norder

  call CNDiffusionMatrix( kappa, dt, DifLUMT_Temp, kpivot_Temp )
 
  if ( tempbc == 'D' ) then
     g_temp = sin(norder*pi/ra*g_R)/(norder*pi/ra*g_R) + tempbndry ! 固有モード
  else if ( tempbc == 'N' ) then
     alpha=CalAlpha(norder)
     g_temp = sin(alpha/ra*g_R)/(alpha/ra*g_R)  ! 固有モード
  else
     call MessageNotify('E','main', 'B.C. not supported')
  endif
  u_temp = u_g(g_temp)

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt
     time = it * dt
     u_temp = u_temp  + dt/2 * kappa &
                      * u_g(1.0D0/g_R** 2* g_Dr_u(u_g(g_R**2*g_Dr_u(u_temp))))

     if ( tempbc == 'D' ) then
        call au_Boundary_D(u_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call au_Boundary_N(u_temp,tempbndry)
     else
        call MessageNotify('E','main', 'B.C. not supported')
     endif

     u_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,u_Temp)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif

  enddo
  call output_gtool4_close                           ! ヒストリー後処理

  stop

  contains

 !------------------- 拡散項 ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
    real(8), dimension(0:km,0:km), intent(OUT)  :: DiffLUMatrix
    ! ピボット情報
    integer, dimension(0:km), intent(OUT)       :: kpivot

    ! 作業用変数
    real(8), dimension(0:km)           :: u_I
    real(8), dimension(0:km)           :: u_DI

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,km
       u_I = 0.0 ; u_I(l) = 1.0             ! 各波数成分独立
       u_DI =  - Diffc * dt/2.0 &
                 * u_g(1.0D0/g_R** 2* g_Dr_u(u_g(g_R**2*g_Dr_u(u_I))))


       if ( tempbc == 'D' ) then
          call au_Boundary_D(u_DI)          ! 変化部分は境界値を変えない
       else if ( tempbc == 'N' ) then
          call au_Boundary_N(u_DI)          ! 変化部分は境界値を変えない
       else
          call MessageNotify('E','main', 'B.C. not supported')
       endif

       DiffLUMatrix(:,l) = u_I + u_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !-----------------------------------------
 ! x sin(x)- cos(x) の解を求める
 !
  function CalAlpha(n)
    integer, intent(IN) :: n                 ! 求める解の番号
    real(8)             :: CalAlpha
    real(8), parameter  :: eps = 1.0D-14     ! 解の精度

    real(8) :: PI
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    PI = atan(1.0D0)*4.0D0

    xs=PI/2.0D0  + n*PI
    xl=PI/2.0D0  + (n+1)*PI

    ValS = xs*sin(xs) - cos(xs)
    ValL = xl*sin(xl) - cos(xl)
    if ( ValS * ValL .GT. 0.0D0 ) &
         call MessageNotify('E','InvXtanX',&
         'Initial values of ValS and ValL are the same sign.')
!!$    write(6,*) 'vals, vall',vals, vall
10  xm = (xs + xl)/2.0
    ValM = xm*sin(xm) - cos(xm)
    
    if ( ValS * ValM .GT. 0.0D0 ) then
       xs = xm ; ValS= xs*sin(xs) - cos(xs)
    else
       xl = xm ; ValL=xl*sin(xl) - cos(xl)
    endif

    if ( abs(xl-xs) .lt. eps ) then
       CalAlpha = xm
       goto 9
    endif

    goto 10

9   continue
  end function CalAlpha

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                               ! ヒストリー作成
         file='au_diff_sphere_cn_DN.nc', &
         title='Diffusion equation in a sphere', &
         source='Sample program of gtool_history/gtool4', &
         institution='GFD_Dennou Club davis/spmodel project',&
         dims=(/'r','t'/), dimsizes=(/im+1,0/),    &
         longnames=(/'radial coordinate',&
                     'time             '/),&
         units=(/'1','1'/),                           &
         origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('r',g_R)                            ! 変数出力

    call HistoryAddVariable( &                          ! 変数定義
         varname='temp', dims=(/'r','t'/), & 
         longname='temperature', units='1', xtype='double')

    call HistoryAddVariable( &                          ! 変数定義
         varname='temp_analytic', dims=(/'r','t'/), & 
         longname='temperature(analytic)', units='1', xtype='double')

    call HistoryAddVariable( &                          ! 変数定義
         varname='temp_error', dims=(/'r','t'/), & 
         longname='temperature(error)', units='1', xtype='double')

  end subroutine output_gtool4_init

  subroutine output_gtool4
    g_temp = g_u(u_temp)
    if ( tempbc == 'D' ) then
       g_temp_analytic = sin(norder*pi/ra*g_R)/(norder*pi/ra*g_R) &
                         *exp(-(norder*pi/ra)**2*time) + tempbndry
    else if ( tempbc == 'N' ) then
       g_temp_analytic = sin(alpha/ra*g_R)/(alpha/ra*g_R) &
                         *exp(-(alpha/ra)**2*time) 
    else
          call MessageNotify('E','main', 'B.C. not supported')
    endif

    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',g_temp)
    call HistoryPut('temp_analytic',g_temp_analytic)
    call HistoryPut('temp_error',g_temp_analytic-g_temp)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program au_diff_sphere_cn_DN
