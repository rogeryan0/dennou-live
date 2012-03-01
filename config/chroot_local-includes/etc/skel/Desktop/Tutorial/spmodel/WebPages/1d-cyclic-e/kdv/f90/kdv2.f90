!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/gt4f90io and ISPACK   2002/09/03 S.Takehiro
!                                                      2004/01/26 M.Odaka
! 
! Solving KdV equation
!     d\zeta/dt  = -\zeta d\zeta/dx + d^3\zeta/dx^3
!
! Time integration is performed by the second order Adams-Bashforth scheme. 
!
program kdv

  use ae_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=128            ! 格子点の設定(X)
  integer, parameter :: km=42             ! 切断波数の設定(X)

 !---- 変数 ----
  real(8)            :: g_Zeta(0:im-1)     ! 格子データ
  real(8)            :: e_Zeta(-km:km)     ! スペクトルデータ
  real(8)            :: e_Zeta0(-km:km)    ! スペクトルデータ
  real(8)            :: e_Zeta1(-km:km)    ! スペクトルデータ

 !---- 座標変数など ----
  real(8), parameter :: xmin=0.0, xmax=3.0

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=2e-7               ! 時間ステップ間隔
  integer, parameter :: nt=25000, ndisp=500    ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: X1=(xmax+xmin)/2.0      ! 初期分布 X 座標
  real(8), parameter :: U1=720.0                ! 初期分布の振幅

  real(8), parameter :: X2=(xmax+3*xmin)/4.0    ! 初期分布 X 座標
  real(8), parameter :: U2=1440.0               ! 初期分布の振幅

  real(8) :: pi                                 ! 円周率
  integer :: i, j, it                           ! DO 変数

  pi = 4.0D0*atan(1.0D0)

  call ae_initial(im,km,xmin,xmax)                ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  !g_Zeta=  cos(pi*g_X)
  g_Zeta= U1*sech((g_X-X1)/sqrt(12/u1))**2 &
         + U2*sech((g_X-X2)/sqrt(12/u2))**2 

  e_Zeta = e_g(g_Zeta)
  e_Zeta1 = e_Zeta ; e_Zeta0 = e_Zeta

  it = 0                                             ! 初期化
  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt
     e_Zeta = e_Zeta1 &                              ! Adams-Bashforth scheme
          + dt * (   3/2.0 * e_DtDZeta_e(e_Zeta1) &
                   - 1/2.0 * e_DtDZeta_e(e_Zeta0)    )

     e_Zeta0=e_Zeta1 ; e_Zeta1=e_Zeta

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        g_Zeta = g_e(e_Zeta)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

contains
 !------------------- 数学関数 ------------------
  function sech(x)
    double precision :: x(0:im-1)
    double precision :: sech(0:im-1)

    sech = 2/(exp(x)+ exp(-x))

  end function sech

 !----------------- 時間変化項 -------------------
  function e_DtDZeta_e(e_Zeta)
    real(8), intent(IN)      :: e_Zeta(-km:km)        ! 入力(仮引数)
    real(8)                  :: e_DtDZeta_e(-km:km)   ! 時間変化項

    e_DtDZeta_e = -e_g(g_e(e_Zeta)*g_e(e_Dx_e(e_Zeta))) &
                  - e_Dx_e(e_Dx_e(e_Dx_e(e_Zeta)))

  end function e_DtDZeta_e

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='kdv2.nc', title='KDV equation model(A.B.Scheme)', &
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
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('zeta',g_Zeta)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program kdv
