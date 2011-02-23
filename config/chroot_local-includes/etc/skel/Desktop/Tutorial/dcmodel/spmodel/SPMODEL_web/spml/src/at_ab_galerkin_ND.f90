!--
!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ab_galerkin_ND1
!
!      チェビシェフ−ガラーキン法
!      片端ノイマン片端ディリクレ境界条件用モジュール
!          f'(:,i=0)=f(:,i=im)=0 [ f'(:,x=xmax)=f(:,x=xmin)=0 ]
!
!      k 次のガラーキン基底が
!
!        \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
!
!      の型式(Type1)
!
!      定式化については解説文書
!
!         「チェビシェフ関数展開を利用したガラーキン法」
!
!      (cheb_gal.pdf)を参照のこと. 
!
!履歴  2006/01/20  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!      2006/02/28  竹広真一  コメントを RDoc 用に変更
!      2009/01/29  佐々木洋平 コメントを RDoc 用に変更
!
!++
module at_ab_galerkin_ND
  !
  != at_ab_galerkin_ND
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_ab_galerkin_ND.f90,v 1.5 2009-02-28 21:33:45 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  ! 
  !== 概要
  !
  ! spml/at_ab_galerkin_ND モジュールはチェビシェフ−ガラーキン法, 
  ! 片端ノイマン片端ディリクレ境界条件
  !  
  !     f'(:,i=0)=f(:,i=im)=0 [ f'(:,x=xmax)=f(:,x=xmin)=0 ]
  !
  ! を k 次のガラーキン基底が
  !
  !   \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
  !
  ! の型式(Type1)を用いて計算するためのモジュールである.
  !
  ! 定式化については解説文書「チェビシェフ関数展開を利用したガラーキン法」
  ! (cheb_gal.pdf[link:link:cheb_gal.pdf])を参照のこと. 
  !
  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_ab_galerkin_ND_Initial  ! 初期化サブルーチン
  public :: ab_at, b_t                 ! チェビシェフ->ガラーキン変換
  public :: ab_ag, b_g                 ! 格子点->ガラーキンデータ変換
  public :: at_ab, t_b                 ! ガラーキン->チェビシェフ変換
  public :: ag_ab, g_b                 ! ガラーキン->格子点データ変換
  public :: ab_Dx_ab, b_Dx_b           ! X 微分

  real(8), allocatable :: TB(:,:)      ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: BT(:,:)      ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:)        ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)     ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)      ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                        ! 格子点数              
  integer :: km                        ! チェビシェフ切断波数  
  integer,parameter :: ks=2            ! ガラーキン基底最低次数

  logical :: ND_Initialized=.false.    ! 初期化フラッグ

  save im, km, TB, BT, kp, alpha, beta, ND_Initialized
  public TB, BT, alpha, beta

contains

  subroutine at_ab_galerkin_ND_Initial(i_in,k_in)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件用モジュール
    !
    ! 初期化サブルーチン
    !
    integer, intent(IN) :: i_in     ! (in)格子点数
    integer, intent(IN) :: k_in     ! (in)チェビシェフ切断波数  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TB(0:km,ks:km),BT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! 片端ノイマン片端ディリクレ条件用変換行列設定
    TB = 0.0D0
    do k=ks,km
       TB(k,k) = 1.0D0
       TB(k-1,k) = -1.0D0*(k**2-(k-2)**2)/((k-1)**2+(k-2)**2)
       TB(k-2,k) = -1.0D0*(k**2+(k-1)**2)/((k-1)**2+(k-2)**2)
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! 片端ノイマン片端ディリクレ条件用変換逆行列
    alpha=1.0 ; alpha(0)=2.0D0

    BT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             BT(m,n) = BT(m,n) + alpha(l)*TB(l,m)*TB(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(BT,kp)

    call MessageNotify('M','at_ab_galerkin_ND_Initial',&
                           'Conversion matrices initialized')

    ND_Initialized=.true.

  end subroutine at_ab_galerkin_ND_Initial

  function ab_at(at_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(IN) :: at_data(:,0:)                !(in)  チェビシェフ係数
    real(8)             :: ab_at(size(at_data,1),ks:km) !(out) ガラーキン係数  

    real(8)             :: ab_work(size(at_data,1),ks:km)  ! 作業用配列

    integer :: k,m

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','ab_at',&
                            'at_ab_galerkin_ND_module not initialized')

    ab_work =0.0
    do m=ks,km
       do k=0,km
          ab_work(:,m) = ab_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TB(k,m)
       enddo
    enddo

    ab_at = LUSolve(BT,kp,ab_work)
  end function ab_at

  function b_t(t_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(IN) :: t_data(0:km)        !(in)  チェビシェフ係数
    real(8)             :: b_t(ks:km)          !(out) ガラーキン係数  

    real(8)             :: b_work(ks:km)       ! 作業用配列

    integer :: k,m

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','b_t',&
                            'at_ab_galerkin_ND_module not initialized')

    b_work =0.0
    do m=ks,km
       do k=0,km
          b_work(m) = b_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TB(k,m)
       enddo
    enddo

    b_t = LUSolve(BT,kp,b_work)
  end function b_t

  function at_ab(ab_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(2次元データ)
    !
    real(8), intent(IN)  :: ab_data(:,ks:)              !(in)  ガラーキン係数
    real(8)              :: at_ab(size(ab_data,1),0:km) !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','at_ab',&
                            'at_ab_galerkin_ND_module not initialized')

    at_ab = 0.0D0
    do m=0,km
       do n=ks,km
          at_ab(:,m) = at_ab(:,m) + TB(m,n)*ab_data(:,n)/beta(m)
       enddo
    enddo

  end function at_ab

  function t_b(b_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(1次元データ)
    !
    real(8), intent(IN)  :: b_data(ks:km)       !(in)  ガラーキン係数  
    real(8)              :: t_b(0:km)           !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','t_b',&
                            'at_ab_galerkin_ND_module not initialized')

    t_b = 0.0D0
    do m=0,km
       do n=ks,km
          t_b(m) = t_b(m) + TB(m,n)*b_data(n)/beta(m)
       enddo
    enddo

  end function t_b

  function ab_ag(ag_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! 格子点データ -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  格子点データ
    real(8)              :: ab_ag(size(ag_data,1),ks:km) !(out) ガラーキン係数

    ab_ag = ab_at(at_ag(ag_data))
  end function ab_ag

  function b_g(g_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! 格子点データ -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(IN)  :: g_data(0:im)        !(in)  格子点データ
    real(8)              :: b_g(ks:km)          !(out) ガラーキン係数

    b_g = b_t(t_g(g_data))
  end function b_g

  function ag_ab(ab_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! ガラーキン係数 -> 格子点データ変換(2次元データ)
    !
    real(8), intent(IN) :: ab_data(:,ks:)              !(in)  ガラーキン係数  
    real(8)             :: ag_ab(size(ab_data,1),0:im) !(out) 格子点データ  
    
    ag_ab = ag_at(at_ab(ab_data))
  end function ag_ab

  function g_b(b_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! ガラーキン係数 -> 格子点データ変換(1次元データ)
    !
    real(8), intent(IN) :: b_data(ks:km)        !(in)  ガラーキン係数  
    real(8)             :: g_b(0:im)            !(out) 格子点データ  
    
    g_b = g_t(t_b(b_data))
  end function g_b

  function ab_Dx_ab(ab_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! X 微分計算(2 次元)
    !
    real(8), intent(IN) :: ab_data(:,ks:)                  
    !(in) ガラーキン係数
    real(8)             :: ab_Dx_ab(size(ab_data,1),ks:km) 
    !(out) 微分ガラーキン
    ab_Dx_ab = ab_at(at_Dx_at(at_ab(ab_data)))
  end function ab_Dx_ab

  function b_Dx_b(b_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 片端ノイマン片端ディリクレ境界条件
    !
    ! X 微分計算(1 次元)
    !
    real(8), intent(IN) :: b_data(ks:km)
    real(8)             :: b_Dx_b(ks:km)

    b_Dx_b = b_t(t_Dx_t(t_b(b_data)))

  end function b_Dx_b

end module at_ab_galerkin_ND
