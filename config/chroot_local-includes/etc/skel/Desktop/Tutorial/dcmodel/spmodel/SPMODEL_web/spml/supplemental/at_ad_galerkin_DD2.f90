!----------------------------------------------------------------------
!   Copyright (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ad_galerkin_DD2
!
!      チェビシェフ−ガラーキン法
!      両端ディリクレ境界条件用モジュール
!          f(:,i=0)=f(:,i=im)=0
!
!      k 次のガラーキン基底が
!
!        \phi_k(x)=T_k(x) + C_1 T_1(x) + C_0 T_0(x) 
!
!      の型式(Type2)
!
!履歴  2005/12/29  竹広真一  新規作成
!      2006/01/07  竹広真一  debug
!      2006/01/20  竹広真一  係数変換定式化変更
!      2006/01/23  竹広真一  モジュール名変更
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!
module at_ad_galerkin_DD

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_ad_galerkin_DD_Initial  ! 初期化サブルーチン
  public :: ad_at, d_t                 ! チェビシェフ->ガラーキン変換
  public :: ad_ag, d_g                 ! 格子点->ガラーキンデータ変換
  public :: at_ad, t_d                 ! ガラーキン->チェビシェフ変換
  public :: ag_ad, g_d                 ! ガラーキン->格子点データ変換
  public :: ad_Dx_ad, d_Dx_d           ! X 微分

  real(8), allocatable :: TD(:,:)      ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: DT(:,:)      ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:)        ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)     ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)      ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                        ! 格子点数              
  integer :: km                        ! チェビシェフ切断波数  
  integer,parameter :: ks=2            ! ガラーキン基底最低次数

  logical :: DD_Initialized=.false.    ! 初期化フラッグ

  save im, km, TD, DT, kp, alpha, beta, DD_Initialized
  public TD, DT, alpha, beta

contains

  !
  ! チェビシェフ−ガラーキン法 
  ! 両端ディリクレ境界条件用モジュール
  ! 初期化サブルーチン
  !
  subroutine at_ad_galerkin_DD_Initial(i_in,k_in)

    integer, intent(IN) :: i_in     ! (in)格子点数
    integer, intent(IN) :: k_in     ! (in)チェビシェフ切断波数  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TD(0:km,ks:km),DT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! 両端ディリクレ条件用変換行列設定
    TD = 0.0D0
    do k=ks,km
       TD(0,k) = -(1+(-1)**k)/2.0D0
       TD(1,k) = -(1-(-1)**k)/2.0D0
       TD(k,k) = 1.0D0
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! 両端ディリクレ条件用変換逆行列
    alpha=1.0 ; alpha(0)=2.0D0

    DT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             DT(m,n) = DT(m,n) + alpha(l)*TD(l,m)*TD(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(DT,kp)

    call MessageNotify('M','at_ad_galerkin_DD_Initial(Type2)',&
                           'Conversion matrices initialized')

    DD_Initialized=.true.

  end subroutine at_ad_galerkin_DD_Initial

  !
  ! 両端ディリクレ境界条件
  ! チェビシェフ係数 -> ガラーキン係数変換(2次元データ)
  !
  function ad_at(at_data)
    real(8), intent(IN) :: at_data(:,0:)                !(in)  チェビシェフ係数
    real(8)             :: ad_at(size(at_data,1),ks:km) !(out) ガラーキン係数  

    real(8)             :: ad_work(size(at_data,1),ks:km)  ! 作業用配列

    integer :: k,m

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','ad_at',&
                            'at_ad_galerkin_DD_module(Type2) not initialized')

    ad_work =0.0
    do m=ks,km
       do k=0,km
          ad_work(:,m) = ad_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TD(k,m)
       enddo
    enddo

    ad_at = LUSolve(DT,kp,ad_work)

  end function ad_at

  !
  ! 両端ディリクレ境界条件
  ! チェビシェフ係数 -> ガラーキン係数変換(1次元データ)
  !
  function d_t(t_data)

    real(8), intent(IN) :: t_data(0:km)        !(in)  チェビシェフ係数
    real(8)             :: d_t(ks:km)          !(out) ガラーキン係数  

    real(8)             :: d_work(ks:km)       ! 作業用配列

    integer :: k,m

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','d_t',&
                            'at_ad_galerkin_DD_module(Type2) not initialized')

    d_work =0.0
    do m=ks,km
       do k=0,km
          d_work(m) = d_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TD(k,m)
       enddo
    enddo

    d_t = LUSolve(DT,kp,d_work)
  end function d_t

  !
  ! 両端ディリクレ境界条件
  ! ガラーキン係数 -> チェビシェフ係数変換(2次元データ)
  !
  function at_ad(ad_data)

    real(8), intent(IN)  :: ad_data(:,ks:)              !(in)  ガラーキン係数
    real(8)              :: at_ad(size(ad_data,1),0:km) !(out) チェビシェフ係数
    integer :: m, n

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','at_ad',&
                            'at_ad_galerkin_DD_module(Type2) not initialized')

    at_ad = 0.0D0
    do m=0,km
       do n=ks,km
          at_ad(:,m) = at_ad(:,m) + TD(m,n)*ad_data(:,n)/beta(m)
       enddo
    enddo
  end function at_ad

  !
  ! 両端ディリクレ境界条件
  ! ガラーキン係数 -> チェビシェフ係数変換(1次元データ)
  !
  function t_d(d_data)

    real(8), intent(IN)  :: d_data(ks:km)       !(in)  ガラーキン係数  
    real(8)              :: t_d(0:km)           !(out) チェビシェフ係数
    integer :: m, n

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','t_d',&
                            'at_ad_galerkin_DD_module(Type2) not initialized')

    t_d = 0.0D0
    do m=0,km
       do n=ks,km
          t_d(m) = t_d(m) + TD(m,n)*d_data(n)/beta(m)
       enddo
    enddo
  end function t_d

  !
  ! 両端ディリクレ境界条件
  ! 格子点データ -> ガラーキン係数変換(2次元データ)
  !
  function ad_ag(ag_data)
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  格子点データ
    real(8)              :: ad_ag(size(ag_data,1),ks:km) !(out) ガラーキン係数

    ad_ag = ad_at(at_ag(ag_data))
  end function ad_ag

  !
  ! 両端ディリクレ境界条件
  ! 格子点データ -> ガラーキン係数変換(1次元データ)
  !
  function d_g(g_data)
    real(8), intent(IN)  :: g_data(0:im)        !(in)  格子点データ
    real(8)              :: d_g(ks:km)          !(out) ガラーキン係数

    d_g = d_t(t_g(g_data))
  end function d_g

  !
  ! 両端ディリクレ境界条件
  ! ガラーキン係数 -> 格子点データ変換(2次元データ)
  !
  function ag_ad(ad_data)
    real(8), intent(IN) :: ad_data(:,ks:)              !(in)  ガラーキン係数  
    real(8)             :: ag_ad(size(ad_data,1),0:im) !(out) 格子点データ  
    
    ag_ad = ag_at(at_ad(ad_data))
  end function ag_ad

  !
  ! 両端ディリクレ境界条件
  ! ガラーキン係数 -> 格子点データ変換(1次元データ)
  !
  function g_d(d_data)
    real(8), intent(IN) :: d_data(ks:km)        !(in)  ガラーキン係数  
    real(8)             :: g_d(0:im)            !(out) 格子点データ  
    
    g_d = g_t(t_d(d_data))
  end function g_d

  !
  ! 両端ディリクレ境界条件
  ! X 微分計算(1 次元)
  !
  function ad_Dx_ad(ad_data)
    real(8), intent(IN) :: ad_data(:,ks:)                  !(in) ガラーキン係数
    real(8)             :: ad_Dx_ad(size(ad_data,1),ks:km) !(out) 微分ガラーキン
    ad_Dx_ad = ad_at(at_Dx_at(at_ad(ad_data)))
  end function ad_Dx_ad

  !
  ! 両端ディリクレ境界条件
  ! X 微分計算(1 次元)
  !
  function d_Dx_d(d_data)
    real(8), intent(IN) :: d_data(ks:km)
    real(8)             :: d_Dx_d(ks:km)

    d_Dx_d = d_t(t_Dx_t(t_d(d_data)))

  end function d_Dx_d

end module at_ad_galerkin_DD
