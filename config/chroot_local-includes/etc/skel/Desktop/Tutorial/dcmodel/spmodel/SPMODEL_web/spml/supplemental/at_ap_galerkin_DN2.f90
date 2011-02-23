!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ap_galerkin_DN2
!
!      チェビシェフ−ガラーキン法
!      片端ディリクレ片端ノイマン境界条件用モジュール
!          f(:,i=0)=f'(:,i=im)=0 [ f(:,x=xmax)=f'(:,x=xmin)=0 ]
!
!      k 次のガラーキン基底が
!
!        \phi_k(x)=T_k(x) + C_1 T_1(x) + C_0 T_0(x) 
!
!      の型式(Type2)
!
!履歴  2005/12/30  竹広真一  新規作成
!      2006/01/07  竹広真一  debug
!      2006/01/20  竹広真一  係数変換定式化変更
!      2006/01/23  竹広真一  モジュール名変更
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!
module at_ap_galerkin_DN

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_ap_galerkin_DN_Initial  ! 初期化サブルーチン
  public :: ap_at, p_t                 ! チェビシェフ->ガラーキン変換
  public :: ap_ag, p_g                 ! 格子点->ガラーキンデータ変換
  public :: at_ap, t_p                 ! ガラーキン->チェビシェフ変換
  public :: ag_ap, g_p                 ! ガラーキン->格子点データ変換
  public :: ap_Dx_ap, p_Dx_p           ! X 微分

  real(8), allocatable :: TP(:,:)      ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: PT(:,:)      ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:)        ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)     ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)      ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                        ! 格子点数              
  integer :: km                        ! チェビシェフ切断波数  
  integer,parameter :: ks=2            ! ガラーキン基底最低次数

  logical :: DN_Initialized=.false.    ! 初期化フラッグ

  save im, km, TP, PT, kp, alpha, beta, DN_Initialized
  public TP, PT, alpha, beta

contains

  !
  ! チェビシェフ−ガラーキン法 
  ! 片端ディリクレ片端ノイマン境界条件用モジュール
  ! 初期化サブルーチン
  !
  subroutine at_ap_galerkin_DN_Initial(i_in,k_in)

    integer, intent(IN) :: i_in     ! (in)格子点数
    integer, intent(IN) :: k_in     ! (in)チェビシェフ切断波数  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TP(0:km,ks:km),PT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! 片端ディリクレ片端ノイマン条件用変換行列設定
    TP = 0.0D0
    do k=ks,km
       TP(0,k) = (-1)**(k+1)*k**2 -1
       TP(1,k) = (-1)**k * k**2
       TP(k,k) = 1.0D0
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! 片端ディリクレ片端ノイマン条件用変換逆行列
    alpha=1.0 ; alpha(0)=2.0D0

    PT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             PT(m,n) = PT(m,n) + alpha(l)*TP(l,m)*TP(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(PT,kp)

    call MessageNotify('M','at_ap_galerkin_DN_Initial(Type2)',&
                           'Conversion matrices initialized')

    DN_Initialized=.true.

  end subroutine at_ap_galerkin_DN_Initial

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! チェビシェフ係数 -> ガラーキン係数変換(2次元データ)
  !
  function ap_at(at_data)
    real(8), intent(IN) :: at_data(:,0:)                !(in)  チェビシェフ係数
    real(8)             :: ap_at(size(at_data,1),ks:km) !(out) ガラーキン係数  

    real(8)             :: ap_work(size(at_data,1),ks:km)  ! 作業用配列

    integer :: k,m

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','ap_at',&
                            'at_ap_galerkin_DN_module(Type2) not initialized')

    ap_work =0.0
    do m=ks,km
       do k=0,km
          ap_work(:,m) = ap_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TP(k,m)
       enddo
    enddo

    ap_at = LUSolve(PT,kp,ap_work)
  end function ap_at

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! チェビシェフ係数 -> ガラーキン係数変換(1次元データ)
  !
  function p_t(t_data)

    real(8), intent(IN) :: t_data(0:km)        !(in)  チェビシェフ係数
    real(8)             :: p_t(ks:km)          !(out) ガラーキン係数  

    real(8)             :: p_work(ks:km)       ! 作業用配列

    integer :: k,m

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','p_t',&
                            'at_ap_galerkin_DN_module(Type2) not initialized')

    p_work =0.0
    do m=ks,km
       do k=0,km
          p_work(m) = p_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TP(k,m)
       enddo
    enddo

    p_t = LUSolve(PT,kp,p_work)
  end function p_t

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! ガラーキン係数 -> チェビシェフ係数変換(2次元データ)
  !
  function at_ap(ap_data)

    real(8), intent(IN)  :: ap_data(:,ks:)              !(in)  ガラーキン係数
    real(8)              :: at_ap(size(ap_data,1),0:km) !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','at_ap',&
                            'at_ap_galerkin_DN_module(Type2) not initialized')

    at_ap = 0.0D0
    do m=0,km
       do n=ks,km
          at_ap(:,m) = at_ap(:,m) + TP(m,n)*ap_data(:,n)/beta(m)
       enddo
    enddo

  end function at_ap

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! ガラーキン係数 -> チェビシェフ係数変換(1次元データ)
  !
  function t_p(p_data)

    real(8), intent(IN)  :: p_data(ks:km)       !(in)  ガラーキン係数  
    real(8)              :: t_p(0:km)           !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','t_p',&
                            'at_ap_galerkin_DN_module(Type2) not initialized')

    t_p = 0.0D0
    do m=0,km
       do n=ks,km
          t_p(m) = t_p(m) + TP(m,n)*p_data(n)/beta(m)
       enddo
    enddo

  end function t_p

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! 格子点データ -> ガラーキン係数変換(2次元データ)
  !
  function ap_ag(ag_data)
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  格子点データ
    real(8)              :: ap_ag(size(ag_data,1),ks:km) !(out) ガラーキン係数

    ap_ag = ap_at(at_ag(ag_data))
  end function ap_ag

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! 格子点データ -> ガラーキン係数変換(1次元データ)
  !
  function p_g(g_data)
    real(8), intent(IN)  :: g_data(0:im)        !(in)  格子点データ
    real(8)              :: p_g(ks:km)          !(out) ガラーキン係数

    p_g = p_t(t_g(g_data))
  end function p_g

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! ガラーキン係数 -> 格子点データ変換(2次元データ)
  !
  function ag_ap(ap_data)
    real(8), intent(IN) :: ap_data(:,ks:)              !(in)  ガラーキン係数  
    real(8)             :: ag_ap(size(ap_data,1),0:im) !(out) 格子点データ  
    
    ag_ap = ag_at(at_ap(ap_data))
  end function ag_ap

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! ガラーキン係数 -> 格子点データ変換(1次元データ)
  !
  function g_p(p_data)
    real(8), intent(IN) :: p_data(ks:km)        !(in)  ガラーキン係数  
    real(8)             :: g_p(0:im)            !(out) 格子点データ  
    
    g_p = g_t(t_p(p_data))
  end function g_p

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! X 微分計算(1 次元)
  !
  function ap_Dx_ap(ap_data)
    real(8), intent(IN) :: ap_data(:,ks:)                  !(in) ガラーキン係数
    real(8)             :: ap_Dx_ap(size(ap_data,1),ks:km) !(out) 微分ガラーキン
    ap_Dx_ap = ap_at(at_Dx_at(at_ap(ap_data)))
  end function ap_Dx_ap

  !
  ! 片端ディリクレ片端ノイマン境界条件
  ! X 微分計算(1 次元)
  !
  function p_Dx_p(p_data)
    real(8), intent(IN) :: p_data(ks:km)
    real(8)             :: p_Dx_p(ks:km)

    p_Dx_p = p_t(t_Dx_t(t_p(p_data)))

  end function p_Dx_p

end module at_ap_galerkin_DN
