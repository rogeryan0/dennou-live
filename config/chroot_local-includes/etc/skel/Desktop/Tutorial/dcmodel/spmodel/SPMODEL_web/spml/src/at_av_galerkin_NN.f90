!--
!----------------------------------------------------------------------
!  Copyright (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_av_galerkin_NN1
!
!      チェビシェフ−ガラーキン法
!      両端ノイマン境界条件用モジュール
!          f'(:,i=0)=f'(:,i=im)=0
!
!      k 次のガラーキン基底が
!
!        \phi_2(x)=T_0(x)
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
!履歴  2005/01/20  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!      2006/02/28  竹広真一  コメントを RDoc 用に変更
!      2009/01/29  佐々木洋平  コメントを RDoc 用に変更
!
!++
module at_av_galerkin_NN
  !
  != at_av_galerkin_NN
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_av_galerkin_NN.f90,v 1.3 2009-02-28 21:33:46 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !
  !== 概要
  !
  ! spml/at_av_galerkin_NN モジュールはチェビシェフ−ガラーキン法,
  ! 両端ノイマン境界条件用モジュール
  !
  !       f'(:,i=0)=f'(:,i=im)=0
  !
  ! k 次のガラーキン基底が
  !
  !     \phi_2(x)=T_0(x)
  !     \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
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
  public :: at_av_galerkin_NN_Initial  ! 初期化サブルーチン
  public :: av_at, v_t                 ! チェビシェフ->ガラーキン変換
  public :: av_ag, v_g                 ! 格子点->ガラーキンデータ変換
  public :: at_av, t_v                 ! ガラーキン->チェビシェフ変換
  public :: ag_av, g_v                 ! ガラーキン->格子点データ変換
  public :: av_Dx_av, v_Dx_v           ! X 微分

  real(8), allocatable :: TV(:,:)      ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: VT(:,:)      ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:)        ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)     ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)      ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                        ! 格子点数              
  integer :: km                        ! チェビシェフ切断波数  
  integer,parameter :: ks=2            ! ガラーキン基底最低次数

  logical :: NN_Initialized=.false.    ! 初期化フラッグ

  save im, km, TV, VT, kp, alpha, beta, NN_Initialized
  public TV, VT, alpha, beta

contains

  subroutine at_av_galerkin_NN_Initial(i_in,k_in)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件用モジュール
    !
    ! 初期化サブルーチン
    !
    integer, intent(IN) :: i_in     ! (in)格子点数
    integer, intent(IN) :: k_in     ! (in)チェビシェフ切断波数  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TV(0:km,ks:km),VT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! 両端ディリクレ条件用変換行列設定
    TV = 0.0D0
    TV(0,ks) = 1.0D0
    do k=ks+1,km
       TV(k,k) = 1.0D0
       TV(k-2,k) = -1.0D0*k**2/(k-2)**2
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! 両端ディリクレ条件用変換逆行列
    alpha=1.0 ; alpha(0)=2.0D0

    VT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             VT(m,n) = VT(m,n) + alpha(l)*TV(l,m)*TV(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(VT,kp)

    call MessageNotify('M','at_av_galerkin_NN_Initial',&
                           'Conversion matrices initialized')

    NN_Initialized=.true.

  end subroutine at_av_galerkin_NN_Initial

  function av_at(at_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(IN) :: at_data(:,0:)                !(in)  チェビシェフ係数
    real(8)             :: av_at(size(at_data,1),ks:km) !(out) ガラーキン係数  

    real(8)             :: av_work(size(at_data,1),ks:km)  ! 作業用配列
    
    integer :: k,m

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','av_at',&
                            'at_av_galerkin_NN_module not initialized')

    av_work =0.0
    do m=ks,km
       do k=0,km
          av_work(:,m) = av_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TV(k,m)
       enddo
    enddo

    av_at = LUSolve(VT,kp,av_work)

  end function av_at

  function v_t(t_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(IN) :: t_data(0:km)        !(in)  チェビシェフ係数
    real(8)             :: v_t(ks:km)          !(out) ガラーキン係数  

    real(8)             :: v_work(ks:km)       ! 作業用配列

    integer :: k,m

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','v_t',&
                            'at_av_galerkin_NN_module not initialized')

    v_work =0.0
    do m=ks,km
       do k=0,km
          v_work(m) = v_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TV(k,m)
       enddo
    enddo

    v_t = LUSolve(VT,kp,v_work)

  end function v_t

  function at_av(av_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(2次元データ)
    !
    real(8), intent(IN)  :: av_data(:,ks:)              !(in)  ガラーキン係数
    real(8)              :: at_av(size(av_data,1),0:km) !(out) チェビシェフ係数
    integer :: m, n

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','at_av',&
                            'at_av_galerkin_NN_module not initialized')

    at_av = 0.0D0
    do m=0,km
       do n=ks,km
          at_av(:,m) = at_av(:,m) + TV(m,n)*av_data(:,n)/beta(m)
       enddo
    enddo

  end function at_av

  function t_v(v_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(1次元データ)
    !
    real(8), intent(IN)  :: v_data(ks:km)       !(in)  ガラーキン係数  
    real(8)              :: t_v(0:km)           !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','t_v',&
                            'at_av_galerkin_NN_module not initialized')

    t_v = 0.0D0
    do m=0,km
       do n=ks,km
          t_v(m) = t_v(m) + TV(m,n)*v_data(n)/beta(m)
       enddo
    enddo

  end function t_v

  function av_ag(ag_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! 格子点データ -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  格子点データ
    real(8)              :: av_ag(size(ag_data,1),ks:km) !(out) ガラーキン係数

    av_ag = av_at(at_ag(ag_data))
  end function av_ag

  function v_g(g_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! 格子点データ -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(IN)  :: g_data(0:im)        !(in)  格子点データ
    real(8)              :: v_g(ks:km)          !(out) ガラーキン係数

    v_g = v_t(t_g(g_data))
  end function v_g

  function ag_av(av_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! ガラーキン係数 -> 格子点データ変換(2次元データ)
    !
    real(8), intent(IN) :: av_data(:,ks:)              !(in)  ガラーキン係数  
    real(8)             :: ag_av(size(av_data,1),0:im) !(out) 格子点データ  
    
    ag_av = ag_at(at_av(av_data))
  end function ag_av

  function g_v(v_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! ガラーキン係数 -> 格子点データ変換(1次元データ)
    !
    real(8), intent(IN) :: v_data(ks:km)        !(in)  ガラーキン係数  
    real(8)             :: g_v(0:im)            !(out) 格子点データ  
    
    g_v = g_t(t_v(v_data))
  end function g_v

  function av_Dx_av(av_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! X 微分計算(1 次元)
    !
    real(8), intent(IN) :: av_data(:,ks:)                  !(in) ガラーキン係数
    real(8)             :: av_Dx_av(size(av_data,1),ks:km) !(out) 微分ガラーキン
    av_Dx_av = av_at(at_Dx_at(at_av(av_data)))
  end function av_Dx_av

  function v_Dx_v(v_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 両端ノイマン境界条件
    !
    ! X 微分計算(1 次元)
    !
    real(8), intent(IN) :: v_data(ks:km)
    real(8)             :: v_Dx_v(ks:km)

    v_Dx_v = v_t(t_Dx_t(t_v(v_data)))

  end function v_Dx_v

end module at_av_galerkin_NN
