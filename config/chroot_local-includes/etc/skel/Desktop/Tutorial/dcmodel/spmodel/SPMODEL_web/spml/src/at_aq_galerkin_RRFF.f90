!--
!----------------------------------------------------------------------
! COPYRIGHT (c) 2006-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_aq_galerkin_RRFF1
!
!      チェビシェフ−ガラーキン法
!      非圧縮流体の流線関数・流れポテンシャル用
!
!      case RR: 両端粘着条件(両端で値と 1 階微分が 0)
!               f(:,i=0)=f(:,i=im)=f'(:,i=0)=f'(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      case FF: 両端自由すべり条件(両端で値と 2 階微分が 0)
!               f(:,i=0)=f(:,i=im)=f''(:,i=0)=f''(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      case RF: 片端粘着条件, 片端自由すべり条件
!               (両端で値が 0, 片側で 1 階微分, もう一方で 2 階微分が 0)
!               f(:,i=0)=f(:,i=im)=f'(:,i=0)=f''(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      case FR: 片端自由すべり条件, 片端粘着条件
!               (両端で値が 0, 片側で 2 階微分, もう一方で 1 階微分が 0)
!               f(:,i=0)=f(:,i=im)=f''(:,i=0)=f'(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      k 次のガラーキン基底が
!        \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
!                         + C_{k-3}T_{k-3}(x) + C_{k-4}T_{k-4}(x) 
!      の型式(Type1)
!
!      定式化については解説文書
!
!         「チェビシェフ関数展開を利用したガラーキン法」
!
!      (cheb_gal.pdf)を参照のこと. 
!
!履歴  2006/01/24  竹広真一  新規作成
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!      2006/02/28  竹広真一  コメントを RDoc 用に変更
!      2009/07/22  佐々木洋平 at_module と境界条件スイッチを揃えた
!
!++
module at_aq_galerkin_RRFF
  !
  != at_aq_galerkin_RRFF
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_aq_galerkin_RRFF.f90,v 1.1 2009-07-22 03:35:11 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/at_aq_galerkin_RRFF はチェビシェフ−ガラーキン法,
  ! 非圧縮流体の流線関数・流れポテンシャルに対して,
  !
  ! * case RR: 両端粘着条件(両端で値と 1 階微分が 0)
  !            f(:,i=0)=f(:,i=im)=f'(:,i=0)=f'(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f'(:,x=xmin)=0 ]
  !
  ! * case FF: 両端自由すべり条件(両端で値と 2 階微分が 0)
  !            f(:,i=0)=f(:,i=im)=f''(:,i=0)=f''(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f''(:,x=xmin)=0 ]
  !
  ! * case RF: 片端粘着条件, 片端自由すべり条件
  !            (両端で値が 0, 片側で 1 階微分, もう一方で 2 階微分が 0)
  !            f(:,i=0)=f(:,i=im)=f'(:,i=0)=f''(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f'(:,x=xmin)=0 ]
  !
  ! * case FR: 片端自由すべり条件, 片端粘着条件
  !            (両端で値が 0, 片側で 2 階微分, もう一方で 1 階微分が 0)
  !            f(:,i=0)=f(:,i=im)=f''(:,i=0)=f'(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f''(:,x=xmin)=0 ]
  !
  ! を k 次のガラーキン基底が
  !     \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
  !                      + C_{k-3}T_{k-3}(x) + C_{k-4}T_{k-4}(x) 
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
  public :: at_aq_galerkin_RRFF_Initial  ! 初期化サブルーチン
  public :: aq_at, q_t                   ! チェビシェフ->ガラーキン変換
  public :: aq_ag, q_g                   ! 格子点->ガラーキンデータ変換
  public :: at_aq, t_q                   ! ガラーキン->チェビシェフ変換
  public :: ag_aq, g_q                   ! ガラーキン->格子点データ変換
  public :: aq_Dx_aq, q_Dx_q             ! X 微分

  real(8), allocatable :: TQ(:,:)   ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: QT(:,:)   ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:)     ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)  ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)   ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                     ! 格子点数              
  integer :: km                     ! チェビシェフ切断波数  
  integer,parameter :: ks=4         ! ガラーキン基底最低次数
  character(LEN=2) :: bc            ! 境界条件(RR/FF/RF/FR)

  logical :: RRFF_Initialized=.false.    ! 初期化フラッグ

  save im, km, bc, TQ, QT, kp, alpha, beta, RRFF_Initialized
  public TQ, QT, alpha, beta

contains

  subroutine at_aq_galerkin_RRFF_Initial(i_in,k_in,bc_in)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! 初期化サブルーチン
    !
    integer, intent(in)          :: i_in     
    ! (in)格子点数
    integer, intent(in)          :: k_in     
    ! (in)チェビシェフ切断波数  
    character(LEN=2), intent(in) :: bc_in    
    ! (in)境界条件(RR/FF/RF/FR)

    integer :: k, l, m, n
    real(8) :: delta

    im=i_in ; km=k_in ; bc=bc_in

    allocate(TQ(0:km,ks:km),QT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))


    select case (bc)

    !---- 両端粘着条件用変換行列設定
    case ('RR','rr')
       TQ = 0.0D0
       do k=ks,km
          TQ(k,k)   = 1.0D0
          TQ(k-2,k) = -2.0*(K-2.0D0)/(K-3.0D0)
          TQ(k-4,k) = (K-1.0D0)/(K-3.0D0)
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for RR-B.C. initialized.')

    !---- 両端自由すべり条件用変換行列設定
    case ('FF','ff','SS','ss')
       TQ = 0.0D0
       do k=ks,km
          TQ(k,k)   = 1.0D0
          TQ(k-2,k) = -(T2(k)-T2(k-4))/(T2(k-2)-T2(k-4))
          TQ(k-4,k) = (T2(k)-T2(k-2))/(T2(k-2)-T2(k-4))
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for FF-B.C. initialized.')


    !---- 片端粘着条件, 片端自由すべり条件変換行列設定
    case ('RF','rf','RS','rs')
       TQ = 0.0D0
       do k=ks,km
          delta = (T1(k-1)-T1(k-3))*(T2(k-2)-T2(k-4)) &
                + (T2(k-1)-T2(k-3))*(T1(k-2)-T1(k-4))
          TQ(k,k)   = 1.0D0
          TQ(k-3,k) = ( (T2(k-2)-T2(k-4))*(T1(k)-T1(k-2)) &
                        - (T1(k-2)-T1(k-4))*(T2(k)-T2(k-2)) )/delta
          TQ(k-4,k) = ( (T2(k-1)-T2(k-3))*(T1(k)-T1(k-2)) &
                        + (T1(k-1)-T1(k-3))*(T2(k)-T2(k-2)) )/delta
          TQ(k-1,k) = -TQ(k-3,k)
          TQ(k-2,k) = -TQ(k-4,k) - 1.0D0
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for RF-B.C. initialized.')

    !---- 片端自由すべり条件, 片端粘着条件変換行列設定
    case ('FR','fr','SR','sr')
       TQ = 0.0D0
       do k=ks,km
          delta = (T2(k-1)-T2(k-3))*(T1(k-2)-T1(k-4)) &
                + (T1(k-1)-T1(k-3))*(T2(k-2)-T2(k-4))
          TQ(k,k)   = 1.0D0
          TQ(k-3,k) = ( (T1(k-2)-T1(k-4))*(T2(k)-T2(k-2)) &
                        - (T2(k-2)-T2(k-4))*(T1(k)-T1(k-2)) )/delta
          TQ(k-4,k) = ( (T1(k-1)-T1(k-3))*(T2(k)-T2(k-2)) &
                        + (T2(k-1)-T2(k-3))*(T1(k)-T1(k-2)) )/delta
          TQ(k-1,k) = -TQ(k-3,k)
          TQ(k-2,k) = -TQ(k-4,k) - 1.0D0
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for FR-B.C. initialized.')

    case default
       call MessageNotify('E','at_aq_galerkin_RRFF_Initial',&
                'Argument for B.C. not valid. Should be RR/FF/RF/FR.')

    end select

    beta=1.0D0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! 両端粘着条件用変換逆行列
    alpha=1.0 ; alpha(0)=2.0D0

    QT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             QT(m,n) = QT(m,n) + alpha(l)*TQ(l,m)*TQ(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(QT,kp)

    RRFF_Initialized=.true.

    contains
      !
      ! k 次チェビシェフ多項式 2 階微分の x=1 での値
      !
      function T2(k)            
        integer, intent(in) :: k
        real(8)             :: T2

        T2 = k**2 * (k**2 -1.0D0) / 3.0D0
      end function T2
      !
      ! k 次チェビシェフ多項式 1 階微分の x=1 での値
      !
      function T1(k)            
        integer, intent(in) :: k
        real(8)             :: T1

        T1 = k**2.0D0
      end function T1

  end subroutine at_aq_galerkin_RRFF_Initial

  function aq_at(at_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(in) :: at_data(:,0:)                
    !(in)  チェビシェフ係数
    real(8)             :: aq_at(size(at_data,1),ks:km) 
    !(out) ガラーキン係数  

    real(8)             :: aq_work(size(at_data,1),ks:km)  
    ! 作業用配列
    
    integer :: k,m

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','aq_at',&
                            'at_aq_galerkin_RRFF_module not initialized')

    aq_work = 0.0D0
    do m=ks,km
       do k=0,km
          aq_work(:,m) = aq_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TQ(k,m)
       enddo
    enddo

    aq_at = LUSolve(QT,kp,aq_work)
  end function aq_at

  function q_t(t_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(in) :: t_data(0:km)        
    !(in)  チェビシェフ係数
    real(8)             :: q_t(ks:km)          
    !(out) ガラーキン係数  

    real(8)             :: q_work(ks:km)       
    ! 作業用配列

    integer :: k,m

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','q_t',&
                            'at_aq_galerkin_RRFF_module not initialized')

    q_work =0.0D0
    do m=ks,km
       do k=0,km
          q_work(m) = q_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TQ(k,m)
       enddo
    enddo

    q_t = LUSolve(QT,kp,q_work)
  end function q_t

  function at_aq(aq_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(2次元データ)
    !
    real(8), intent(in)  :: aq_data(:,ks:)              
    !(in)  ガラーキン係数
    real(8)              :: at_aq(size(aq_data,1),0:km) 
    !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','at_aq',&
                            'at_aq_galerkin_RRFF_module not initialized')

    at_aq = 0.0D0
    do m=0,km
       do n=ks,km
          at_aq(:,m) = at_aq(:,m) + TQ(m,n)*aq_data(:,n)/beta(m)
       enddo
    enddo
  end function at_aq

  function t_q(q_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(1次元データ)
    !
    real(8), intent(in)  :: q_data(ks:km)       
    !(in)  ガラーキン係数  
    real(8)              :: t_q(0:km)           
    !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','t_q',&
                            'at_aq_galerkin_RRFF_module not initialized')

    t_q = 0.0D0
    do m=0,km
       do n=ks,km
          t_q(m) = t_q(m) + TQ(m,n)*q_data(n)/beta(m)
       enddo
    enddo
  end function t_q

  function aq_ag(ag_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! 格子点データ -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(in)  :: ag_data(:,0:)                
    !(in)  格子点データ
    real(8)              :: aq_ag(size(ag_data,1),ks:km) 
    !(out) ガラーキン係数

    aq_ag = aq_at(at_ag(ag_data))
  end function aq_ag

  function q_g(g_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! 格子点データ -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(in)  :: g_data(0:im)        
    !(in)  格子点データ
    real(8)              :: q_g(ks:km)          
    !(out) ガラーキン係数

    q_g = q_t(t_g(g_data))
  end function q_g

  function ag_aq(aq_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! ガラーキン係数 -> 格子点データ変換(2次元データ)
    !
    real(8), intent(in) :: aq_data(:,ks:)              
    !(in)  ガラーキン係数  
    real(8)             :: ag_aq(size(aq_data,1),0:im) 
    !(out) 格子点データ  
    
    ag_aq = ag_at(at_aq(aq_data))
  end function ag_aq

  function g_q(q_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! ガラーキン係数 -> 格子点データ変換(1次元データ)
    !
    real(8), intent(in) :: q_data(ks:km)        
    !(in)  ガラーキン係数  
    real(8)             :: g_q(0:im)            
    !(out) 格子点データ  
    
    g_q = g_t(t_q(q_data))
  end function g_q

  function aq_Dx_aq(aq_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! X 微分計算(2 次元)
    !
    real(8), intent(in) :: aq_data(:,ks:)                  
    !(in) ガラーキン係数
    real(8)             :: aq_Dx_aq(size(aq_data,1),ks:km) 
    !(out) 微分ガラーキン
    aq_Dx_aq = aq_at(at_Dx_at(at_aq(aq_data)))
  end function aq_Dx_aq

  function q_Dx_q(q_data)
    !
    ! チェビシェフ−ガラーキン法
    ! 非圧縮流体の流線関数・流れポテンシャル用
    !
    ! X 微分計算(1 次元)
    !
    real(8), intent(in) :: q_data(ks:km)
    real(8)             :: q_Dx_q(ks:km)

    q_Dx_q = q_t(t_Dx_t(t_q(q_data)))

  end function q_Dx_q

end module at_aq_galerkin_RRFF
