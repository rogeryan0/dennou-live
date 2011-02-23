!--
!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_af_galerkin_MM2
!
!      チェビシェフ−ガラーキン法
!      ディリクレ・ノイマン混合境界条件用モジュール
!          cfdx1_xmax f'(:,i=0)  + cfdx0_xmax f(:,i=0)  = 0,
!          cfdx1_xmin f'(:,i=im) + cfdx0_xmin f(:,i=im) = 0,
!
!      k 次のガラーキン基底が
!
!        \phi_k(x)=T_k(x) + C_1 T_1(x) + C_0 T_0(x) 
!
!      の型式(Type2)
!
!      定式化については解説文書
!
!         「チェビシェフ関数展開を利用したガラーキン法」
!
!      (cheb_gal.pdf)を参照のこと. 
!
!履歴  2006/01/03  竹広真一  新規作成
!      2006/01/07  竹広真一  debug
!      2006/01/22  竹広真一  係数変換定式化変更
!      2006/01/23  竹広真一  モジュール名変更
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!      2006/02/15  竹広真一  係数行列式 0 の場合に対応
!      2006/02/26  竹広真一  係数行列式が 0 の場合に完全対応
!      2006/02/28  竹広真一  コメントを RDoc 用に変更
!      2009/01/29  佐々木洋平 コメントを RDoc 用に変更
!
!++
module at_af_galerkin_MM
  !
  != at_af_galerkin_MM
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_af_galerkin_MM.f90,v 1.3 2009-02-28 21:33:46 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/at_af_galerkin_MM モジュールはチェビシェフ−ガラーキン法,
  ! ディリクレ・ノイマン混合境界条件
  !
  !      cfdx1_xmax f'(:,i=0)  + cfdx0_xmax f(:,i=0)  = 0,
  !      cfdx1_xmin f'(:,i=im) + cfdx0_xmin f(:,i=im) = 0,
  !
  ! を k 次のガラーキン基底が
  !
  !    \phi_k(x)=T_k(x) + C_1 T_1(x) + C_0 T_0(x) 
  !
  ! の型式(Type2)を用いて計算するためのモジュールである.
  !
  ! 定式化については解説文書「チェビシェフ関数展開を利用したガラーキン法」
  ! (cheb_gal.pdf[link:link:cheb_gal.pdf])を参照のこと. 
  !
  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_af_galerkin_MM_Initial  ! 初期化サブルーチン
  public :: af_at, f_t                 ! チェビシェフ->ガラーキン変換
  public :: af_ag, f_g                 ! 格子点->ガラーキンデータ変換
  public :: at_af, t_f                 ! ガラーキン->チェビシェフ変換
  public :: ag_af, g_f                 ! ガラーキン->格子点データ変換
  public :: af_Dx_af, f_Dx_f           ! X 微分

  real(8), allocatable :: TF(:,:)      ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: FT(:,:)      ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:)        ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)     ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)      ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                        ! 格子点数              
  integer :: km                        ! チェビシェフ切断波数  
  integer :: jm                        ! 第 1 次元の大きさ
  integer,parameter :: ks=2            ! ガラーキン基底最低次数

  real(8) :: cfd0_xmin                 ! 境界条件係数(0階微分 at x=xmin)
  real(8) :: cfd1_xmin                 ! 境界条件係数(1階微分 at x=xmin)
  real(8) :: cfd0_xmax                 ! 境界条件係数(0階微分 at x=xmax)
  real(8) :: cfd1_xmax                 ! 境界条件係数(1階微分 at x=xmax)

  logical :: MM_Initialized=.false.    ! 初期化フラッグ
  logical :: MM_Delta0     =.false.    ! 係数行列式判定
  real(8),parameter :: EPS = 1.0D-14   ! 行列式判定用微小値

  save im, km, jm, TF, FT, kp, alpha, beta, MM_Initialized, MM_Delta0
  save cfd0_xmin, cfd1_xmin, cfd0_xmax, cfd1_xmax
  public TF, FT, alpha, beta

contains

  subroutine at_af_galerkin_MM_Initial(i_in,k_in,              &
                                    cfdx0_xmax,cfdx1_xmax,  &
                                    cfdx0_xmin,cfdx1_xmin    )
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件用モジュール
    !
    ! 初期化サブルーチン
    !

    integer, intent(IN) :: i_in      ! (in)格子点数
    integer, intent(IN) :: k_in      ! (in)チェビシェフ切断波数  

    real(8), intent(IN),optional :: cfdx0_xmin ! 境界条件係数(0階微分@x=xmin)
    real(8), intent(IN),optional :: cfdx1_xmin ! 境界条件係数(1階微分@x=xmin)
    real(8), intent(IN),optional :: cfdx0_xmax ! 境界条件係数(0階微分@x=xmax)
    real(8), intent(IN),optional :: cfdx1_xmax ! 境界条件係数(1階微分@x=xmax)

    real(8) :: Dfac                  ! 微分変換係数
    real(8) :: Delta                 ! 係数行列の行列式

    real(8) :: a, b, c, d, e, f
    integer :: k, l, m, n

    !---------- 格子点数・切断波数設定 ----------
    im=i_in ; km=k_in

    !---------- 境界条件係数設定 ----------
    Dfac = (g_X(0)-g_X(im))/2.0D0

    if( present(cfdx0_xmin) ) then
       cfd0_xmin = cfdx0_xmin
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial',&
                          'cfdx0_xmin set to zero internally.')
       cfd0_xmin = 0.0D0
    endif

    if( present(cfdx0_xmax) ) then
       cfd0_xmax = cfdx0_xmax
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial',&
                          'cfdx0_xmax set to zero internally.')
       cfd0_xmax = 0.0D0
    endif

    if( present(cfdx1_xmin) ) then
       cfd1_xmin = cfdx1_xmin/Dfac
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial',&
                          'cfdx1_xmin set to zero internally.')
       cfd1_xmin = 0.0D0
    endif

    if( present(cfdx1_xmax) ) then
       cfd1_xmax = cfdx1_xmax/Dfac
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial',&
                          'cfdx1_xmax set to zero internally.')
       cfd1_xmax = 0.0D0
    endif

    !---------- 境界条件係数チェック ----------

    Delta = cfd1_xmin*cfd0_xmax-cfd1_xmax*cfd0_xmin -2 * cfd0_xmin*cfd0_xmax

    if ( abs(Delta) &
         /max(abs(cfd1_xmin),abs(cfd0_xmin),abs(cfd1_xmax),abs(cfd0_xmax)) &
         .LT. EPS ) then
       MM_Delta0 = .true.
       call MessageNotify('W','at_af_galerkin_MM_Initial',&
                           'Determinant of coefficent matrix equals zero.' )
       call MessageNotify('M','at_af_galerkin_MM_Initial',&
                           'T2 and T1 are used for Galerkin base functions' )
    else
       MM_Delta0 = .false.
    endif

    !---------- ディリクレ・ノイマン混合条件用変換行列設定 ----------
    if ( allocated(TF) ) deallocate(TF)
    if ( allocated(FT) ) deallocate(FT)
    if ( allocated(kp) ) deallocate(kp)
    if ( allocated(alpha) ) deallocate(alpha)
    if ( allocated(beta) ) deallocate(beta)
    allocate(TF(0:km,ks:km),FT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    TF = 0.0D0
    if ( MM_Delta0 ) then
       a = 4 * cfd1_xmax + cfd0_xmax  ; b = cfd1_xmax + cfd0_xmax
       c = -4 * cfd1_xmin + cfd0_xmin ;  d = cfd1_xmin - cfd0_xmin
       Delta = a*d - b*c

       TF(0,2) = cfd1_xmax + cfd0_xmax
       TF(1,2) = -cfd0_xmax
       do k=ks+1,km
          e = cfd1_xmax * k**2 + cfd0_xmax
          f = cfd1_xmin * (-1)**(k+1) * k**2 + cfd0_xmin*(-1)**k
          TF(1,k) = -1.0D0/Delta*(-c*e+a*f)
          TF(2,k) = -1.0D0/Delta*(d*e-b*f)
          TF(k,k) = 1.0D0
       enddo
    else
       do k=ks,km
          TF(0,k) = 1.0D0/Delta                                       &
                * ( (-cfd1_xmin+cfd0_xmin)*(cfd1_xmax*k**2+cfd0_xmax) &
                   +  (cfd1_xmax+cfd0_xmax)                           &
                     *(    cfd1_xmin*(-1)**(k+1)*k**2                 &
                         + (-1)**k*cfd0_xmin         )                &
                  )
          TF(1,k) = 1.0D0/Delta                                       &
                * (   cfd0_xmin*(cfd1_xmax*k**2+cfd0_xmax)            &
                    - cfd0_xmax*(   cfd1_xmin*(-1)**(k+1)*k**2        &
                                  + (-1)**k*cfd0_xmin         )       &
                  )
          TF(k,k) = 1.0D0
       enddo
    endif

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! ディリクレ・ノイマン混合条件用変換逆行列
    alpha=1.0 ; alpha(0)=2.0D0

    FT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             FT(m,n) = FT(m,n) + alpha(l)*TF(l,m)*TF(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(FT,kp)

    call MessageNotify('M','at_af_galerkin_MM_Initial',&
                           'Conversion matrices initialized')

    MM_Initialized=.true.

  end subroutine at_af_galerkin_MM_Initial

  function af_at(at_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(IN) :: at_data(:,0:)                !(in)  チェビシェフ係数
    real(8)             :: af_at(size(at_data,1),ks:km) !(out) ガラーキン係数  

    real(8)             :: af_work(size(at_data,1),ks:km)  ! 作業用配列

    integer :: k,m

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','af_at',&
                            'at_af_galerkin_MM_module not initialized')

    af_work =0.0
    do m=ks,km
       do k=0,km
          af_work(:,m) = af_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TF(k,m)
       enddo
    enddo

    af_at = LUSolve(FT,kp,af_work)
  end function af_at

  function f_t(t_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! チェビシェフ係数 -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(IN) :: t_data(0:km)        !(in)  チェビシェフ係数
    real(8)             :: f_t(ks:km)          !(out) ガラーキン係数  

    real(8)             :: f_work(ks:km)       ! 作業用配列

    integer :: k,m

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','f_t',&
                            'at_af_galerkin_MM_module not initialized')

    f_work =0.0
    do m=ks,km
       do k=0,km
          f_work(m) = f_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TF(k,m)
       enddo
    enddo

    f_t = LUSolve(FT,kp,f_work)
  end function f_t

  function at_af(af_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(2次元データ)
    !
    real(8), intent(IN)  :: af_data(:,ks:)              !(in)  ガラーキン係数
    real(8)              :: at_af(size(af_data,1),0:km) !(out) チェビシェフ係数
    integer :: m, n

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','at_af',&
                            'at_af_galerkin_MM_module not initialized')

    at_af = 0.0D0
    do m=0,km
       do n=ks,km
          at_af(:,m) = at_af(:,m) + TF(m,n)*af_data(:,n)/beta(m)
       enddo
    enddo

  end function at_af

  function t_f(f_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! ガラーキン係数 -> チェビシェフ係数変換(1次元データ)
    !
    real(8), intent(IN)  :: f_data(ks:km)       !(in)  ガラーキン係数  
    real(8)              :: t_f(0:km)           !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','t_f',&
                            'at_af_galerkin_MM_module not initialized')

    t_f = 0.0D0
    do m=0,km
       do n=ks,km
          t_f(m) = t_f(m) + TF(m,n)*f_data(n)/beta(m)
       enddo
    enddo

  end function t_f

  function af_ag(ag_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! 格子点データ -> ガラーキン係数変換(2次元データ)
    !
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  格子点データ
    real(8)              :: af_ag(size(ag_data,1),ks:km) !(out) ガラーキン係数

    af_ag = af_at(at_ag(ag_data))
  end function af_ag

  function f_g(g_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! 格子点データ -> ガラーキン係数変換(1次元データ)
    !
    real(8), intent(IN)  :: g_data(0:im)        !(in)  格子点データ
    real(8)              :: f_g(ks:km)          !(out) ガラーキン係数

    f_g = f_t(t_g(g_data))
  end function f_g

  function ag_af(af_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! ガラーキン係数 -> 格子点データ変換(2次元データ)
    !
    real(8), intent(IN) :: af_data(:,ks:)              !(in)  ガラーキン係数  
    real(8)             :: ag_af(size(af_data,1),0:im) !(out) 格子点データ  
    
    ag_af = ag_at(at_af(af_data))
  end function ag_af

  function g_f(f_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! ガラーキン係数 -> 格子点データ変換(1次元データ)
    !
    real(8), intent(IN) :: f_data(ks:km)        !(in)  ガラーキン係数  
    real(8)             :: g_f(0:im)            !(out) 格子点データ  
    
    g_f = g_t(t_f(f_data))
  end function g_f

  function af_Dx_af(af_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! X 微分計算(2 次元)
    !
    real(8), intent(IN) :: af_data(:,ks:)                  !(in) ガラーキン係数
    real(8)             :: af_Dx_af(size(af_data,1),ks:km) !(out) 微分ガラーキン
    af_Dx_af = af_at(at_Dx_at(at_af(af_data)))
  end function af_Dx_af

  function f_Dx_f(f_data)
    !
    ! チェビシェフ−ガラーキン法
    ! ディリクレ・ノイマン混合境界条件
    !
    ! X 微分計算(1 次元)
    !
    real(8), intent(IN) :: f_data(ks:km)
    real(8)             :: f_Dx_f(ks:km)

    f_Dx_f = f_t(t_Dx_t(t_f(f_data)))

  end function f_Dx_f

end module at_af_galerkin_MM
