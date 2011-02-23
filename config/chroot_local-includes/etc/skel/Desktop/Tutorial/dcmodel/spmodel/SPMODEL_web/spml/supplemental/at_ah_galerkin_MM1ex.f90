!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ah_galerkin_MM1ex
!
!      チェビシェフ−ガラーキン法
!      ディリクレ・ノイマン混合境界条件(可変係数)用モジュール
!
!          cfdx1_xmax(j) f'(j,i=0)  + cfdx0_xmax f(j,i=0)  = 0,
!          cfdx1_xmin(j) f'(j,i=im) + cfdx0_xmin f(j,i=im) = 0,
!
!      k 次のガラーキン基底が
!
!        \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
!
!      の型式(Type1)
!
!注意
!      高次(k>2)の係数行列式が 0 となる場合には計算できない. 
!
!履歴  2006/01/22  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!      2006/02/15  竹広真一  係数行列式 0 の特別な場合
!                            (cfdx0_xmin=cfdx0_xmax=0)に対応
!      2006/02/26  竹広真一  k=2 の 係数行列式が 0 の場合に完全対応
!
module at_ah_galerkin_MMex

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_ah_galerkin_MMex_Initial   ! 初期化サブルーチン
  public :: ah_at                         ! チェビシェフ->ガラーキン変換
  public :: ah_ag                         ! 格子点->ガラーキンデータ変換
  public :: at_ah                         ! ガラーキン->チェビシェフ変換
  public :: ag_ah                         ! ガラーキン->格子点データ変換
  public :: ah_Dx_ah                      ! X 微分

  real(8), allocatable :: TH(:,:,:)       ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: HT(:,:,:)       ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:,:)         ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)     ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)      ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                        ! 格子点数              
  integer :: km                        ! チェビシェフ切断波数  
  integer :: jm                        ! 第 1 次元の大きさ
  integer,parameter :: ks=2            ! ガラーキン基底最低次数

  logical :: MMex_Initialized=.false.   ! 初期化フラッグ
  real(8), parameter :: EPS = 1.0D-14   ! 行列式判定用微小値

  real(8), allocatable :: cfd0_xmin(:) ! 境界条件係数(0階微分 at x=xmin)
  real(8), allocatable :: cfd1_xmin(:) ! 境界条件係数(1階微分 at x=xmin)
  real(8), allocatable :: cfd0_xmax(:) ! 境界条件係数(0階微分 at x=xmax)
  real(8), allocatable :: cfd1_xmax(:) ! 境界条件係数(1階微分 at x=xmax)

  save im, km, jm, TH, HT, kp, alpha, beta, MMex_Initialized
  save cfd0_xmin, cfd1_xmin, cfd0_xmax, cfd1_xmax
  public TH, HT, alpha, beta

contains

  !
  ! チェビシェフ−ガラーキン法 
  ! ディリクレ・ノイマン混合境界条件用モジュール
  ! 初期化サブルーチン
  !
  subroutine at_ah_galerkin_MMex_Initial(i_in,k_in,j_in,      &
                                      cfdx0_xmax,cfdx1_xmax,  &
                                      cfdx0_xmin,cfdx1_xmin    )

    integer, intent(IN) :: i_in      ! (in)格子点数
    integer, intent(IN) :: k_in      ! (in)チェビシェフ切断波数  
    integer, intent(IN) :: j_in      ! (in)第 1 次元の大きさ

    real(8), intent(IN),optional :: cfdx0_xmin(j_in) ! 境界条件係数(0階微分@x=xmin)
    real(8), intent(IN),optional :: cfdx1_xmin(j_in) ! 境界条件係数(1階微分@x=xmin)
    real(8), intent(IN),optional :: cfdx0_xmax(j_in) ! 境界条件係数(0階微分@x=xmax)
    real(8), intent(IN),optional :: cfdx1_xmax(j_in) ! 境界条件係数(1階微分@x=xmax)

    real(8) :: Dfac                  ! 微分変換係数
    real(8) :: Delta(j_in)           ! 係数行列の行列式

    integer :: j, k, l, m, n
    real(8) :: a(j_in),b(j_in),c(j_in),d(j_in),e(j_in),f(j_in)
    character(len=10) :: cj


    !---------- 格子点数・切断波数設定 ----------
    im=i_in ; km=k_in ; jm = j_in

    !---------- 境界条件係数設定 ----------
    if ( allocated(cfd0_xmin) ) deallocate(cfd0_xmin)
    if ( allocated(cfd1_xmin) ) deallocate(cfd1_xmin)
    if ( allocated(cfd0_xmax) ) deallocate(cfd0_xmax)
    if ( allocated(cfd1_xmax) ) deallocate(cfd1_xmax)

    allocate(cfd0_xmin(jm),cfd1_xmin(jm))
    allocate(cfd0_xmax(jm),cfd1_xmax(jm))

    Dfac = (g_X(0)-g_X(im))/2.0D0

    if( present(cfdx0_xmin) ) then
       cfd0_xmin = cfdx0_xmin
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx0_xmin set to zero internally.')
       cfd0_xmin = 0.0D0
    endif

    if( present(cfdx0_xmax) ) then
       cfd0_xmax = cfdx0_xmax
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx0_xmax set to zero internally.')
       cfd0_xmax = 0.0D0
    endif

    if( present(cfdx1_xmin) ) then
       cfd1_xmin = cfdx1_xmin/Dfac
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx1_xmin set to zero internally.')
       cfd1_xmin = 0.0D0
    endif

    if( present(cfdx1_xmax) ) then
       cfd1_xmax = cfdx1_xmax/Dfac
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx1_xmax set to zero internally.')
       cfd1_xmax = 0.0D0
    endif

    !---------- ディリクレ・ノイマン混合条件用変換行列設定 ----------
    if ( allocated(TH) ) deallocate(TH)
    if ( allocated(HT) ) deallocate(HT)
    if ( allocated(kp) ) deallocate(kp)
    if ( allocated(alpha) ) deallocate(alpha)
    if ( allocated(beta) ) deallocate(beta)
    allocate(TH(jm,0:km,ks:km),HT(jm,ks:km,ks:km),kp(jm,ks:km))
    allocate(alpha(0:km),beta(0:km))

    TH = 0.0D0
    do k=ks,ks
       a = cfd1_xmax * (k-1)**2 + cfd0_xmax
       b = cfd1_xmax * (k-2)**2 + cfd0_xmax 
       c = cfd1_xmin * (k-1)**2 - cfd0_xmin 
       d = -cfd1_xmin * (k-2)**2 + cfd0_xmin 
       e = -( cfd1_xmax * k**2 + cfd0_xmax )
       f = -(-cfd1_xmin * k**2 + cfd0_xmin )
       Delta = a*d -b*c

       do j=1,jm
          if ( abs(Delta(j))/max(abs(a(j)),abs(b(j)),abs(c(j)),abs(d(j))) &
               .LT. EPS ) then
             write(cj,'(I10)') j
             call MessageNotify('W','at_ah_galerkin_MM_Initial(Type1)',&
                  'Determinant of coefficent matrix equals zero.' )
             call MessageNotify('W','at_af_galerkin_MM_Initial(Type1)',&
                  'Type of the base function at the ('                 &
                  //trim(adjustl(cj))//',2)-th element is modified to '&
                  //'C_1 T_1(x) + C_0 T_0(x)' )

             TH(j,k-1,k) = -cfdx0_xmax(j)
             TH(j,k-2,k) = cfdx1_xmax(j) + cfdx0_xmax(j)
          else
             TH(j,k-1,k) = 1.0D0/Delta(j) * (  d(j)*e(j) - b(j)*f(j) )
             TH(j,k-2,k) = 1.0D0/Delta(j) * ( -c(j)*e(j) + a(j)*f(j) )
             TH(j,k,k) = 1.0D0
          end if
       enddo
    end do

    do k=ks+1,km
       a = cfd1_xmax * (k-1)**2 + cfd0_xmax
       b = cfd1_xmax * (k-2)**2 + cfd0_xmax 
       c = cfd1_xmin * (k-1)**2 - cfd0_xmin 
       d = -cfd1_xmin * (k-2)**2 + cfd0_xmin 
       e = -( cfd1_xmax * k**2 + cfd0_xmax )
       f = -(-cfd1_xmin * k**2 + cfd0_xmin )
       Delta = a*d -b*c

       do j=1,jm
          if ( abs(Delta(j))/max(abs(a(j)),abs(b(j)),abs(c(j)),abs(d(j))) &
               .LT. EPS ) then
             write(6,*)j,k
             call MessageNotify('E','at_ah_galerkin_MM_Initial(Type1)',&
                  'Determinant of coefficent matrix equals zero.' )
          else
             TH(j,k-1,k) = 1.0D0/Delta(j) * (  d(j)*e(j) - b(j)*f(j) )
             TH(j,k-2,k) = 1.0D0/Delta(j) * ( -c(j)*e(j) + a(j)*f(j) )
             TH(j,k,k) = 1.0D0
          end if
       enddo
    end do

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! ディリクレ・ノイマン混合条件用変換逆行列
    alpha=1.0 ; alpha(0)=2.0D0

    HT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             HT(:,m,n) = HT(:,m,n) + alpha(l)*TH(:,l,m)*TH(:,l,n)
          enddo
       enddo
    enddo

    call LUDecomp(HT,kp)

    call MessageNotify('M','at_ah_galerkin_MMex_Initial(Type1)',&
                           'Conversion matrices initialized')

    MMex_Initialized=.true.

  end subroutine at_ah_galerkin_MMex_Initial

  !
  ! ディリクレ・ノイマン混合境界条件
  ! チェビシェフ係数 -> ガラーキン係数変換(2次元データ)
  !
  function ah_at(at_data)
    real(8), intent(IN) :: at_data(:,0:)                !(in)  チェビシェフ係数
    real(8)             :: ah_at(size(at_data,1),ks:km) !(out) ガラーキン係数  

    real(8)             :: ah_work(size(at_data,1),ks:km)  ! 作業用配列

    integer :: k,m

    if ( .not. MMex_Initialized ) &
         call MessageNotify('E','ah_at',&
                            'at_ah_galerkin_MMex_module(Type1) not initialized')

    ah_work =0.0
    do m=ks,km
       do k=0,km
          ah_work(:,m) = ah_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TH(:,k,m)
       enddo
    enddo

    ah_at = LUSolve(HT,kp,ah_work)
  end function ah_at

  !
  ! ディリクレ・ノイマン混合境界条件
  ! ガラーキン係数 -> チェビシェフ係数変換(2次元データ)
  !
  function at_ah(ah_data)

    real(8), intent(IN)  :: ah_data(:,ks:)              !(in)  ガラーキン係数
    real(8)              :: at_ah(size(ah_data,1),0:km) !(out) チェビシェフ係数

    integer :: m, n

    if ( .not. MMex_Initialized ) &
         call MessageNotify('E','at_ah',&
                            'at_ah_galerkin_MMex_module(Type1) not initialized')

    at_ah = 0.0D0
    do m=0,km
       do n=ks,km
          at_ah(:,m) = at_ah(:,m) + TH(:,m,n)*ah_data(:,n)/beta(m)
       enddo
    enddo

  end function at_ah

  !
  ! ディリクレ・ノイマン混合境界条件
  ! 格子点データ -> ガラーキン係数変換(2次元データ)
  !
  function ah_ag(ag_data)
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  格子点データ
    real(8)              :: ah_ag(size(ag_data,1),ks:km) !(out) ガラーキン係数

    ah_ag = ah_at(at_ag(ag_data))
  end function ah_ag

  !
  ! ディリクレ・ノイマン混合境界条件
  ! ガラーキン係数 -> 格子点データ変換(2次元データ)
  !
  function ag_ah(ah_data)
    real(8), intent(IN) :: ah_data(:,ks:)              !(in)  ガラーキン係数  
    real(8)             :: ag_ah(size(ah_data,1),0:im) !(out) 格子点データ  
    
    ag_ah = ag_at(at_ah(ah_data))
  end function ag_ah

  !
  ! ディリクレ・ノイマン混合境界条件
  ! X 微分計算(1 次元)
  !
  function ah_Dx_ah(ah_data)
    real(8), intent(IN) :: ah_data(:,ks:)                  !(in) ガラーキン係数
    real(8)             :: ah_Dx_ah(size(ah_data,1),ks:km) !(out) 微分ガラーキン
    ah_Dx_ah = ah_at(at_Dx_at(at_ah(ah_data)))
  end function ah_Dx_ah

end module at_ah_galerkin_MMex
