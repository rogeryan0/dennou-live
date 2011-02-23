!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ah_galerkin_MM2ex
!
!      チェビシェフ−ガラーキン法
!      ディリクレ・ノイマン混合境界条件(可変係数)用モジュール
!
!          cfdx1_xmax(j) f'(j,i=0)  + cfdx0_xmax f(j,i=0)  = 0,
!          cfdx1_xmin(j) f'(j,i=im) + cfdx0_xmin f(j,i=im) = 0,
!
!      k 次のガラーキン基底が
!
!        \phi_k(x)=T_k(x) + C_1 T_1(x) + C_0 T_0(x) 
!
!      の型式(Type2)
!
!履歴  2006/01/06  竹広真一  新規作成
!      2006/01/07  竹広真一  debug
!      2006/01/22  竹広真一  係数変換定式化変更
!      2006/01/23  竹広真一  モジュール名変更
!      2006/01/25  竹広真一  モジュール初期化フラッグ導入
!      2006/02/15  竹広真一  係数行列式 0 の場合に対応
!      2006/02/26  竹広真一  係数行列式が 0 の場合に完全対応
!
module at_ah_galerkin_MMex

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_ah_galerkin_MMex_Initial   ! 初期化サブルーチン
  public :: ah_at                      ! チェビシェフ->ガラーキン変換
  public :: ah_ag                      ! 格子点->ガラーキンデータ変換
  public :: at_ah                      ! ガラーキン->チェビシェフ変換
  public :: ag_ah                      ! ガラーキン->格子点データ変換
  public :: ah_Dx_ah                   ! X 微分

  real(8), allocatable :: TH(:,:,:)    ! ガラーキン->チェビシェフ変換行列
  real(8), allocatable :: HT(:,:,:)    ! チェビシェフ->ガラーキン変換行列
  integer, allocatable :: kp(:,:)      ! LU 分解用ピボット格納配列

  real(8), allocatable :: alpha(:)  ! チェビシェフ<->ガラーキン行列積係数
  real(8), allocatable :: beta(:)   ! チェビシェフ<->ガラーキン行列和係数

  integer :: im                        ! 格子点数              
  integer :: km                        ! チェビシェフ切断波数  
  integer :: jm                        ! 第 1 次元の大きさ
  integer,parameter :: ks=2            ! ガラーキン基底最低次数

  logical :: MMex_Initialized=.false.  ! 初期化フラッグ

  real(8), allocatable :: cfd0_xmin(:) ! 境界条件係数(0階微分 at x=xmin)
  real(8), allocatable :: cfd1_xmin(:) ! 境界条件係数(1階微分 at x=xmin)
  real(8), allocatable :: cfd0_xmax(:) ! 境界条件係数(0階微分 at x=xmax)
  real(8), allocatable :: cfd1_xmax(:) ! 境界条件係数(1階微分 at x=xmax)

  logical, allocatable :: MMex_Delta0(:) ! 係数行列式判定
  real(8), parameter   :: EPS = 1.0D-14  ! 行列式判定用微小値

  save im, km, jm, TH, HT, kp, alpha, beta, MMex_Initialized, MMex_Delta0
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
    real(8) :: a, b, c, d, e, f
    character(len=10) :: cnumber

    !---------- 格子点数・切断波数設定 ----------
    im=i_in ; km=k_in ; jm = j_in

    !---------- 境界条件係数設定 ----------
    if ( allocated(cfd0_xmin) ) deallocate(cfd0_xmin)
    if ( allocated(cfd1_xmin) ) deallocate(cfd1_xmin)
    if ( allocated(cfd0_xmax) ) deallocate(cfd0_xmax)
    if ( allocated(cfd1_xmax) ) deallocate(cfd1_xmax)
    if ( allocated(MMex_Delta0) ) deallocate(MMex_Delta0)

    allocate(cfd0_xmin(jm),cfd1_xmin(jm))
    allocate(cfd0_xmax(jm),cfd1_xmax(jm))
    allocate(MMex_Delta0(jm))

    Dfac = (g_X(0)-g_X(im))/2.0D0

    if( present(cfdx0_xmin) ) then
       cfd0_xmin = cfdx0_xmin
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type2)',&
                          'cfdx0_xmin set to zero internally.')
       cfd0_xmin = 0.0D0
    endif

    if( present(cfdx0_xmax) ) then
       cfd0_xmax = cfdx0_xmax
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type2)',&
                          'cfdx0_xmax set to zero internally.')
       cfd0_xmax = 0.0D0
    endif

    if( present(cfdx1_xmin) ) then
       cfd1_xmin = cfdx1_xmin/Dfac
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type2)',&
                          'cfdx1_xmin set to zero internally.')
       cfd1_xmin = 0.0D0
    endif

    if( present(cfdx1_xmax) ) then
       cfd1_xmax = cfdx1_xmax/Dfac
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type2)',&
                          'cfdx1_xmax set to zero internally.')
       cfd1_xmax = 0.0D0
    endif

    !---------- 境界条件係数チェック ----------

    Delta = cfd1_xmin*cfd0_xmax-cfd1_xmax*cfd0_xmin -2 * cfd0_xmin*cfd0_xmax

    do j=1,jm
       if ( abs(Delta(j))                             &
            /max(abs(cfd1_xmin(j)),abs(cfd0_xmin(j)), &
                 abs(cfd1_xmax(j)),abs(cfd0_xmax(j))) &
         .LT. EPS ) then

          write(cnumber,'(I10)') j
          MMex_Delta0(j) = .true.
          call MessageNotify('W','at_ah_galerkin_MM_Initial(Type2)',&
               'Determinant of coefficent matrix equals zero.' )
          call MessageNotify('M','at_ah_galerkin_MM_Initial(Type2)',&
               'T2 and T1 are used for Galerkin base functions at the ' &
               //trim(adjustl(cnumber))//'-th element.' )
         else
            MMex_Delta0(j) = .false.
         endif
    enddo

    !---------- ディリクレ・ノイマン混合条件用変換行列設定 ----------
    if ( allocated(TH) ) deallocate(TH)
    if ( allocated(HT) ) deallocate(HT)
    if ( allocated(kp) ) deallocate(kp)
    if ( allocated(alpha) ) deallocate(alpha)
    if ( allocated(beta) ) deallocate(beta)
    allocate(TH(jm,0:km,ks:km),HT(jm,ks:km,ks:km),kp(jm,ks:km))
    allocate(alpha(0:km),beta(0:km))

    TH = 0.0D0
    do j=1,jm
       if ( MMex_Delta0(j) ) then
          a = 4 * cfd1_xmax(j) + cfd0_xmax(j)
          b = cfd1_xmax(j) + cfd0_xmax(j)
          c = -4 * cfd1_xmin(j) + cfd0_xmin(j)
          d = cfd1_xmin(j) - cfd0_xmin(j)
          Delta(j) = a*d - b*c

          TH(j,0,2) = cfd1_xmax(j) + cfd0_xmax(j)
          TH(j,1,2) = -cfd0_xmax(j)
          do k=ks+1,km
             e = cfd1_xmax(j) * k**2 + cfd0_xmax(j)
             f = cfd1_xmin(j) * (-1)**(k+1) * k**2 + cfd0_xmin(j)*(-1)**k
             TH(j,1,k) = -1.0D0/Delta(j)*(-c*e+a*f)
             TH(j,2,k) = -1.0D0/Delta(j)*(d*e-b*f)
             TH(j,k,k) = 1.0D0
          enddo
       else
          do k=ks,km
             TH(j,0,k) = 1.0D0/Delta(j)                                &
                  * (   (-cfd1_xmin(j)+cfd0_xmin(j))                   &
                           *(cfd1_xmax(j)*k**2+cfd0_xmax(j))           &
                      + (cfd1_xmax(j)+cfd0_xmax(j))                    &
                        *(    cfd1_xmin(j)*(-1)**(k+1)*k**2            &
                            + (-1)**k*cfd0_xmin(j)         )           &
                     )
             TH(j,1,k) = 1.0D0/Delta(j)                                &
                   * (   cfd0_xmin(j)                                  &
                           *(cfd1_xmax(j)*k**2+cfd0_xmax(j))           &
                       - cfd0_xmax(j)                                  &
                           *(   cfd1_xmin(j)*(-1)**(k+1)*k**2          &
                              + (-1)**k*cfd0_xmin(j)         )         &
                     )
             TH(j,k,k) = 1.0D0
          enddo
       endif
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

    call MessageNotify('M','at_ah_galerkin_MMex_Initial(Type2)',&
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
         call MessageNotify('E','ah_at(Type2)',&
                            'at_ah_galerkin_MMex_module not initialized')

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
         call MessageNotify('E','at_ah(Type2)',&
                            'at_ah_galerkin_MMex_module not initialized')
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
