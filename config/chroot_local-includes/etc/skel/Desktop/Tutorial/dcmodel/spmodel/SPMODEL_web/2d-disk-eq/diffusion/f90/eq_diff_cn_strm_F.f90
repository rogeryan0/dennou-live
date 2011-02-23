!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eq_module サンプルプログラム
!      円盤内の流線関数拡散問題(Crank-Nicolson scheme)
!      外側すべり条件
!
!      計算結果比較のための解析解については解説文書
!
!         「2 次元円盤領域での拡散型方程式の解析解」
!
!      (eq_diffusion.pdf)を参照のこと. 
!
!履歴  2008/04/16  竹広真一
!
program eq_diff_cn_polmag_F
  !
  !  eq_module サンプルプログラム
  !      円盤内の流線関数拡散問題(Crank-Nicolson scheme)
  !      外側すべり粘着条件
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「2 次元円盤領域での拡散型方程式の解析解」
  !
  !      (eq_diffusion.pdf)を参照のこと. 
  !
  use dc_message
  use lumatrix
  use eq_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16         ! 格子点の設定(方位角, 動径)
  integer,parameter  :: km=10, lm=21         ! 切断波数の設定(方位角, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ra=1.5               ! 球半径

 !---- 変数 ----
  real(8), dimension(jm,0:im-1)   :: rp_Strm      ! 流線関数
  real(8), dimension(-km:km,0:lm) :: eq_Strm      ! 流線関数
  real(8), dimension(-km:km,jm)   :: er_Strm      ! 流線関数
  real(8), dimension(-km:km,0:lm) :: eq_Vor ! 渦度(▽^2流線)

  real(8), dimension(jm,0:im-1)  :: rp_StrmInit  ! ポロイダル速度場(初期値)
  real(8), dimension(jm,0:im-1)  :: rp_StrmSol   ! ポロイダル速度場(解析解)

 ! Crank Nicholson 設定
  real(8)             :: DifLUMT_Strm(-km:km,0:lm,0:lm)
                         ! Crank Nicholson 陰的計算用拡散行列(ポロイダル速度場)
  integer             :: kpivot_Strm(-km:km,0:lm)
                         ! Crank Nicholson 陰的計算用拡散行列ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: nu=1.0                  ! 粘性係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! 動径波数
  real(8) :: sigma                   ! 成長率
  real(8) :: C(2)                    ! 独立解の係数

 !---- その他 ----
  integer :: it=0
  real(8) :: time=0                  ! 時間
  integer :: j,k,l

 !---------------- 座標値の設定 ---------------------
  call eq_Initial(im,jm,km,lm,ra)

  call CNDiffusionMatrixStrm( nu, dt, DifLUMT_Strm, kpivot_Strm )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Azimuthal, radial wavenumbers: k,l?'
  read(5,*)k,l

  alpha = CalAlpha(k,l)
  sigma = -nu* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Groeqh rate       : ', sigma

  C = CalcCoeff(k,alpha)

  er_Strm = 0.0
  do j=1,jm
     er_Strm(k,j) &
        =   C(1)*r_Rad(j)**k  + C(2)*bessj(k,alpha*r_Rad(j))
  enddo

  eq_Strm = eq_er(er_Strm)
  rp_StrmInit = rp_eq(eq_Strm)
  eq_Vor = eq_er(er_Lapla_eq(eq_Strm))

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     time = it * dt

     eq_Vor = eq_Vor &
          + dt/2 *( nu * eq_er(er_Lapla_eq(eq_Vor))  )
     eq_Strm = eq_Vor2Strm_eq(eq_Vor,cond='F')
     eq_Strm = LuSolve(DifLUMT_Strm,kpivot_Strm,eq_Strm)
     eq_Vor = eq_er(er_Lapla_eq(eq_Strm))

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains
  !
  ! Bessel functions
  !
  function bessj(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: bessj
    integer             :: icon

    call DBJN(x,n,bessj,icon)

    if ( icon /= 0 ) then
       write(6,*) 'icon=',icon
       call MessageNotify('E','bessj','SSL2 ROUTINE DBJN ERROR')
    endif

  end function bessj

  function dbjdx(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: dbjdx
    real(8)             :: bessj, bessj1
    integer             :: icon

    call DBJN(x,n,bessj,icon)
    if ( icon /= 0 ) then
       write(6,*) 'icon=',icon
       call MessageNotify('E','dbjdx','SSL2 ROUTINE DBJN ERROR')
    endif
    call DBJN(x,n+1,bessj1,icon)
    if ( icon /= 0 ) then
       write(6,*) 'icon=',icon
       call MessageNotify('E','dbjdx','SSL2 ROUTINE DBJN ERROR')
    endif

    dbjdx = n/x*bessj - bessj1

  end function dbjdx

  function d2bjdx2(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: d2bjdx2

    d2bjdx2 = - dbjdx(n,x)/x - (1-n**2/x**2) * bessj(n,x)

  end function d2bjdx2

  recursive function detNxN(a) result(determinant)
    !
    ! NxN 行列の行列式の計算
    !
    implicit none
    real(8),intent(IN) :: a(:,:)
    real(8) :: determinant
    real(8),allocatable :: b(:,:)
    integer :: i,n

    if ( size(a,1) /= size(a,2) )then
       write(6,*) '**** detNxN **** Matrix is not square. '
       stop
    endif

    n=size(a,1)

    if ( n == 2 ) then
       determinant = a(1,1)*a(2,2)-a(2,1)*a(1,2)
    else
       allocate(b(n-1,n-1))
       b = a(2:n,2:n)
       determinant = a(1,1) * detNxN(b)
       do i=2,n-1
          b(1:i-1,:) = a(1:i-1,2:n)
          b(i:n-1,:) = a(i+1:n,2:n)
          determinant = determinant + (-1)**(i-1)*a(i,1)*detNxN(b)
       enddo
       b = a(1:n-1,2:n)
       determinant = determinant + (-1)**(n-1)*a(n,1)*detNxN(b)
       deallocate(b)
    end if

    return 

  end function detNxN

  function det(l,x)
    !
    ! 係数行列式Δの計算
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! 行列式
    real(8)             :: a(2,2)            ! 行列

    a(1,1) = ra**l             ; a(1,2) = bessj(l,ra*x)
    a(2,1) = l*(l-2)*ra**(l-2) ; a(2,2) = x**2* d2bjdx2(l,ra*x) - x/ra*dbjdx(l,ra*x)

    det=detNxN(a)

  end function det

  function CalcCoeff(l,alpha)
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: alpha             ! 動径波数
    real(8)             :: CalcCoeff(2)      ! 係数

    CalcCoeff(1) = bessj(l,ra*alpha) 
    CalcCoeff(2) = -ra**l

  end function CalcCoeff

  function CalAlpha(l,n)
    !
    ! Δ=0 の解を求める
    !
    integer, intent(IN) :: l                 ! 次数
    integer, intent(IN) :: n                 ! 求める解の番号
    real(8)             :: CalAlpha
    real(8), parameter  :: eps = 1.0D-14     ! 解の精度

    real(8) :: PI
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    real(8) :: dx
    integer :: nx=20
    integer :: nn

    nn = n
    PI = atan(1.0D0)*4.0D0
    dx = PI/ra/nx
    xl = dx ; ValL = det(l,xl)

    !
    ! det = 0 となる点を x=0 から刻 dx の幅で探していき
    ! n 番目を挟む xl, xs を設定する.
    !
100 continue
       xs =xl ; ValS = ValL
       xl = xl+dx
       ValL = det(l,xl)
       if( ValL*ValS .le. 0.0 ) nn=nn-1
       if( nn .lt. 0 ) goto 199
       goto 100
199 continue
        
1000 xm = (xs + xl)/2.0
    ValM = det(l,xm)
      
    if ( ValS * ValM .GT. 0.0D0 ) then
       xs = xm ; ValS = det(l,xs)
    else
       xl = xm ; ValL = det(l,xl)
    endif
     
    if ( abs(xl-xs) .lt. eps ) then
       CalAlpha = xm
       goto 99
    endif
     
    goto 1000
     
99  continue
  end function CalAlpha
    
    !------------------- 拡散項 ----------------------
    subroutine CNDiffusionMatrixStrm( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (1-D dt/2▽^2, LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 設定
      real(8), intent(OUT)  :: DiffLUMatrix(-km:km,0:lm,0:lm)
                              ! Crank Nicholson 拡散陰的計算用行列
                              ! (1-D dt/2▽^2, LU 分解)

      integer, intent(OUT)  :: kpivot(-km:km,0:lm)     ! ピボット情報
                              ! Crank Nicholson 拡散陰的計算用行列ピボット情報

      real(8)               :: eq_I(-km:km,0:lm)       ! 作業用変数
      real(8)               :: eq_DI(-km:km,0:lm)       ! 作業用変数

      integer :: l

      DiffLUMatrix = 0.0
      do l=0,lm
         eq_I = 0.0 ; eq_I(:,l) = 1.0             ! 各波数成分独立
         eq_DI = - Diffc * dt/2.0 &
                  * eq_er(er_Lapla_eq(eq_er(er_Lapla_eq(eq_I))))
         eq_DI = eq_Vor2Strm_eq(eq_DI,cond='F')
         DiffLUMatrix(:,:,l) = eq_I + eq_DI
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixStrm

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    !
    ! ヒストリー出力初期化ルーチン
    !
    !   ファイル作成
    !   変数定義
    !
    call HistoryCreate( &                                  ! ヒストリー作成
           file='eq_diff_cn_strm_F.nc', &
           title='Diffusion model in a circle', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'phi','rad','t  '/), dimsizes=(/im,jm,0/),&
           longnames=(/'Azimuth','Radius ','time   '/),&
           units=(/'1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('phi',p_Phi/pi*180)                       ! 変数出力
    call HistoryPut('rad',r_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='strm', dims=(/'rad','phi','t  '/), & 
           longname='stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='strmsol', dims=(/'rad','phi','t  '/), & 
           longname='stream function (analytic)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='strmerror', dims=(/'rad','phi','t  '/), & 
           longname='stream function (error)', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! ヒストリー出力
    !
    !   各物理量の gtool4 ファイルへの出力を行う
    !
    write(6,*) 'it = ',it
    rp_Strm = rp_eq(eq_Strm)
    rp_StrmSol = rp_StrmInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('strm',rp_Strm)
    call HistoryPut('strmsol',rp_StrmSol)
    call HistoryPut('strmerror',rp_Strm-rp_StrmSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! ヒストリー出力終了処理
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program eq_diff_cn_polmag_F
