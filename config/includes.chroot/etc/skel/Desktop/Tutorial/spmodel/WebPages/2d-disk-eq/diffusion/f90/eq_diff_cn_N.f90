!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eq_module
!      球内のトロイダル磁場拡散問題(Crank-Nicolson scheme)
!
!      チェビシェフータウ法による境界条件適用
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球領域での拡散型方程式の解析解」
!
!      (eq_diffusion.pdf)を参照のこと. 
!
!履歴  2008/04/15  竹広真一
!
program eq_diff_cn_N

  use dc_message
  use dc_string,   only : StoA
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
  real(8), dimension(jm,0:im-1)      :: rp_Temp   ! トロイダル磁場
  real(8), dimension(-km:km,0:lm)    :: eq_Temp    ! トロイダル磁場
  real(8), dimension(-km:km,jm)      :: er_Temp    ! トロイダル磁場

  real(8), dimension(jm,0:im-1)     :: rp_TempInit ! トロイダル磁場(初期値)
  real(8), dimension(jm,0:im-1)     :: rp_TempSol  ! トロイダル磁場(解析解)

 ! Crank Nicholson 陰的計算用拡散行列(トロイダル磁場)
  real(8)             :: DifLUMT_Temp(-km:km,0:lm,0:lm)
  integer             :: kpivot_Temp(-km:km,0:lm)    ! ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: kappa=1.0                  ! 拡散係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! 動径波数
  real(8) :: sigma                   ! 成長率

 !---- その他 ----
  integer :: it=0
  real(8) :: time=0                  ! 時間
  integer :: j,k,n

 !---------------- 座標値の設定 ---------------------
  call eq_Initial(im,jm,km,lm,ra)

  call CNDiffusionMatrix( kappa, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- 初期値設定 ----------------------
  write(6,*)'k,n?'
  read(5,*)k,n

  alpha = CalAlpha(k,n)
  sigma = -kappa* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  er_Temp = 0.0
  do j=1,jm
     er_Temp(k,j) = bessj(k,alpha*r_Rad(j))
  enddo
  rp_Temp = rp_er(er_Temp)
  rp_TempInit = rp_Temp
  eq_Temp = eq_rp(rp_Temp)

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     time = it * dt
     eq_Temp = eq_Temp + dt/2 * eq_er( kappa * er_Lapla_eq(eq_Temp) )
     call eq_Boundary(eq_Temp,cond='N')
     eq_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,eq_Temp)

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
   !-----------------------------------------
   ! dbjdx(ra*x)=0 の解を求める
   !
    function CalAlpha(l,n)
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
      xl = dx ; ValL = dbjdx(l,ra*xl)

      !
      ! det = 0 となる点を x=0 から刻 dx の幅で探していき
      ! n 番目を挟む xl, xs を設定する.
      !
100   continue
      xs =xl ; ValS = ValL
      xl = xl+dx
      ValL = dbjdx(l,ra*xl)
      if( ValL*ValS .le. 0.0 ) nn=nn-1
      if( nn .lt. 0 ) goto 199
      goto 100
199 continue

1000  xm = (xs + xl)/2.0
      ValM = dbjdx(l,ra*xm)
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=dbjdx(l,ra*xs)
      else
         xl = xm ; ValL=dbjdx(l,ra*xl)
      endif

      if ( abs(xl-xs) .lt. eps ) then
         CalAlpha = xm
         goto 99
      endif

      goto 1000

99    continue
    end function CalAlpha

 !------------------- 拡散項 ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
    real(8), dimension(-km:km,0:lm,0:lm), intent(OUT)  :: DiffLUMatrix
    ! ピボット情報
    integer, dimension(-km:km,0:lm), intent(OUT)       :: kpivot

    ! 作業用変数
    real(8), dimension(-km:km,0:lm)           :: eq_I
    real(8), dimension(-km:km,0:lm)           :: eq_DI

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       eq_I = 0.0 ; eq_I(:,l) = 1.0             ! 各波数成分独立
       eq_DI =  - Diffc * dt/2.0 * eq_er(er_Lapla_eq(eq_I))
       call eq_Boundary(eq_DI,cond='N')
       DiffLUMatrix(:,:,l) = eq_I + eq_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='eq_diff_cn_N.nc', &
           title='Diffusion model in a circle', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'phi','rad','t  '/), dimsizes=(/im,jm,0/),&
           longnames=StoA('Azimuth','Radius','time'),&
           units=(/'1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('phi',p_Phi/pi*180)                       ! 変数出力
    call HistoryPut('rad',r_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='temp', dims=(/'rad','phi','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tempsol', dims=(/'rad','phi','t  '/), & 
           longname='temperature(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='temperror', dims=(/'rad','phi','t  '/), & 
           longname='temperature(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    rp_Temp = rp_eq(eq_Temp)
    rp_TempSol = rp_TempInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',rp_Temp)
    call HistoryPut('tempsol',rp_TempSol)
    call HistoryPut('temperror',rp_Temp-rp_TempSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program eq_diff_cn_N
