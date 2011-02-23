!--------------------------------------------------------------------------
!  Copyright (C) 2002--2005 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)
!
!    2002/09/24 S.Takehiro
!    2004/01/26 M.Odaka
!    2004/06/17 M.Odaka
!    2005/03/16 S.Takehiro
! 
! Solving 2-D Boussinesq fluid system 
!     d\zeta/dt 
!        + Ud\zeta/dx - dU^2/dy^2 dpsi/dx + J(\psi,\zeta) 
!        + g/\rho_0 d\rho/dx - nu\lapla\zeta = 0,
!
!     \nabla\psi = \zeta
!
!     d\rho/dt + Ud\rho/dx + J(\psi,\rho) = D\nabla\rho
!
!     psi = zeta = 0 at y=0,1
!
! where U(y) is basic flow.
!
  program kh1

    use esc_module
    use gt4_history
    implicit none

 !---- 空間解像度設定 ----
    integer, parameter :: km=42 , lm=42         ! 切断波数の設定(X,Y)
    integer, parameter :: im=128, jm=64         ! 格子点の設定(X,Y)

 !---- 変数 ----
    real(8)            :: yx_Psi(0:jm,0:im-1)     ! 格子データ(流線)
    real(8)            :: yx_Zeta(0:jm,0:im-1)    ! 格子データ(渦度)
    real(8)            :: yx_Rho(0:jm,0:im-1)     ! 格子データ(密度)
    real(8)            :: yx_U(0:jm,0:im-1)       ! 格子データ(基本流)
    real(8)            :: yx_dU2dy2(0:jm,0:im-1)  ! 格子データ(基本流)

    real(8)            :: es_PsiA(-km:km,lm)      ! スペクトルデータ(流線,t+1)
    real(8)            :: es_ZetaA(-km:km,lm)     ! スペクトルデータ(渦度,t+1)
    real(8)            :: ec_RhoA(-km:km,0:lm)    ! スペクトルデータ(密度,t+1)

    real(8)            :: es_PsiB(-km:km,lm)      ! スペクトルデータ(流線,t)
    real(8)            :: es_ZetaB(-km:km,lm)     ! スペクトルデータ(渦度,t)
    real(8)            :: ec_RhoB(-km:km,0:lm)    ! スペクトルデータ(密度,t)

 !---- 座標変数など ----
    real(8), parameter :: xmin=0.0, xmax=18.0     ! 領域範囲(X 方向) 
    real(8), parameter :: ymin=0.0, ymax=6.0      ! 領域範囲(Y 方向) 

 !---- 時間積分パラメター ----
    real(8), parameter :: dt=0.25d-4               ! 時間ステップ間隔
    integer, parameter :: nt=40000, ndisp=800     ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
    real(8), parameter :: Grav=9.8d2               ! 重力加速度
    real(8), parameter :: Visc=1.0d-1              ! 動粘性係数
    real(8), parameter :: Diff=1.0d-5              ! 拡散係数

    real(8), parameter :: U0 = 3.0d1               ! 基本流の分布(速度差)
    real(8), parameter :: A0 = 0.2                 ! 基本流の分布(シアー層の幅)
    real(8), parameter :: EPS= 1e-6                ! 初期擾乱の速度振幅

    real(8), parameter :: deltaRho = 0.05          ! 密度差
    real(8), parameter :: Rho0     = 1.0d0         ! 下層の密度

 !--- 超粘性 ---
    real(8), parameter :: nu=1e-10                 ! 粘性係数のfactor
    integer, parameter :: nv=5                     ! 粘性の階数
    real(8)            :: es_sVisc(-km:km,lm)      ! スペクトルデータ(粘性係数)
    real(8)            :: ec_sDiff(-km:km,0:lm)    ! スペクトルデータ(粘性係数)

    real(8), parameter :: pi = 3.141592653589793   ! 円周率

    integer            :: i, j, k, l, it           ! DO 変数
    real               :: rnum

 !---------------- 座標値の設定 ---------------------
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)      ! ISPACK初期化

 !------------------- 初期値設定 ----------------------

   ! 流速分布
    yx_U      = U0 * tanh((yx_Y-(ymin+ymax)/2)/A0)
    !yx_dU2dY2 = yx_ec(ec_dy_es(es_dy_ec(ec_yx(yx_U))))     ! 境界で dU0/dy=0
    yx_dU2dY2 = -2*U0/A0**2 * sinh((yx_Y-(ymin+ymax)/2)/A0) &
	                    / cosh((yx_Y-(ymin+ymax)/2)/A0)**3 

    !yx_Psi = EPS * cos(2*pi/(xmax-xmin)*yx_X) &
    !             * cos(2*pi/(ymax-ymin)*yx_Y)
    !yx_Psi = 0.0 ; yx_Psi(jm/2,im/2) = EPS
    do j=0,jm
	do i=0,im-1
	   call RANDOM_NUMBER(rnum)
	   yx_Psi(j,i)=EPS * rnum
	enddo
    enddo

    es_PsiA  = es_yx(yx_Psi)        ;  yx_Psi = yx_es(es_PsiA) 
    es_ZetaA = es_Lapla_es(es_PsiA) ;  yx_Zeta  = yx_es(es_ZetaA)


   ! 密度分布
    yx_Rho    = Rho0 - deltaRho * (tanh((yx_Y-(ymin+ymax)/2)/A0) + 1)/2
    ec_RhoA    = ec_yx(yx_Rho)

    es_ZetaB = es_ZetaA
    es_PsiB  = es_PsiA
    ec_RhoB  = ec_RhoA

    call output_gtool4_init                              ! ヒストリー初期化
    call output_gtool4                                   ! 初期値出力

 !---------------------- 超粘性設定 ----------------------
    do l=1,lm
       do k=-km,km
          es_sVisc(k,l)= Visc * nu &
                    * ((2*pi*k/(xmax-xmin))**2+(2*pi*l/(ymax-ymin))**2)**nv
       enddo
    enddo

    do l=0,lm
       do k=-km,km
          ec_sDIff(k,l)= Diff * nu &
                     * ((2*pi*k/(xmax-xmin))**2+(2*pi*l/(ymax-ymin))**2)**nv
       enddo
    enddo

 !---------------------- 時間積分 ----------------------
    do it=1,nt
       ec_RhoA = ( ec_RhoB + &
                  dt*( - ec_Jacobian_es_ec(es_PsiB,ec_RhoB)       &
                       - ec_yx(yx_U*yx_ec(ec_dx_ec(ec_RhoB)))    &
                       + Diff * ec_Lapla_ec(ec_RhoB) )           &
                 )/(1+ec_sDiff*dt)

       es_ZetaA = ( es_ZetaB +                                 &
                    dt*( - es_Jacobian_es_es(es_PsiB,es_ZetaB)       &
                         - es_yx(yx_U*yx_es(es_dx_es(es_ZetaB)))    &
                         + es_yx(yx_dU2dy2*yx_es(es_dx_es(es_PsiB)))&
                         - Grav/Rho0 * es_yx(yx_ec(ec_Dx_ec(ec_RhoB))) &
                         + Visc * es_lapla_es(es_ZetaB) )           &
                  )/(1+es_sVisc*dt)

       es_PsiA = es_LaplaInv_es(es_ZetaA)

       es_ZetaB = es_ZetaA
       es_PsiB  = es_PsiA
       ec_RhoB  = ec_RhoA

       if(mod(it,ndisp) .eq. 0)then                        ! 出力
          call output_gtool4
       endif
    enddo

    call output_gtool4_close
    stop

  contains
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='kh1.nc', title='K-H instability', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'cm','cm','s '/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                                   ! 変数出力
      call HistoryAddattr('x','topology','circular')             ! 周期属性
      call HistoryAddattr('x','modulo',xmax-xmin)                ! 周期属性
      call HistoryPut('y',y_Y)                                   ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='psi', dims=(/'x','y','t'/), & 
           longname='stream function', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='voticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='rho', dims=(/'x','y','t'/), & 
           longname='density', units='g/cm3', xtype='double')

      call HistoryAddVariable( &                          ! 変数定義
           varname='u0', dims=(/'y'/), & 
           longname='basic flow', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='d2u0dy2', dims=(/'y'/), & 
           longname='d2U0/dy', units='1', xtype='double')
      call HistoryPut('u0',yx_U(:,1))
      call HistoryPut('d2u0dy2',yx_dU2dy2(:,1))
    end subroutine output_gtool4_init

    subroutine output_gtool4
       yx_Psi  = yx_es(es_PsiA)
       yx_Zeta = yx_es(es_ZetaA)
       yx_Rho = yx_ec(ec_RhoA)

      write(6,*) 'it = ',it
      call HistoryPut('t',dt*it)
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('rho',transpose(yx_Rho))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program kh1
