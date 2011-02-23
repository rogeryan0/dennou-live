!-------------------------------------------------------------------------
! Copyright (C) 2002--2005 SPMODEL Development Group. All rights reserved.
!-------------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)   
!
!    2002/09/18 S.Takehiro
!    2004/01/26 M.Odaka
!    2004/06/17 M.Odaka
!    2005/03/16 S.Takehiro
! 
! Solving 2-D Boussinesq fluid system (Infinite Prandtl number, TT problem)
!     Ra dT/dx + \nabla\zeta = 0.
!     dT/dt + J(\psi,T) - d\psi/dx = \nabla T
!     \nabla\psi = \zeta
!     psi = zeta = T = 0 at y=0,1
!
  program infprcnv1

    use esc_module
    use gt4_history
    implicit none

 !---- 空間解像度設定 ----
    integer, parameter :: km=21, lm=21           ! 切断波数の設定(X,Y)
    integer, parameter :: im=64, jm=32           ! 格子点の設定(X,Y)

 !---- 変数 ----
    real(8)            :: yx_Psi(0:jm,0:im-1)    ! 格子データ(流線)
    real(8)            :: yx_Temp(0:jm,0:im-1)   ! 格子データ(温度/温度擾乱)
    real(8)            :: yx_Zeta(0:jm,0:im-1)   ! 格子データ(渦度)

    real(8)            :: es_Psi(-km:km,lm)      ! スペクトルデータ(流線)
    real(8)            :: es_Zeta(-km:km,lm)     ! スペクトルデータ(渦度)

    real(8)            :: es_TempA(-km:km,lm)  ! スペクトルデータ(温度擾乱,t+1)
    real(8)            :: es_TempB(-km:km,lm)  ! スペクトルデータ(温度擾乱,t)

 !---- 座標変数など ----
    real(8), parameter :: xmin=0.0, xmax=2.0     ! 領域範囲(X 方向) 
    real(8), parameter :: ymin=0.0, ymax=1.0     ! 領域範囲(Y 方向) 

 !---- 時間積分パラメター ----
    real(8), parameter :: dt=1e-4                ! 時間ステップ間隔
    integer, parameter :: nt=5000, ndisp=250     ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
    real(8), parameter :: Ra=1.0e4               ! レイリー数

    integer            :: it                     ! DO 変数

 !---------------- 座標値の設定 ---------------------
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ISPACK初期化

 !------------------- 初期値設定 ----------------------
    yx_Temp = 0.0 ;  yx_Temp(jm/2,im/2) = 0.01           ! 温度擾乱場
    yx_Psi  = 0.0
    yx_Zeta = 0.0

    es_TempA = es_yx(yx_Temp) ; es_TempB = es_TempA
    es_Psi  = es_yx(yx_Psi)  
    es_Zeta = es_yx(yx_Zeta) 
 
    call output_gtool4_init                              ! ヒストリー初期化
    call output_gtool4                                   ! 初期値出力

 !------------------- 時間積分 ----------------------
    do it=1,nt
       es_TempA = es_TempB + &
            dt*( -es_Jacobian_es_es(es_Psi,es_TempB) &
                 + es_Dx_es(es_Psi) &
                 + es_Lapla_es(es_TempB) )
       es_Zeta = es_LaplaInv_es(-Ra*es_Dx_es(es_TempA))
       es_Psi = es_LaplaInv_es(es_Zeta)

       es_TempB = es_TempA

       if(mod(it,ndisp) .eq. 0)then                        ! 出力
          call output_gtool4
       endif
    enddo

    call output_gtool4_close
    stop

  contains
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='infprcnv1.nc', &
           title='Infinite Prandtl number Boussinesq convection (TT)', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
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
           longname='vorticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      yx_Temp = yx_es(es_TempA) + 1 - yx_Y                     ! 出力は全温度場
      yx_Psi  = yx_es(es_Psi)
      yx_Zeta = yx_es(es_Zeta)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('temp',transpose(yx_Temp))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program infprcnv1

