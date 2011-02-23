!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ef_module デモプログラム : 地衡流調節
!
!履歴  2009/12/13  竹広真一
!
!      線形浅水方程式   
!
!       du/dt - fv = -g dh/dx
!       dv/dt + fu = -g dh/dy
!       dh/dt = -H(du/dx + dv/dy)
!
program ef_module_gsadj

  use dc_message, only : MessageNotify
  use gtool_history
  use ef_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=8, jm=256           ! 格子点の設定(X,Y)
  integer, parameter :: km=2, lm=128           ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: yrad = 1.0d0

 !---- 物理パラメター ----
  real(8), parameter :: f   = 1.0               ! コリオリパラメター
  real(8), parameter :: g   = 1.0               ! 重力加速度
  real(8), parameter :: H0  = 1.0               ! 平均表面変位
  real(8), parameter :: y0  = 0.2               ! 初期変位の大きさ

  real(8), parameter :: HDC = 0.0D-16           ! 超粘性拡散係数
  integer, parameter :: NHD = 2                 ! 超粘性拡散係数

 !---- 計算パラメター ----
  real(8), parameter :: delta_t = 1.0D-2        ! 時間刻み
  integer, parameter :: ntmax   = 5000          ! 計算時間ステップ数
  integer, parameter :: ndisp   = 100           ! 出力時間ステップ数

 !---- 変数 ----
  real(8)            :: ef_U(-lm:lm,-km:km)    ! 速度(X) 
  real(8)            :: ef_V(-lm:lm,-km:km)    ! 速度(Y) 
  real(8)            :: ef_H(-lm:lm,-km:km)    ! 表面変位

  real(8)            :: yx_U(0:jm-1,0:im-1)    ! 速度(X) 
  real(8)            :: yx_V(0:jm-1,0:im-1)    ! 速度(Y) 
  real(8)            :: yx_H(0:jm-1,0:im-1)    ! 表面変位

  real(8)            :: time=0.0D0
  integer            :: it, j

  call MessageNotify('M','ef_module_gsadj', &
       'ef_module demo program : geostrophic adjustment problem')

 !---------------- 座標値の設定 ---------------------
  call ef_initial(im,jm,km,lm,xmin,xmax,yrad)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  do j=0,jm-1
     if ( -y0/2 < y_Y(j) .AND. y_Y(j) < y0/2 ) then
        yx_H(j,:) = 1.0D0
     else
        yx_H(j,:) = 0.0D0
     endif
  end do


!!$  yx_H = (tanh((yx_Y+y0/2)/(y0/5D1))-tanh((yx_Y-y0/2)/(y0/5D1)))/2.0D0

  ef_H = ef_yx(yx_H)
  ef_U = 0.0D0
  ef_V = 0.0D0

  call output_history_init
  call output_history

 !------------------- 時間積分 ----------------------

  do it=1, ntmax
     time = it*delta_t
     ef_U = ef_U &
          + delta_t*(   f*ef_V - g*ef_yx(yx_Dx_ef(ef_H)) + ef_HDIff_ef(ef_U) )
     ef_V = ef_V &
          + delta_t*( - f*ef_U - g*ef_yx(yx_Dy_ef(ef_H)) + ef_HDiff_ef(ef_V) )
     ef_H = ef_H &
          + delta_t*( - H0*ef_yx(yx_Dx_ef(ef_U)+yx_Dy_ef(ef_V)) + ef_HDiff_ef(ef_H) )

     if ( mod(it,ndisp) == 0.0 ) then
        call output_history
     end if
  enddo

  call output_history_close
contains

!=========================== 超粘性・拡散 ============================
  function ef_HDiff_ef(ef_Data)
    real(8),intent(IN)   :: ef_Data(-lm:lm,-km:km) 
    real(8)              :: ef_HDiff_ef(-lm:lm,-km:km) 

    integer :: n

    ef_HDiff_ef = ef_Data
    do n=1,NHD/2
       ef_HDiff_ef = -ef_yx(yx_Lapla_ef(ef_HDiff_ef))
    enddo

    ef_HDiff_ef = - HDC * ef_HDiff_ef

  end function ef_HDiff_ef

!=========================== ヒストリー出力 ============================
 !
 ! ヒストリー出力初期化
 !
  subroutine output_history_init

    call HistoryCreate( &
           file='ef_module_gstadj.nc', &
           title='Sample program of spmodel/ef_module',  &
           source='ef_module_gstadj.f90 (2009/12/13)',   &
           institution='GFD_Dennou Club SPMODEL project',&
           dims=(/'x','y','t'/), &
           dimsizes=(/im,jm,0/), &
           longnames=(/'X           ','Y           ',&
                       'time        '/),&
           units=(/'1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t), &
           xtypes=(/'double','double','double'/))
    call HistoryPut('x',x_X )                               ! 変数出力
    call HistoryAddattr('x','topology','circular')          ! 周期属性
    call HistoryAddattr('x','modulo',xmax-xmin )            ! 周期属性
    call HistoryPut('y',y_Y )                               ! 変数出力

    call HistoryAddVariable( &                              ! 変数定義
           varname='x_weight', dims=(/'x'/), & 
           longname='Weight for integration in X', &
           units='1', xtype='double')
    call HistoryPut('x_weight',x_X_Weight)                  ! 変数出力

    call HistoryAddVariable( &                              ! 変数定義
           varname='y_weight', dims=(/'y'/), & 
           longname='Weight for integration in Y', &
           units='1', xtype='double')
    call HistoryPut('y_weight',y_Y_Weight)                  ! 変数出力

   !---- パラメター定義, 出力 ----

   !---- 物理変数定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='u', dims=(/'x','y','t'/), & 
           longname='x-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='v', dims=(/'x','y','t'/), & 
           longname='y-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='h', dims=(/'x','y','t'/), & 
           longname='surface height', units='1', xtype='double')

  !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('x','+delta_t', delta_t )
    call HistoryAddAttr('x','+f',    f  )
    call HistoryAddAttr('x','+g',    g  )
    call HistoryAddAttr('x','+H0',   H0 )
    call HistoryAddAttr('x','+y0',   y0 )

  end subroutine output_history_init

 !
 ! ヒストリー出力
 !
  subroutine output_history
    write(6,*) ' History file output at it = ',it, '  time = ', time
    call HistoryPut('t',time)

   !---- 物理変数出力 ----
    yx_U = yx_ef(ef_U)
    yx_V = yx_ef(ef_V)
    yx_H = yx_ef(ef_H)

    call HistoryPut('u',transpose(yx_U))
    call HistoryPut('v',transpose(yx_V))
    call HistoryPut('h',transpose(yx_H))

  end subroutine output_history

 !
 ! ヒストリー出力終了
 !
  subroutine output_history_close
    call HistoryClose
  end subroutine output_history_close

end program ef_module_gsadj

