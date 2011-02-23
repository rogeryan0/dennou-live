!----------------------------------------------------------------------
!  Copyright (C) 2001--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/gt4f90io and ISPACK   2002/08/21 S.Takehiro
!                                                      2004/01/26 M.Odaka
!
! Solving a linear 2-D shallow water system on a sphere 
!     with an isolated mountain
!     du/dt + u0/a\cos\phi du/d\lambda + v/a du0/d\phi - u0v\tan\phi/a 
!           - 2\Omega\sin\phi v = -g/a\cos\phi dh/d\lambda - \nu\lapla^4 u,
!     dv/dt + u0/a\cos\phi dv/d\lambda + 2 u0 u \tan\phi/a 
!           + 2\Omega\sin\phi u  =  -g/a  dh/d\phi - \nu\lapla^4 v,
!     dh/dt + ( 1/\cos\phi d( (H+h0)u+(h-ht)u0 )/d\lambda 
!               + 1/\cos\phi d( (H+h0)v\cos\phi)/d\phi ) = - \nu\lapla^4 h.
!
! A setup is similar to the experiment of Grose and Hoskins (1979) 
! with a superrotating rigid-rotation zonal wind profile. 
!
  program spshallow_topo_linear

    use w_module
    use gt4_history
    implicit none

  !---- ���ֲ��������� ----
!!$    integer, parameter :: im=512, jm=256             ! �ʻ���������(X,Y)
!!$    integer, parameter :: nm=170
!!$    integer, parameter :: im=128, jm=64             ! �ʻ���������(X,Y)
!!$    integer, parameter :: nm=42
!!$    integer, parameter :: im=96, jm=48             ! �ʻ���������(X,Y)
!!$    integer, parameter :: nm=31
    integer, parameter :: im=64, jm=32             ! �ʻ���������(X,Y)
    integer, parameter :: nm=21

  !---- �ѿ� ----
    real(8)            :: xy_U(0:im-1,jm)          ! �ʻ����ǡ���(®�ٷ�����ʬ)
    real(8)            :: xy_V(0:im-1,jm)          ! �ʻ����ǡ���(®�ٰ�����ʬ)
    real(8)            :: xy_H(0:im-1,jm)          ! �ʻ����ǡ���(�Ѱ�)

    real(8)            :: w_U((nm+1)*(nm+1))       ! ���ڥ��ȥ�(®�ٷ�����ʬ)
    real(8)            :: w_V((nm+1)*(nm+1))       ! ���ڥ��ȥ�(®�ٰ�����ʬ)
    real(8)            :: w_H((nm+1)*(nm+1))       ! ���ڥ��ȥ�(�Ѱ�)

    real(8)            :: xy_U0(0:im-1,jm)         ! �ʻ����ǡ���(®�ٷ�����ʬ)
    real(8)            :: xy_H0(0:im-1,jm)         ! �ʻ����ǡ���(�Ѱ�)
    real(8)            :: w_U0((nm+1)*(nm+1))      ! ���ڥ��ȥ�(®�ٷ�����ʬ)
    real(8)            :: w_H0((nm+1)*(nm+1))      ! ���ڥ��ȥ�(�Ѱ�)

    real(8)            :: xy_Zeta(0:im-1,jm)       ! �ʻ����ǡ���(����)
    real(8)            :: xy_Htopo(0:im-1,jm)      ! �ʻ����ǡ���(�Ϸ�)

  !---- ������ʬ�ѥ�᥿�� ----
    real(8), parameter :: dt = 100                 ! ���֥��ƥå״ֳ�
    integer, parameter :: nt = 15000, ndisp = 500  ! ������ʬ��, ɽ�����ƥå�
 
  !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: pi = 3.141592653589793   ! �߼�Ψ
    real(8), parameter :: R0 = 6.371D6             ! ���Ⱦ��
    real(8), parameter :: H0 = 10D3                ! ʿ���Ѱ�
    real(8), parameter :: Omega = 7.292D-5         ! ��ž��®��
    real(8), parameter :: Grav = 9.8               ! ���ϲ�®��

    real(8), parameter :: U0   = 15                ! ʿ��ή
    real(8), parameter :: Lon0 = pi, Lat0=pi/6     ! �����濴����
    real(8), parameter :: W0 = 8                   ! ���ι�����εտ�
    real(8), parameter :: Htopo0= 2500             ! ���ι⤵

    integer, parameter :: ndiff=4                  ! ĶǴ���μ���
    real(8), parameter :: Nu=2.338D16              ! ĶǴ������

  !------ ����ѿ� ------
    integer            :: i, j, it=0
    real(8)            :: Delta

  !---------------- ��ɸ�ͤ����� ---------------------
    call w_Initial(nm,im,jm)                ! ISPACK�����

  !------------------- ��������� ----------------------
    xy_U0  = U0*cos(xy_Lat)
    xy_H0  = ( Omega*R0*U0/(2*Grav) + U0**2/(4*Grav) )*cos(2*xy_Lat)

    xy_U  = 0 ; xy_V  = 0 ; xy_H  = 0

    w_U0 = w_xy(xy_U0) ; w_H0 = w_xy(xy_H0)
    w_U = w_xy(xy_U) ; w_V = w_xy(xy_V) ; w_H = w_xy(xy_H)

  !------------------- �Ϸ����� ----------------------

    do j=1,jm
       do i=0,im-1
          delta = acos( cos(xy_Lat(i,j))*cos(Lat0)*cos(xy_Lon(i,j)-Lon0) &
                       + sin(xy_Lat(i,j))*sin(Lat0) )
          if ( abs(delta) <= pi/W0 ) then 
             xy_Htopo(i,j)= htopo0 * (1+cos(W0*Delta))/2
          else
             xy_Htopo(i,j)= 0
          endif
       enddo
    enddo

    call output_gtool4_init                        ! �ҥ��ȥ꡼�����
    call output_gtool4                             ! ����ͽ���

  !------------------- ������ʬ ----------------------
    do it=1,nt
       w_U = ( w_U &
               + dt * w_xy( - xy_U0 * xy_GradLon_w(w_U) / R0   &
                            - xy_V  * xy_GradLat_w(w_U0) / R0  &
                            + xy_U0 * xy_V * tan(xy_Lat) / R0  &
                            + 2 * Omega * sin(xy_Lat) * xy_V   &
                           - Grav * xy_GradLon_w(w_H)/ R0   ) &
             )/(1+Nu*(-rn(:,1)/R0**2)**(ndiff/2)*dt)

       xy_U = xy_w(w_U)

       w_V = ( w_V &
              + dt * w_xy( - xy_U0 * xy_GradLon_w(w_V) / R0   &
                           - xy_U * xy_U0 * tan(xy_Lat) / R0  &
                           - xy_U0 * xy_U * tan(xy_Lat) / R0  &
                           - 2 * Omega * sin(xy_Lat) * xy_U   &
                           - Grav * xy_GradLat_w(w_H) / R0 )  &
             )/(1+Nu*(-rn(:,1)/r0**2)**(ndiff/2)*dt)

       xy_V = xy_w(w_V)

       w_H = ( w_H &
              + dt * ( - w_Div_xy_xy( xy_U*(H0+xy_H0)+xy_U0*(xy_H-xy_Htopo),   &
                                      xy_V*(H0+xy_H0) ) / R0      ) &
              )/(1+Nu*(-rn(:,1)/R0**2)**(ndiff/2)*dt)

       xy_H = xy_w(w_H)

       if(mod(it,ndisp) .eq. 0)then                        ! ����
          call output_gtool4
       endif
    enddo

    call output_gtool4_close
    stop

  contains
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='spshallow_topo_linear.nc',                         &
           title='Shallow water equation on a sphere',            &
           source='Sample program of gtool_History/gtool4',       &
           institution='GFD_Dennou Club davis/spmodel project',   &
           dims=(/'lon','lat','t  '/), dimsizes=(/im,jm,0/),      &
           longnames=(/'longitude','latitude ','time     '/),     &
           units=(/'deg.','deg.','sec.'/),                        &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('lon',x_Lon*180/pi)              ! �ѿ�����
      call HistoryAddattr('x','topology','circular')   ! ����°��
      call HistoryAddattr('x','modulo',360.0)          ! ����°��
      call HistoryPut('lat',y_Lat*180/pi)              ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='h', dims=(/'lon','lat','t  '/), & 
           longname='surface displacement ', units='m', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='u', dims=(/'lon','lat','t  '/), & 
           longname='velocity(longitude) ', units='m/s', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='v', dims=(/'lon','lat','t  '/), & 
           longname='velocity(latitude) ', units='m/s', xtype='double')

      call HistoryAddVariable( &                          ! �ѿ����
           varname='zeta', dims=(/'lon','lat','t  '/), & 
           longname='vorticity', units='1/s', xtype='double')

      call HistoryAddVariable( &                          ! �ѿ����
           varname='htopo', dims=(/'lon','lat'/), & 
           longname='orography', units='m', xtype='double')
      call HistoryPut('htopo',xy_Htopo)
    end subroutine output_gtool4_init

    subroutine output_gtool4
      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('u',xy_U)
      call HistoryPut('v',xy_V)
      call HistoryPut('h',xy_H)
      xy_Zeta = xy_w(w_Divlon_xy(xy_V) - w_Divlat_xy(xy_U))/r0
      call HistoryPut('zeta',xy_Zeta)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program spshallow_topo_linear
