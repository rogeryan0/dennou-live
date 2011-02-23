!----------------------------------------------------------------------
!  Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gtool_history/gtool4 and ISPACK
!
!     2002/08/20 S.Takehiro
!     2005/03/15 S.Takehiro
! 
! Solving equatorial beta plane shallow water equations (Gill 1980)
!     du/dt - beta y v = -g dh/dx
!     dv/dt + beta y u = -g dh/dy
!     dh/dt + H0( du/dx + dv/dy ) = -qh
!     v = 0 at y=ymin,ymax
!
  program equatorial_beta_shallow_water
    use et_module
    use gt4_history
    use gt4f90io
    implicit none

 !---- ���ֲ��������� ----
!    integer, parameter :: lm=16,  km=16          ! �����ȿ�������(X,Y)
!    integer, parameter :: im=64,  jm=32          ! �ʻ���������(X,Y)
    integer, parameter :: km=16,  lm=32          ! �����ȿ�������(X,Y)
    integer, parameter :: im=64,  jm=32          ! �ʻ���������(X,Y)

 !---- �ѿ� ----
    real(8)            :: yx_U(0:jm,0:im-1)      ! �ʻҥǡ���(®�� X ��ʬ)
    real(8)            :: yx_V(0:jm,0:im-1)      ! �ʻҥǡ���(®�� Y ��ʬ)
    real(8)            :: yx_H(0:jm,0:im-1)      ! �ʻҥǡ���(ɽ���Ѱ�)
    real(8)            :: et_U(-km:km,0:lm)      ! ���ڥ��ȥ�(®�� X ��ʬ)
    real(8)            :: et_V(-km:km,0:lm)      ! ���ڥ��ȥ�(®�� Y ��ʬ)
    real(8)            :: et_H(-km:km,0:lm)      ! ���ڥ��ȥ�(ɽ���Ѱ�)

    real(8)            :: yx_QH(0:jm,0:im-1)     ! �ʻҥǡ���(Ǯ��ʬ��)
    real(8)            :: et_QH(-km:km,0:lm)     ! ���ڥ��ȥ�(Ǯ��ʬ��)
    !real(8)            :: yx_data(0:jm,0:im-1) 

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=-200.0, xmax=200.0  ! �ΰ��ϰ�(X ����)
    real(8), parameter :: ymin=-5.0, ymax=5.0      ! �ΰ��ϰ�(Y ����) 

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Grav=1.0              ! ���ϲ�®��
    real(8), parameter :: Beta=1.0              ! ��ƻ�¥ѥ�᥿
    real(8), parameter :: H0 = 1.0              ! ʿ�Ѥο���
    real(8), parameter :: Q0=1.0                ! Ǯ��ʬ�ۤο���
    real(8), parameter :: A0=0.2                ! Ǯ��ʬ�ۤ� Y ��������
    real(8), parameter :: Xrange=20.0           ! Ǯ��ʬ�ۤ� X ��������

 !---- ������ʬ�ѥ�᥿�� ----
    real(8), parameter :: dt=5e-3               ! ���֥��ƥå״ֳ�        
    integer, parameter :: nt=40000, ndisp=2000  ! ������ʬ��, ɽ�����ƥå�

    integer :: i, j, it

    real(8) :: pi
    pi = atan(1.0D0)*4.0D0

    call et_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax)

 !-------- ��������� -----------
    ! ����ͤʤˤ�ʤ�
    yx_H = 0.0
    yx_U = 0.0
    yx_V = 0.0

    ! ����ͤ򼰤�Ϳ������
!    yx_h = sin(4*pi/(xmax-xmin)*yx_X)*cos(pi/(ymax-ymin)*yx_Y)
!    yx_u = sin(4*pi/(xmax-xmin)*yx_X)*cos(pi/(ymax-ymin)*yx_Y)
!    yx_h = exp( -(yx_X**2+yx_Y**2)/A0**2 )

    ! �ꥹ�����ȥե������ɤ߹��ߤξ��
!    call LoadVariable('shallow_eqbeta_init.nc@uvel,t=50', yx_data, im*(jm+1))
!    yx_u=transpose(yx_data) ; et_u = et_yx(yx_u)
!    call LoadVariable('shallow_eqbeta_init.nc@vvel,t=50', yx_data, im*(jm+1))
!    yx_v=transpose(yx_data) ; et_v = et_yx(yx_v)
!    call LoadVariable('shallow_eqbeta_init.nc@height,t=50', yx_data, im*(jm+1))
!    yx_h=transpose(yx_data) ; et_h = et_yx(yx_h)

 !----------------------- Ǯ����ʬ�� --------------------------
    do i=0,im-1
       do j=0,jm
          if ( abs(yx_X(j,i)) .le. Xrange ) then
             yx_QH(j,i) = Q0 * exp( -yx_Y(j,i)**2/A0 )&
                             * cos(pi*yx_X(j,i)/(2*Xrange))
          else
             yx_QH(j,i) = 0.0
          endif
       enddo
    enddo

    et_QH=et_yx(yx_QH)

    call output_gtool4_init
    call output_gtool4

 !-------- ������ʬ -----------
    do it=1,nt
       et_H = et_H &
              + dt*( -H0*(et_Dx_et(et_U) + et_Dy_et(et_V)) -et_QH )

       et_U = et_U &
              + dt* (   et_yx( Beta * yx_Y * (yx_et(et_V))) &
                      - Grav * et_Dx_et(et_H) )
       et_V = et_V &
              + dt* (  -et_yx( Beta * yx_Y * (yx_et(et_U))) &
                       - Grav * et_Dy_et(et_H) )

       call et_Boundaries(et_V)                          ! v=0

       if(mod(it,ndisp) .eq. 0)then                      ! ����
          call output_gtool4
       endif
    enddo

    call output_gtool4_close
    stop

  contains

   !----------------------- gtool4 ���� ----------------------------
    subroutine output_gtool4_init
      call HistoryCreate( &                             ! �ҥ��ȥ꡼����
           file='shallow_eqbeta_et.nc', &
           title='Equatorial beta-plane shallow water model ', &
           source='A sample program of ISPACK and gtool_history/gtool4', &
           institution='GFD_Dennou Club davis project',     &
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'m','m','s'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                                   ! �ѿ�����
      call HistoryAddattr('x','topology','circular')             ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)                ! ����°��
      call HistoryPut('y',y_Y)                                   ! �ѿ�����

      call HistoryAddVariable( &                                 ! �ѿ����
           varname='uvel', dims=(/'x','y','t'/), & 
           longname='X-velocity', units='m/s', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='vvel', dims=(/'x','y','t'/), & 
           longname='Y-velocity', units='m/s', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='height', dims=(/'x','y','t'/), & 
           longname='height', units='m', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='wvel', dims=(/'x','y','t'/), & 
           longname='Z-velocity', units='m/s', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='hdiv', dims=(/'x','y','t'/), & 
           longname='Horizontal divergence', units='1/s', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4
      write(6,*) 'it = ',it
      yx_U   = yx_et(et_U)
      yx_V   = yx_et(et_V)
      yx_H   = yx_et(et_H)

      call HistoryPut('t',real(it*dt))
      call HistoryPut('uvel',transpose(yx_U))
      call HistoryPut('vvel',transpose(yx_V))
      call HistoryPut('height',transpose(yx_H))
      call HistoryPut('wvel', &
           transpose(yx_et(-H0*(et_DX_et(et_U)+et_DY_et(et_V))-et_QH)))
      call HistoryPut('hdiv', &
           transpose(yx_et(et_DX_et(et_U)+et_DY_et(et_V))))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

    subroutine LoadVariable(var_uri, array, length)
        ! �ѿ���ؼ����� URI
        character(len = *), intent(in):: var_uri
        ! ����Ȥ���Ĺ��
        integer, intent(in):: length
        real(8), intent(out):: array(length)
        type(GT_VARIABLE):: var
    continue
        call open(var, var_uri)
        call get(var, array, length)
        call close(var)
      end subroutine LoadVariable

  end program equatorial_beta_shallow_water
