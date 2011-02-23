!----------------------------------------------------------------------
!     Copyright (c) 2003 Masatsugu Odaka. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/g4f90io and ISPACK  2004/01/26 M. Odaka
! 
! Solving a non-linear 2-D shallow water system on a sphere. 
!     
!     du/dt - 2\Omega\sin\phi v = -g/a\cos\phi dh/d\lambda - \nu\lapla^4 u,
!     dv/dt + 2\Omega\sin\phi u = -g/a  dh/d\phi - \nu\lapla^4 v,
!     dH/dt + ( 1/\cos\phi d([H-hs] u)/d\lambda 
!               + 1/\cos\phi d([H-hs] v\cos\phi)/d\phi ) = - \nu\lapla^4 h.
!
! where hs is hight of topography.
!
! The time integration is performed by using leap-frog scheme.
! 

  program shallow_uvh

    use w_module
    use gt4_history
    implicit none

  !---- ���ֲ��������� ----
!!$    integer, parameter :: im=512, jm=256            ! �ʻ���������(X,Y)
!!$    integer, parameter :: nm=170
!!$    integer, parameter :: im=128, jm=64             ! �ʻ���������(X,Y)
!!$    integer, parameter :: nm=42
!!$    integer, parameter :: im=96, jm=48              ! �ʻ���������(X,Y)
!!$    integer, parameter :: nm=31
    integer, parameter :: im=64, jm=32             ! �ʻ���������(X,Y)
    integer, parameter :: nm=21

  !---- �ѿ� ----
    real(8)            :: xy_U(im,jm)              ! �ʻ����ǡ���(®�ٷ�����ʬ)
    real(8)            :: xy_V(im,jm)              ! �ʻ����ǡ���(®�ٰ�����ʬ)
    real(8)            :: xy_H(im,jm)              ! �ʻ����ǡ���(�Ѱ�)
    real(8)            :: xy_Zeta(im,jm)           ! �ʻ����ǡ���(����)
    real(8)            :: xy_D(im,jm)              ! �ʻ����ǡ���(ȯ��)
    real(8)            :: xy_Coli(im,jm)           ! �ʻ����ǡ���(���ꥪ��)
    real(8)            :: xy_Topo(im,jm)           ! �ʻ����ǡ���(�Ϸ�)

    real(8)            :: w_U_A((nm+1)*(nm+1))     ! ���ڥ��ȥ�(®�ٷ�����ʬ t+��t)
    real(8)            :: w_V_A((nm+1)*(nm+1))     ! ���ڥ��ȥ�(®�ٰ�����ʬ t+��t)
    real(8)            :: w_H_A((nm+1)*(nm+1))     ! ���ڥ��ȥ�(�Ѱ� t+��t)

    real(8)            :: w_U((nm+1)*(nm+1))       ! ���ڥ��ȥ�(®�ٷ�����ʬ)
    real(8)            :: w_V((nm+1)*(nm+1))       ! ���ڥ��ȥ�(®�ٰ�����ʬ)
    real(8)            :: w_H((nm+1)*(nm+1))       ! ���ڥ��ȥ�(�Ѱ� t)

    real(8)            :: w_U_B((nm+1)*(nm+1))     ! ���ڥ��ȥ�(®�ٷ�����ʬ t+��t)
    real(8)            :: w_V_B((nm+1)*(nm+1))     ! ���ڥ��ȥ�(®�ٰ�����ʬ t+��t)
    real(8)            :: w_H_B((nm+1)*(nm+1))     ! ���ڥ��ȥ�(�Ѱ� t-��t)


  !---- ������ʬ�ѥ�᥿�� ----
    real(8)            :: t                        ! ����
 
  !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: pi = 3.141592653589793   ! �߼�Ψ
    real(8)            :: R0                       ! ���Ⱦ��
    real(8)            :: Omega                    ! ��ž��®��
    real(8)            :: Grav                     ! ���ϲ�®��
    real(8)            :: H0                       ! ʿ���Ѱ�
    namelist /nmpcon/ R0, Omega, Grav, H0

  !----- ����ѿ� ----
    integer            :: it=0
    integer            :: nt                       ! ������ʬ���ƥå׿�

  !----- NAMELIST �ѿ� ----
    character(len=100) :: run=''                   ! �¸�̾
    character(len=100) :: sign=''                  ! �¹Լ�̾
    namelist /nmrun/  run, sign
    !----------------------
    real(8)            :: delt                     ! ���֥��ƥå�
    real(8)            :: tfil                     ! ���֥ե��륿������
    namelist /nmintg/ delt, tfil
    !----------------------
    integer            :: endrst                   ! ���ϲ��
    integer            :: intrst                   ! ���ϥ��ƥå״ֳ�
    namelist /nmtime/ intrst, endrst
    !----------------------
    character(len=100) :: input_file=''            ! ���ϥե�����̾  
    real(8)            :: tinit                    ! ��ʬ���ϻ���
    namelist /nminit/ input_file, tinit
    !----------------------
    character(len=100) :: output_file=''           ! ���ϥե�����̾  
    namelist /nmrstr/ output_file
    !----------------------
    character(len=100) :: model_name=''            ! ��ǥ�̾
    character(len=10)  :: model=''                 ! ��ǥ�����̾
    namelist /nmodel/ model_name, model 

  !------------------- ��ɸ�ͤ����� --------------------
    write(*,*) "call w_initial"
    call w_initial(nm,im,jm)                       ! ISPACK�����

  !------------------- NAMELIST �ɤ߹��� ---------------
    read(5,nml=nmrun)  ; write(6,nml=nmrun)     
    read(5,nml=nmintg) ; write(6,nml=nmintg)
    read(5,nml=nmtime) ; write(6,nml=nmtime)
    read(5,nml=nminit) ; write(6,nml=nminit)
    read(5,nml=nmrstr) ; write(6,nml=nmrstr)
    read(5,nml=nmodel) ; write(6,nml=nmodel)
    read(5,nml=nmpcon) ; write(6,nml=nmpcon)

  !------------------- ��������� ----------------------

    xy_Coli = 2 * Omega * sin(xy_Lat) 

    call input_gtool4(input_file, tinit, &
&                         xy_U, xy_V, xy_H, xy_Zeta, xy_D, xy_Topo)

    w_U = w_xy(xy_U) ; w_U_A = w_U ; w_U_B = w_U
    w_V = w_xy(xy_V) ; w_V_A = w_V ; w_V_B = w_V
    w_H = w_xy(xy_H) ; w_H_A = w_H ; w_H_B = w_H

    call output_gtool4_init                        ! �ҥ��ȥ꡼�����
    call output_gtool4                             ! ����ͽ���

  !------------------- ������ʬ ------------------------
    nt = intrst * endrst

    do it=1,nt
      t = tinit + it*delt

      w_U_A = w_U_B + delt * min(2,it) * w_xy(               & 
&               - xy_U * xy_GradLon_w(w_U) / R0              &  
&               - xy_V * xy_GradLat_w(w_U) / R0              &
&               + (xy_Coli + xy_U * tan(xy_Lat) / R0) * xy_V &
&               - xy_GradLon_w(Grav * w_H) / R0)

      w_V_A = w_V_B + delt * min(2,it) * w_xy(               & 
&               - xy_U * xy_GradLon_w(w_V) / R0              &  
&               - xy_V * xy_GradLat_w(w_V) / R0              &
&               - (xy_Coli + xy_U * tan(xy_Lat) / R0) * xy_U &
&               - xy_GradLat_w(Grav*w_H) / R0)

      w_H_A = w_H_B + delt * min(2,it) * (                   &
&               - w_Div_xy_xy((xy_H - xy_Topo) * xy_U,       &
&                             (xy_H - xy_Topo) * xy_V) / R0)
                                                                     
  !------------------- ���֥ե��륿�� ------------------

      call tfilt(w_U_A, w_U, w_U_B)              ! ���֥ե��륿��
      call tfilt(w_V_A, w_V, w_V_B)              ! ���֥ե��륿��
      call tfilt(w_H_A, w_H, w_H_B)              ! ���֥ե��륿��

      xy_U = xy_w(w_U) ; xy_V = xy_w(w_V) ; xy_H = xy_w(w_H)

      if(mod(it,intrst) == 0)then                ! ����
        call output_gtool4
      endif
    enddo

    call output_gtool4_close
    stop

  contains
 
  !------------------- ���֥ե��륿�� ------------------
    subroutine tfilt(w_data_A, w_data, w_data_B)

      real(8),intent(in)    :: w_data_A((nm+1)*(nm+1))
      real(8),intent(inout) :: w_data((nm+1)*(nm+1))
      real(8),intent(inout) :: w_data_B((nm+1)*(nm+1))

      w_data_B = (1.0 - 2.0 * tfil) * w_data + tfil * (w_data_B + w_data_A)
      w_data = w_data_A

    end subroutine tfilt
  !------------------- ������ ----------------------
    subroutine input_gtool4( filename, time, xy_U, xy_V, xy_H, &
&                                            xy_Zeta, xy_D, xy_Topo  )
      character(len=*), intent(in) :: filename    ! ����ͥե�����̾
      real(8),intent(in)           :: time        ! ��ʬ���ϻ���
      real(8),intent(out)          :: xy_U(im,jm) ! �ʻ����ǡ���(®�ٷ�����ʬ)
      real(8),intent(out)          :: xy_V(im,jm) ! �ʻ����ǡ���(®�ٰ�����ʬ)
      real(8),intent(out)          :: xy_H(im,jm) ! �ʻ����ǡ���(�Ѱ�)
      real(8),intent(out)          :: xy_Zeta(im,jm) ! �ʻ����ǡ���(����)
      real(8),intent(out)          :: xy_D(im,jm) ! �ʻ����ǡ���(ȯ��)
      real(8),intent(out)          :: xy_Topo(im,jm)! �ʻ����ǡ���(�Ϸ�)

      character(len=100) :: varname, ctime
      write(ctime,*) time

      varname = trim(filename)//'@topo'
      call LoadVariable(varname, xy_Topo, im*jm)

      varname = trim(filename)//'@u,t='//trim(adjustl(ctime))
      call LoadVariable(varname, xy_U, im*jm)

      varname = trim(filename)//'@v,t='//trim(adjustl(ctime))
      call LoadVariable(varname, xy_V, im*jm)

      varname = trim(filename)//'@h,t='//trim(adjustl(ctime))
      call LoadVariable(varname, xy_H, im*jm)

      varname = trim(filename)//'@zeta,t='//trim(adjustl(ctime))
      call LoadVariable(varname, xy_Zeta, im*jm)

      varname = trim(filename)//'@div,t='//trim(adjustl(ctime))
      call LoadVariable(varname, xy_D, im*jm)

    end subroutine input_gtool4

    subroutine LoadVariable(var_uri, array, length)
      use gt4f90io
      character(len = *), intent(in):: var_uri         ! �ѿ�̾ URI
      integer, intent(in)           :: length          ! ����Ĺ��
      real(8), intent(out)          :: array(length)   ! �ѿ�����̾
      type(GT_VARIABLE)             :: var
      
      call open(var, var_uri)
      call get(var, array, length)
      call close(var)
    end subroutine LoadVariable

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file=trim(output_file),                                & 
&          title=trim(run),                                       &
&          source=trim(model_name),                               &
&          institution=trim(sign),                                &
&          dims=(/'lon','lat','t  '/), dimsizes=(/im,jm,0/),      &
&          longnames=(/'longitude','latitude ','time     '/),     &
&          units=(/'deg.','deg.','sec.'/),                        &
&          origin=real(tinit), interval=real(intrst*delt) )

      call HistoryPut('lon',x_Lon*180/pi)              ! �ѿ�����
      call HistoryPut('lat',y_Lat*180/pi)              ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
&           varname='topo', dims=(/'lon','lat'/), & 
&           longname='Topography', units='m', xtype='double')

      call HistoryPut('topo',xy_Topo)

      call HistoryAddVariable( &                          ! �ѿ����
&          varname='h', dims=(/'lon','lat','t  '/), & 
&          longname='surface displacement ', units='m', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
&          varname='u', dims=(/'lon','lat','t  '/), & 
&          longname='velocity(longitude) ', units='m/s', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
&          varname='v', dims=(/'lon','lat','t  '/), & 
&          longname='velocity(latitude) ', units='m/s', xtype='double')

      call HistoryAddVariable( &                          ! �ѿ����
&          varname='zeta', dims=(/'lon','lat','t  '/), & 
&          longname='vorticity', units='1/s', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
&          varname='div', dims=(/'lon','lat','t  '/), & 
&          longname='divergence', units='1/s', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4

      xy_Zeta = xy_w(w_DivLon_xy(xy_V) - w_DivLat_xy(xy_U)) / R0
      xy_D    = xy_w(w_Div_xy_xy(xy_U, xy_V) / R0)

      call HistoryPut('t',real(tinit+it*delt))
      call HistoryPut('u',xy_U)
      call HistoryPut('v',xy_V)
      call HistoryPut('h',xy_H)
      call HistoryPut('zeta',xy_Zeta)
      call HistoryPut('div',xy_D)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program shallow_uvh

