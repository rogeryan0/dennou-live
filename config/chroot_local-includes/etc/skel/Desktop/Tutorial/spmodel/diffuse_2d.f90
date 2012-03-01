! Sample program for gtool_history/gtool4 and spmodel   2009/02/07 S.Takehiro
!                                                       2010/02/17 S.Takehiro
! 
! Solving 2-D non-divergent (barotropic) fluid system
!     d\zeta/dt  = \nu\nabla^2\zeta
!
program diffuse_2d

  use ee_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10            ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Zeta(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ee_Zeta(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin=0.0, xmax=1.0
  real(8), parameter :: ymin=0.0, ymax=1.0

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=1000, ndisp=20       ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: nu=1.0                  ! Ǵ������
  real(8), parameter :: x1=(xmin+xmax)/2.0      ! ���ʬ�� X ��ɸ
  real(8), parameter :: y1=(xmin+xmax)/2.0      ! ���ʬ�� Y ��ɸ
  real(8), parameter :: sigma=0.1               ! ���ʬ�ۤ��礭��

  integer ::  it=0                              ! DO �ѿ�


 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  yx_Zeta=  exp(-((yx_X-x1)**2+(yx_Y-y1)**2)/(2*sigma**2))
  ee_Zeta = ee_yx(yx_Zeta)

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt
     ee_Zeta = ee_Zeta + dt * nu * ee_Lapla_ee(ee_Zeta)   ! Euler ˡ�ˤ�������ʬ

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        yx_Zeta = yx_ee(ee_Zeta)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

contains

  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='diffuse_2d.nc', title='2D diffusion model',   &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis project',     &
           dims=(/'x','y','t'/), dimsizes=(/im,jm,0/),      &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('x',x_X)                                  ! �ѿ�����
    call HistoryPut('y',y_Y)                                  ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='vorticity', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('zeta',transpose(yx_Zeta))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program diffuse_2d
