!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_module
!      ����ȥ�����ݥƥ󥷥��Ȼ�����
!
!����  2002/08/09  �ݹ�����
!      2004/02/15  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2005/06/03  ������ ��ʿ it=0 ������Ū�����
!
program wt_diff4

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  !real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��
  real(8),parameter  :: ri=0.1, ro=1.0       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(im,jm,0:km)     :: xyz_TMAG   ! �ݥ����뼧��
  real(8), dimension((nm+1)**2,0:lm) :: wt_TMAG    ! �ݥ����뼧��

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=20000, ndisp=1000    ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: eta=1.0                 ! ����Ǵ������

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: lon1=pi                 ! ���ʬ�۰��ٺ�ɸ
  real(8), parameter :: lat1=0.0                ! ���ʬ�۷��ٺ�ɸ
  real(8), parameter :: rad1=(ri+ro)/2.0        ! ���ʬ��ư�º�ɸ
  real(8), parameter :: sigma=0.2               ! ���ʬ�ۤ��礭��

 !---- ����¾ ----
  integer :: it=0

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !------------------- ��������� ----------------------
  !xyz_TMAG =  exp(-((xyz_LON-lon1)**2+(xyz_LAT-lat1)**2+(xyz_RAD-rad1)**2)&
  !               /(2*sigma**2))
  xyz_TMAG = sin(xyz_LAT) * sin( pi*(xyz_RAD-ri)/(ro-ri) )

  wt_TMAG = wt_xyz(xyz_TMAG)

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     wt_TMAG = wt_TMAG + dt *( eta * wt_Lapla_wt(wt_TMAG) )
     call wt_TormagBoundaries(wt_TMAG)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        xyz_TMAG = xyz_wt(wt_TMAG)
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- ���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wt_diff4.nc', title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_LON/pi*180)                       ! �ѿ�����
    call HistoryPut('lat',y_LAT/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_RAD)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='tmag', dims=(/'lon','lat','rad','t  '/), & 
           longname='magnetic toroidal potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='Em', dims=(/'t  '/), & 
           longname='squared toroidal potential', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('tmag',xyz_TMAG)
    call HistoryPut('Em',IntLonLatRad_xyz(xyz_TMAG**2/2))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_diff4
