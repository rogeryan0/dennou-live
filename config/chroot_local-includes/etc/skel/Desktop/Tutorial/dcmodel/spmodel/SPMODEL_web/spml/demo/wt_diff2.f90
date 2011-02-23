!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_module
!      ������®�٥ȥ�����Ȼ�����
!
!����  2002/02/04  �ݹ�����
!      2002/04/11  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/08/24  �ݹ�����  �⥸�塼��̾�ѹ�
!      2004/02/15  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2005/06/03  ������ ��ʿ it=0 ������Ū�����
!
program wt_diff2

  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(im,jm,0:km)     :: xyz_Torvel   ! �ȥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Torvel    ! �ȥ�����®��

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-5                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=10000, ndisp=1000    ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: TvBC='FF'      ! ®�ٶ������(FF/FR/RF/RR)
  real(8), parameter :: nu=1.0                  ! Ǵ������

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: Lon1=pi                 ! ���ʬ�۰��ٺ�ɸ
  real(8), parameter :: Lat1=0.0                ! ���ʬ�۷��ٺ�ɸ
  real(8), parameter :: Rad1=(ri+ro)/2.0        ! ���ʬ��ư�º�ɸ
  real(8), parameter :: Sigma=0.2               ! ���ʬ�ۤ��礭��

 !---- ����¾ ----
  integer :: it=0

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !------------------- ��������� ----------------------
  xyz_Torvel =  exp(-((xyz_Lon-Lon1)**2+(xyz_Lat-Lat1)**2+(xyz_Rad-Rad1)**2)&
                 /(2*Sigma**2))
  wt_Torvel = wt_xyz(xyz_Torvel)

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     wt_Torvel = wt_Torvel + dt *( nu * wt_Lapla_wt(wt_Torvel))

     call wt_TorBoundaries(wt_Torvel,cond=TvBC)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        xyz_Torvel = xyz_wt(wt_Torvel)
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wt_diff2.nc', title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='torvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='toriodal velocity potential', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('torvel',xyz_Torvel)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_diff2
