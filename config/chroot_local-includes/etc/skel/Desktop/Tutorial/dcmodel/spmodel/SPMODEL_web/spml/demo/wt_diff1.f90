!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_module
!      �����β��ٳȻ�����
!
!����  2002/02/04  �ݹ�����
!      2002/04/11  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/08/24  �ݹ�����  �⥸�塼��̾�ѹ�
!      2004/02/15  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!
program wt_diff1

  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(im,jm,0:km)     :: xyz_Temp   ! ����
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp    ! ����

  real(8), dimension(im,jm,0:km)     :: xyz_Q      ! Ǯ��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Q       ! Ǯ��

  real(8)             :: xy_Tempbndry(im,jm,2)     ! ������
  real(8)             :: w_Tempbndry((nm+1)**2,2)  ! ������

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-5                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=10000, ndisp=1000    ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: TempBC='DN'    ! ���ٶ������(DD/DN/ND/NN)
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
  xyz_Temp =  exp(-((xyz_Lon-Lon1)**2+(xyz_Lat-Lat1)**2+(xyz_Rad-Rad1)**2)&
                 /(2*Sigma**2))
  wt_Temp = wt_xyz(xyz_Temp)

  xyz_Q = 1.0
  wt_Q = wt_xyz(xyz_Q)

 !------------------- ���������� ----------------------
  xy_Tempbndry(:,:,1) = 0
  xy_Tempbndry(:,:,2) = 0

  w_Tempbndry(:,1) = w_xy(xy_Tempbndry(:,:,1))
  w_Tempbndry(:,2) = w_xy(xy_Tempbndry(:,:,2))

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     wt_Temp = wt_Temp + dt *( nu * wt_Lapla_wt(wt_Temp) + wt_Q )
     call wt_Boundaries(wt_Temp,w_Tempbndry,cond=TempBC)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        xyz_Temp = xyz_wt(wt_Temp)
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wt_diff1.nc', title='Diffusion model in a spherical shell', &
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
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',xyz_Temp)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_diff1
