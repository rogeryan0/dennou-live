!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_module ����ץ�ץ���� : ��������(�¶���)
!
!      �Ȼ������� d zeta/dt = kappa d^2 zeta/dx^2
!
!����  2005/01/27  �ݹ�����
!      2008/12/05 ��������ʿ. ʸ���� sort ��Ĺ���� 2 ��
!
program at_diff_lanal

  use eigmatrix
  use lumatrix
  use at_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16               ! �ʻ���������
  integer, parameter :: km=16               ! �����ȿ�������
  integer, parameter :: nm=10               ! ���ϸ�ͭ�⡼�ɤ�����

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0   ! �ϰ�

 !---- �ѿ� ----
  real(8), dimension(0:im)  :: g_zeta
  real(8), dimension(0:km)  :: t_zeta

  real(8), dimension(0:km)  :: t_dzetadt

 !---- �����������ѿ��� ----
  real(8), dimension(im-1,im-1)  :: matrix   ! ��������

  real(8), dimension(nm)       :: eigen_r     ! ��ͭ�ͼ���
  real(8), dimension(nm)       :: eigen_i     ! ��ͭ�͵���
  real(8), dimension(im-1,nm)  :: eigvec_r    ! ��ͭ�٥��ȥ����
  real(8), dimension(im-1,nm)  :: eigvec_i    ! ��ͭ�٥��ȥ����

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter, dimension(2) :: zetabndry=(/0.0,0.0/)  ! ������
  real(8), parameter :: kappa=1.0               ! �Ȼ�����

  integer info, i, n

 !---------------- ��ɸ�ͤ����� ---------------------
  call at_initial(im,km,xmin,xmax)

 !---------------- ��������׻� ---------------------
  do i=1,im-1
     g_zeta    = 0.0
     g_zeta(i) = 1.0
     call boundaries
     t_zeta = t_g(g_zeta)
     t_dzetadt = kappa * t_dx_t(t_dx_t(t_zeta) )

     g_zeta    = g_t(t_dzetadt)
     matrix(:,i) = g_zeta(1:im-1)
  enddo

 !------------------ ��ͭ�ͷ׻� ---------------------
  call eigen(matrix,eigen_r,eigen_i,eigvec_r,eigvec_i,info,&
             sort=' R',reverse=.true.)

 !------------------- ���� ----------------------
  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4
  call output_gtool4_close                           ! �ҥ��ȥ꡼�����

  stop

  contains

    subroutine boundaries
      g_zeta(0) = zetabndry(1)
      g_zeta(im) = zetabndry(2)
    end subroutine boundaries

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='at_diff_eigen_grid.nc', title='Linear analysis fo diffusion equation (Grid base)', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x   ','mode'/), dimsizes=(/im+1,0/), &
           longnames=(/'X-coordinate','mode number '/),    &
           units=(/'1','1'/),                              &
           origin=1.0, interval=1.0 ) 

      call HistoryPut('x',g_x)                            ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='eigen_r', dims=(/'mode'/), & 
           longname='Eigen value (Real part)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='eigen_i', dims=(/'mode'/), & 
           longname='Eigen value (Imaginary part)', units='1', xtype='double')

      call HistoryAddVariable( &                          ! �ѿ����
           varname='eigvec_r', dims=(/'x   ','mode'/), & 
           longname='Eigen vector (R)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='eigvec_i', dims=(/'x   ','mode'/), & 
           longname='Eigen vector (I)', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      do n=1,nm
         call HistoryPut('mode',real(n))
         call HistoryPut('eigen_r',eigen_r(n))
         call HistoryPut('eigen_i',eigen_i(n))

         g_zeta(1:im-1) = eigvec_r(:,n)
         call boundaries
         call HistoryPut('eigvec_r',g_zeta)
         g_zeta(1:im-1) = eigvec_i(:,n)
         call boundaries
         call HistoryPut('eigvec_i',g_zeta)
      enddo
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program at_diff_lanal
