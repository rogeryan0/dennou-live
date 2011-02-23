!--
!----------------------------------------------------------------------
! Copyright(c) 2009-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_galerkin_module_sjpack
!
!      3 ��������ΰ�
!      ����Ĵ��ȡ��Ÿ�� + �����ӥ����աݥ��顼����ˡ(sjpack ��)
!
!      spml/wt_galerkin_module �⥸�塼��ϵ��̾太��ӵ����Ǥ�ή�α�ư��
!      ���ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤���
!      ��ΤǤ���. 
!
!      ��ʿ�����˵���Ĵ��ȡ���Ѵ�����Ӿ岼�˥����ӥ����ե��顼����ˡ
!      ���Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
!
!      ������ wt_module_sjpack ����Ӿ岼��������򤯤����
!      �����ӥ����ա����顼����ˡ�Υ⥸�塼�뷲
!      (at_ab_galerkin_ND, at_ad_galerkin_DD, at_af_galerkin_MM,
!       at_ah_galerkin_MMex, at_ap_galerkin_DN, at_aq_galerkin_RRFF,
!       at_av_galerkin_NN) ���Ѥ��Ƥ���.
!
!      �ǲ����Ǥϵ���Ĵ���Ѵ�����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� 
!      ISPACK �� Fortran77 ���֥롼����(sjpack, ftack)���Ѥ��Ƥ���.
!
!      �ؿ�, ���֥롼�����̾���ȵ�ǽ�� wt_galerkin_module �Τ�Τ�Ʊ���Ǥ���. 
!      �������ä� use ʸ�� wt_galerkin_module ���� wt_galerkin_module_sjpack ��
!      �ѹ���������� SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
! 
!      ������ l_nm, nm_l �λȤ����ˤ���դ��줿��. wt_galerkin_module ��
!      l_nm, nm_l �� wt_galerkin_Initial �ǽ�������ʤ��Ȥ��Ѥ��뤳�Ȥ�
!      �Ǥ���(��̤������ȿ��˰ͤ�ʤ�)��, wt_galerkin_module_sjpack ��
!      ��ΤϽ���������Τ��ˤ����Ȥ����Ȥ��Ǥ��ʤ�. 
!
!
!����  2009/09/18  �ݹ� ���� wt_galerkin_module ��� sjpack �Ѥ˲�¤
!      2009/09/20  �ݹ�����  ���Ѿ����դ��ɲ�
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!++
module wt_galerkin_module_sjpack
  !
  != wt_galerkin_module_sjpack
  !
  ! Authors:: Youhei SASAKI, Shin-Ichi Takehiro
  ! Version:: $Id: wt_galerkin_module_sjpack.f90,v 1.2 2010-03-02 10:09:07 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  !      3 ��������ΰ�
  !      ����Ĵ��ȡ��Ÿ�� + �����ӥ����աݥ��顼����ˡ(sjpack ��)
  !
  !      spml/wt_galerkin_module �⥸�塼��ϵ��̾太��ӵ����Ǥ�ή�α�ư��
  !      ���ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤���
  !      ��ΤǤ���. 
  !
  !      ��ʿ�����˵���Ĵ��ȡ���Ѵ�����Ӿ岼�˥����ӥ����ե��顼����ˡ
  !      ���Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
  !
  !      ������ wt_module_sjpack ����Ӿ岼��������򤯤����
  !      �����ӥ����ա����顼����ˡ�Υ⥸�塼�뷲
  !      (at_ab_galerkin_ND, at_ad_galerkin_DD, at_af_galerkin_MM,
  !       at_ah_galerkin_MMex, at_ap_galerkin_DN, at_aq_galerkin_RRFF,
  !       at_av_galerkin_NN) ���Ѥ��Ƥ���.
  !
  !      �ǲ����Ǥϵ���Ĵ���Ѵ�����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� 
  !      ISPACK �� Fortran77 ���֥롼����(sjpack, ftack)���Ѥ��Ƥ���.
  !
  !      �ؿ�, ���֥롼�����̾���ȵ�ǽ�� wt_galerkin_module �Τ�Τ�Ʊ���Ǥ���. 
  !      �������ä� use ʸ�� wt_galerkin_module ���� wt_galerkin_module_sjpack ��
  !      �ѹ���������� SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
  ! 
  !      ������ l_nm, nm_l �λȤ����ˤ���դ��줿��. wt_galerkin_module ��
  !      l_nm, nm_l �� wt_galerkin_Initial �ǽ�������ʤ��Ȥ��Ѥ��뤳�Ȥ�
  !      �Ǥ���(��̤������ȿ��˰ͤ�ʤ�)��, wt_galerkin_module_sjpack ��
  !      ��ΤϽ���������Τ��ˤ����Ȥ����Ȥ��Ǥ��ʤ�. 
  !
  use dc_message
  use lumatrix
  use wt_module_sjpack
  use at_ad_galerkin_DD, only: at_ad_galerkin_DD_Initial,      &
                               wd_wt => ad_at, wt_wd => at_ad, &
                               wd_wg => ad_ag, wg_wd => ag_ad, &
                               wd_DRad_wd => ad_Dx_ad,         &
                               d_t, t_d, d_g, g_d,             &
                               d_DRad_d => d_Dx_d
  use at_av_galerkin_NN, only: at_av_galerkin_NN_Initial,      &
                               wv_wt => av_at, wt_wv => at_av, &
                               wv_wg => av_ag, wg_wv => ag_av, &
                               wv_DRad_wv => av_Dx_av,         &
                               v_t, t_v, v_g, g_v,             &
                               v_DRad_v => v_Dx_V
  use at_ab_galerkin_ND, only: at_ab_galerkin_ND_Initial,      &
                               wb_wt => ab_at, wt_wb => at_ab, &
                               wb_wg => ab_ag, wg_wb => ag_ab, &
                               wb_DRad_wb => ab_Dx_ab,         &
                               b_t, t_b, b_g, g_b,             &
                               b_DRad_b => b_Dx_b
  use at_ap_galerkin_DN, only: at_ap_galerkin_DN_Initial,      &
                               wp_wt => ap_at, wt_wp => at_ap, &
                               wp_wg => ap_ag, wg_wp => ag_ap, &
                               wp_DRad_wp => ap_Dx_ap,         &
                               p_t, t_p, p_g, g_p,             &
                               p_DRad_p => p_Dx_p
  use at_af_galerkin_MM, only: at_af_galerkin_MM_Initial,      &
                               wf_wt => af_at, wt_wf => at_af, &
                               wf_wg => af_ag, wg_wf => ag_af, &
                               wf_DRad_wf => af_Dx_af,         &
                               f_t, t_f, f_g, g_f,             &
                               f_DRad_f => f_Dx_f
  use at_ah_galerkin_MMex, only: at_ah_galerkin_MMex_Initial,    &
                                 wh_wt => ah_at, wt_wh => at_ah, &
                                 wh_wg => ah_ag, wg_wh => ag_ah, &
                                 wh_DRad_wh => ah_Dx_ah
  use at_aq_galerkin_RRFF, only: at_aq_galerkin_RRFF_Initial,    &
                                 wq_wt => aq_at, wt_wq => at_aq, &
                                 wq_wg => aq_ag, wg_wq => ag_aq, &
                                 wq_DRad_wq => aq_Dx_aq,         &
                                 q_t, t_q, q_g, g_q,             &
                                 q_DRad_q => q_Dx_q,             &
                                 TQ, QT, alpha, beta

  implicit none
  private

  public wt_galerkin_Initial       ! �����
  public wq_LaplaPol2Pol_wt        ! �ݥ�����®�پ�ݥƥ󥷥�� �� <= ��^2��

  !-- wt_module 
  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public z_Rad, z_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xyz_Lon, xyz_Lat, xyz_Rad
  public wz_Rad
  public wt_VMiss

  public w_xy, xy_w
  public at_Dr_at, t_Dr_t, az_at, at_az
  public xyz_wt, wt_xyz, xyz_wz, wz_xyz, wz_wt, wt_wz
  public wt_DRad_wt, wt_DivRad_wt, wt_RotRad_wt, wt_Lapla_wt
  public xyz_GradLon_wt, xyz_gradlat_wt
  public wt_DivLon_xyz, wt_DivLat_xyz
  public wt_Div_xyz_xyz_xyz, xyz_Div_xyz_xyz_xyz
  public xyz_RotLon_wt_wt, xyz_RotLat_wt_wt, wt_RotRad_xyz_xyz

  public yz_IntLon_xyz, xz_IntLat_xyz, xy_IntRad_xyz
  public x_IntLatRad_xyz, y_IntLonRad_xyz, z_IntLonLat_xyz
  public IntLonLatRad_xyz

  public x_IntLat_xy, y_IntLon_xy, IntLonLat_xy
  public z_IntLat_yz, y_IntRad_yz, IntLatRad_yz
  public z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
  public IntLon_x, IntLat_y, IntRad_z

  public yz_AvrLon_xyz, xz_AvrLat_xyz, xy_AvrRad_xyz
  public x_AvrLatRad_xyz, y_AvrLonRad_xyz, z_AvrLonLat_xyz
  public AvrLonLatRad_xyz

  public x_AvrLat_xy, y_AvrLon_xy, AvrLonLat_xy
  public z_AvrLat_yz, y_AvrRad_yz, AvrLatRad_yz
  public z_AvrLon_xz, x_AvrRad_xz, AvrLonRad_xz
  public AvrLon_x, AvrLat_y, AvrRad_z

  public wt_KxRGrad_wt, xyz_KGrad_wt, wt_L2_wt, wt_L2Inv_wt, wt_QOperator_wt
  public wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz
  public wt_Potential2vector, wt_Potential2Rotation
  public wt_VGradV

  public nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt
  public nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt

  public wt_Boundaries, wt_TorBoundaries, wz_LaplaPol2Pol_wz
  public wt_TormagBoundaries, wt_PolmagBoundaries

  public wt_BoundariesTau, wt_TorBoundariesTau
  public wt_TormagBoundariesTau, wt_PolmagBoundariesTau

  public wt_BoundariesGrid, wt_TorBoundariesGrid, wt_LaplaPol2PolGrid_wt
  public wt_TormagBoundariesGrid, wt_PolmagBoundariesGrid

  !-- at_ad_galerkin_DD
  public :: wd_wt, d_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: wd_wg, d_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: wt_wd, t_d                 ! ���顼����->�����ӥ������Ѵ�
  public :: wg_wd, g_d                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: wd_DRad_wd, d_DRad_d       ! ư����ʬ

  !-- at_av_galerkin_NN
  public :: wv_wt, v_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: wv_wg, v_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: wt_wv, t_v                 ! ���顼����->�����ӥ������Ѵ�
  public :: wg_wv, g_v                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: wv_DRad_wv, v_DRad_v       ! ư����ʬ

  !-- at_ab_galerkin_ND
  public :: wb_wt, b_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: wb_wg, b_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: wt_wb, t_b                 ! ���顼����->�����ӥ������Ѵ�
  public :: wg_wb, g_b                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: wb_DRad_wb, b_DRad_b       ! ư����ʬ

  !-- at_ap_galerkin_DN
  public :: wp_wt, p_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: wp_wg, p_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: wt_wp, t_p                 ! ���顼����->�����ӥ������Ѵ�
  public :: wg_wp, g_p                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: wp_DRad_wp, p_DRad_p       ! ư����ʬ

  !-- at_af_galerkin_MM
  public :: wf_wt, f_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: wf_wg, f_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: wt_wf, t_f                 ! ���顼����->�����ӥ������Ѵ�
  public :: wg_wf, g_f                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: wf_DRad_wf, f_DRad_f       ! ư����ʬ

  !-- at_ab_galerkin_MMex
  public :: wh_wt                      ! �����ӥ�����->���顼�����Ѵ�
  public :: wh_wg                      ! �ʻ���->���顼����ǡ����Ѵ�
  public :: wt_wh                      ! ���顼����->�����ӥ������Ѵ�
  public :: wg_wh                      ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: wh_DRad_wh                 ! ư����ʬ

  !-- at_aq_galerkin_RRFF
  public :: wq_wt, q_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: wq_wg, q_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: wt_wq, t_q                 ! ���顼����->�����ӥ������Ѵ�
  public :: wg_wq, g_q                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: wq_DRad_wq, q_DRad_q       ! ư����ʬ


  !-- �����ѿ�--
  integer :: im=64, jm=32, km=16       ! �ʻ���������(����, ����, ư��)
  integer :: nm=21, lm=16              ! �����ȿ�������(��ʿ, ư��)

  logical :: Set_DD    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_NN    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_DN    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_ND    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_MM    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_MMex  =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_RRFF  =.false.        ! �⥸�塼���ɤ߹��ߥ����å�

  save im, jm, km, nm, lm, &
       Set_DD, Set_NN, Set_DN, Set_ND, Set_MM, Set_MMex, Set_RRFF

  contains
  !--------------- ����� -----------------
    subroutine wt_galerkin_Initial(i,j,k,n,l,r_in,r_out,&
         DD,NN,DN,ND,velBC,&
         MM_cfdx0_ri, MM_cfdx1_ri, MM_cfdx0_ro, MM_cfdx1_ro,&
         MMex_cfdx0_ri, MMex_cfdx1_ri, MMex_cfdx0_ro, MMex_cfdx1_ro, np)
      !
      !  3 ��������ΰ�
      !  ����Ĵ��ȡ��Ÿ�� + �����ӥ����աݥ��顼����ˡ
      !
      !  ��������֥롼����
      !
      integer,intent(in) :: i, j, k        ! �ʻ���������(����, ����, ư��)
      integer,intent(in) :: n, l           ! �����ȿ�������(��ʿ, ư��)
      integer,intent(in),optional :: np    ! OPENMP �Ǥκ��祹��åɿ�

      real(8),intent(in) :: r_in, r_out    ! ����⳰Ⱦ��

      logical,intent(in),optional :: DD  ! �⥸�塼���ɤ߹��ߥ����å�
      logical,intent(in),optional :: NN  ! �⥸�塼���ɤ߹��ߥ����å�
      logical,intent(in),optional :: DN  ! �⥸�塼���ɤ߹��ߥ����å�
      logical,intent(in),optional :: ND  ! �⥸�塼���ɤ߹��ߥ����å�

      ! at_af_galerkin_MM_module ������ﷸ��
      real(8),intent(in),optional :: MM_cfdx0_ri ! (0����ʬ@x=xmin)
      real(8),intent(In),optional :: MM_cfdx1_ri ! (1����ʬ@x=xmin)
      real(8),intent(in),optional :: MM_cfdx0_ro ! (0����ʬ@x=xmax)
      real(8),intent(in),optional :: MM_cfdx1_ro ! (1����ʬ@x=xmax)

      ! at_ah_galerkin_MMex_module ������ﷸ��
      real(8),intent(in),optional :: MMex_cfdx0_ri((n+1)*(n+1)) ! (0����ʬ@r=ri)
      real(8),intent(in),optional :: MMex_cfdx1_ri((n+1)*(n+1)) ! (1����ʬ@r=ri)
      real(8),intent(in),optional :: MMex_cfdx0_ro((n+1)*(n+1)) ! (0����ʬ@r=ro)
      real(8),intent(in),optional :: MMex_cfdx1_ro((n+1)*(n+1)) ! (1����ʬ@r=ro)

      ! at_aq_galerkin_RRFF_module ®�پ춭�����
      character(LEN=2),intent(in),optional :: velBC  ! �������(RR/FF/RF/FR)

      !--------------- �������� -----------------
      im=i ; jm = j ; km=k ; nm=n ; lm=l

      if ( present(DD) ) Set_DD = DD
      if ( present(NN) ) Set_NN = NN
      if ( present(DN) ) Set_DN = DN
      if ( present(ND) ) Set_ND = ND

      if ( present(MM_cfdx0_ri) .AND.present(MM_cfdx1_ri) .AND.&
           present(MM_cfdx0_ro) .AND.present(MM_cfdx1_ro) ) &
           Set_MM=.true.

      if ( present(MMex_cfdx0_ri) .AND.present(MMex_cfdx1_ri) .AND.&
           present(MMex_cfdx0_ro) .AND.present(MMex_cfdx1_ro) ) &
           Set_MMex=.true.

      if ( present(velBC) ) Set_RRFF = .true.

      !--------------- �⥸�塼������ -----------------
      if ( present(np) ) then
        call wt_Initial(i,j,k,n,l,r_in,r_out, np)
      else
        call wt_Initial(i,j,k,n,l,r_in,r_out)
      endif

      if ( Set_DD ) call at_ad_galerkin_DD_Initial(km,lm)
      if ( Set_NN ) call at_av_galerkin_NN_Initial(km,lm)
      if ( Set_DN ) call at_ap_galerkin_DN_Initial(km,lm)
      if ( Set_ND ) call at_ab_galerkin_ND_Initial(km,lm)
      if ( Set_MM ) call at_af_galerkin_MM_Initial      &
                               (km,lm, MM_cfdx0_ro, MM_cfdx1_ro,&
                                       MM_cfdx0_ri, MM_cfdx1_ri   )
      if ( Set_MMex ) call at_ah_galerkin_MMex_Initial      &
                               (km,lm,(nm+1)*(nm+1), &
                                MMex_cfdx0_ro, MMex_cfdx1_ro,&
                                MMex_cfdx0_ri, MMex_cfdx1_ri   )

      if ( Set_RRFF ) call at_aq_galerkin_RRFF_Initial(km,lm,velBC)

      call MessageNotify('M','wt_galerkin_initial','wt_galerkin_module_sjpack is initialized')
    end subroutine wt_galerkin_initial

   !----- �ݥ�����®�پ�ݥƥ󥷥�� �դ� ��^2�� ������� ------
    function wq_LaplaPol2Pol_wt(wt_LaplaPolvel)
      !
      !  3 ��������ΰ�
      !  ����Ĵ��ȡ��Ÿ�� + �����ӥ����աݥ��顼����ˡ
      !
      !  �ݥ�����®�پ�ݥƥ󥷥�� �դ� ��^2�� �������
      !
      real(8), intent(IN) :: wt_LaplaPolvel((nm+1)*(nm+1),0:lm)
        !(in) �ݥ�����®�پ�ݥƥ󥷥��Υ�ץ饷���� ��^2��
      real(8)             :: wq_LaplaPol2Pol_wt((nm+1)*(nm+1),4:lm) 
        !(out) �ݥ�����®�پ�ݥƥ󥷥�� ��

      real(8), allocatable :: LaplaMT(:,:,:)
      real(8), allocatable :: LaplaInvMT(:,:,:)
      integer, allocatable :: kpvot(:,:)

      integer, parameter :: ls=4

      real(8) :: wq_work((nm+1)*(nm+1),ls:lm)
      real(8) :: wt_work((nm+1)*(nm+1),0:lm)

      integer :: k, m, n, l, p
      logical :: first = .true.
      save LaplaInvMT, kpvot, first

      if ( .not. Set_RRFF ) &
           call MessageNotify('E','wq_LaplaPol2Pol_wt',&
                    'at_aq_galerkin_RRFF_module not initialized.')

      if ( first ) then
         first = .false.
         allocate(LaplaMT((nm+1)*(nm+1),0:lm,0:lm))
         allocate(LaplaInvMT((nm+1)*(nm+1),ls:lm,ls:lm))
         allocate(kpvot((nm+1)*(nm+1),ls:lm))

         LaplaMT=0.0D0
         do l=0,lm
            wt_work = 0.0D0 ; wt_work(:,l) = 1.0D0
            wt_work = wt_Lapla_wt(wt_work)
            LaplaMT(:,:,l) = wt_work
         enddo

         LaplaInvMT=0.0D0
         do n=ls,lm
            do m=ls,lm
               do l=0,lm
                  do p=0,lm
                     LaplaInvMT(:,n,m)=LaplaInvMT(:,n,m) &
                          +TQ(l,n)*LaplaMT(:,l,p)*alpha(p)*TQ(p,m)
                  enddo
               enddo
            enddo
         enddo

         call LUDecomp(LaplaInvMT,kpvot)
         deallocate(LaplaMT)
      endif

      wq_work=0.0D0
      do m=ls,lm
         do k=0,lm
            wq_work(:,m)=wq_work(:,m) &
                 + alpha(k) * beta(k) * wt_LaplaPolvel(:,k)* TQ(k,m) 
         enddo
      enddo

      wq_LaplaPol2Pol_wt = LUSolve(LaplaInvMT,kpvot,wq_work)

    end function wq_LaplaPol2Pol_wt

end module wt_galerkin_module_sjpack

