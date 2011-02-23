!--
!----------------------------------------------------------------------
! Copyright(c) 2006-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  et_galerkin_module
!
!      2 ������ϩ�ΰ�
!      Fourier Ÿ�� + �����ӥ����աݥ��顼����ˡ�ѥ⥸�塼��
!
!����  2006/01/24  �ݹ�����  ��������
!      2006/02/28  �ݹ�����  �����Ȥ� RDoc �Ѥ��ѹ�
!      2009/06/03  �ݹ�����  ��ʬ�ؿ��ɲý���
!      2009/06/05  �ݹ�����  et_ef, et_eh ����̾�ѹ�, ����������
!      2009/07/05  �ݹ�����  et_LaplaInv_et �ɲ�
!      2009/07/31  �ݹ�����  �������׻�������� threadprivate ����(OpenMP)
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!++
module et_galerkin_module
  !
  != et_galerkin_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: et_galerkin_module.f90,v 1.8 2010-03-02 10:09:03 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/et_galerkin_module �⥸�塼��� 2 ��������ͥ��ΰ�Ǥ�ή�α�ư
  ! �򥹥ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ����󶡤���.
  ! ����Ū�ʶ������򰷤������ X �����ؤΥա��ꥨ�Ѵ�, 
  ! �����ɤ򰷤������ Y �����Υ����ӥ������Ѵ�(�����ӥ����ա����顼����ˡ)
  ! �Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���
  !
  ! ������ et_module, ����Ӷ�������򤯤���˥����ӥ����ա����顼����ˡ��
  ! �⥸�塼�뷲(at_ab_galerkin_ND, at_ad_galerkin_DD, at_af_galerkin_MM,
  ! at_ah_galerkin_MMex, at_ap_galerkin_DN, at_aq_galerkin_RRFF,
  ! at_av_galerkin_NN) ���Ѥ��Ƥ���.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (et_, yx_, x_, y_...) ��, �֤��ͤη��򼨤��Ƥ���.
  !
  !   et_ :: 2�������ڥ��ȥ�ǡ���
  !   yx_ :: 2 �����ʻ����ǡ���
  !   x_  :: X ���� 1 �����ʻ����ǡ���
  !   y_  :: Y ���� 1 �����ʻ����ǡ���
  !
  !   ed_ :: 2�������顼����ǡ���(y=ymin,ymax ���ͤ� 0)
  !   ev_ :: 2�������顼����ǡ���(y=ymin,ymax �� 1 ����ʬ�ͤ� 0)
  !   eb_ :: 2�������顼����ǡ���(y=ymin ���ͤ� 0, ymax �� 1 ����ʬ�ͤ� 0)
  !   ep_ :: 2�������顼����ǡ���(y=ymin ��1 ����ʬ�ͤ� 0, ymax �� �ͤ� 0)
  !   ef_ :: 2�������顼����ǡ���(y=ymin,ymax �Ǻ��維����郎 0)
  !   eh_ :: 2�������顼����ǡ���(y=ymin,ymax �ǿ�ʿ�ȿ���κ��維����郎 0)
  !   eq_ :: 2����ή���ؿ����顼����ǡ���(y=ymin,ymax �Ǽ�ͳ���٤�/Ǵ����)
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dx, Dy, Lapla, LaplaInv, Jacobian)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_et_et, _et, _yx, _x, _y...) ��, �����ѿ��Υ��ڥ��ȥ�ǡ���
  !   �ʻ����ǡ�������ӥ��顼����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !
  !   _et :: 2�������ڥ��ȥ�ǡ���
  !   _yx :: 2 �����ʻ����ǡ���
  !   _x  :: X ���� 1 �����ʻ����ǡ���
  !   _y  :: Y ���� 1 �����ʻ����ǡ���
  !   _ed :: 2�������顼����ǡ���(y=ymin,ymax ���ͤ� 0)
  !   _ev :: 2�������顼����ǡ���(y=ymin,ymax �� 1 ����ʬ�ͤ� 0)
  !   _eb :: 2�������顼����ǡ���(y=ymin ���ͤ� 0, ymax �� 1 ����ʬ�ͤ� 0)
  !   _ep :: 2�������顼����ǡ���(y=ymin ��1 ����ʬ�ͤ� 0, ymax �� �ͤ� 0)
  !   _ef :: 2�������顼����ǡ���(y=ymin,ymax �Ǻ��維����郎 0)
  !   _eh :: 2�������顼����ǡ���(y=ymin,ymax �ǿ�ʿ�ȿ���κ��維����郎 0)
  !   _eq :: 2����ή���ؿ����顼����ǡ���(y=ymin,ymax �Ǽ�ͳ���٤�/Ǵ����)
  !
  !=== �ƥǡ����μ��������
  !
  ! * yx : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:jm,0:im-1). 
  !   * im, jm �Ϥ��줾�� X, Y ��ɸ�γʻ������Ǥ���, ���֥롼����
  !     et_galerkin_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !   * �� 1 ������ Y ��ɸ�γʻ��������ֹ�, �� 2 ������ X ��ɸ��
  !     �ʻ��������ֹ�Ǥ��� (X, Y �ν�ǤϤʤ�)���Ȥ����.
  !
  ! * et : 2 �������ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km,0:lm). 
  !   * km, lm �Ϥ��줾�� X, Y �����κ����ȿ��Ǥ���, ���֥롼����
  !     et_galerkin_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ�...
  !
  ! * ed, ev, eb, ef, eh : 2 �������顼����ǡ���. 
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km,2:lm). 
  !   * km, lm �Ϥ��줾�� X, Y �����κ����ȿ��Ǥ���, ���֥롼����
  !     et_galerkin_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !
  ! * eq : 2 �������顼����ǡ���. 
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km,4:lm). 
  !   * km, lm �Ϥ��줾�� X, Y �����κ����ȿ��Ǥ���, ���֥롼����
  !     et_galerkin_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !
  ! * x, y : X, Y ���� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1)
  !     ����� real(8), dimension(0:jm).
  !
  ! * e, t : 1 �������ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km) 
  !     ����� real(8), dimension(-lm:lm).
  !
  ! * ax, ay : 1 �����ʻ����ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,0:im-1) 
  !     ����� real(8), dimension(:,0:jm).
  !
  ! * ae, at : 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,-km:km) 
  !     ����� real(8), dimension(:,0:lm).
  !
  ! * et_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * ed_, ev_, eb_, ep_, ef_, eh_, eq_ �ǻϤޤ�ؿ����֤��ͤ�
  !   ���顼����ǡ�����Ʊ��.
  !
  ! * yx_ �ǻϤޤ�ؿ����֤��ͤ� 2 �����ʻ����ǡ�����Ʊ��.
  !
  ! * x_, y_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�/���顼����ǡ������Ф�����ʬ���κ��ѤȤ�, 
  !   �б�����ʻ����ǡ�������ʬ�ʤɤ���Ѥ������ǡ�����
  !   ���ڥ��ȥ�/���顼�����Ѵ�������Τ��ȤǤ���.
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! et_galerlin_Initial :: ���ڥ��ȥ�/���顼�����Ѵ��γʻ�����, �ȿ�, 
  !                        �ΰ���礭��, ���Ѥ��륬�顼�����ѿ�������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_X, y_Y     ::  �ʻ�����ɸ(X,Y��ɸ)���Ǽ���� 1 ��������
  ! x_X_Weight, y_Y_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! yx_X, yx_Y   :: �ʻ����ǡ����� XY ��ɸ(X,Y)(�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! yx_et :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! et_yx :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! ax_ae, x_e :: X �����Υ��ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! ay_at, y_t :: Y �����Υ��ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! ae_ax, e_x :: X �����γʻ����ǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! at_ay, t_y :: Y �����γʻ����ǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  ! ed_yx      :: ���顼����ǡ���->�ʻ����Ѵ� 
  ! yx_ed      :: �ʻ���->���顼����ǡ����Ѵ� 
  ! ed_et, d_t :: �����ӥ�����->���顼�����Ѵ�
  ! et_ed, t_d :: ���顼����->�����ӥ������Ѵ�
  ! ed_ey, d_y :: �ʻ���->���顼����ǡ����Ѵ�
  ! ey_ed, y_d :: ���顼����->�ʻ����ǡ����Ѵ�
  !
  ! ev_yx      :: ���顼����ǡ���->�ʻ����Ѵ� 
  ! yx_ev      :: �ʻ���->���顼����ǡ����Ѵ� 
  ! ev_et, v_t :: �����ӥ�����->���顼�����Ѵ�
  ! et_ev, t_v :: ���顼����->�����ӥ������Ѵ�
  ! ev_ey, v_y :: �ʻ���->���顼����ǡ����Ѵ�
  ! ey_ev, y_v :: ���顼����->�ʻ����ǡ����Ѵ�
  !
  ! eb_yx      :: ���顼����ǡ���->�ʻ����Ѵ� 
  ! yx_eb      :: �ʻ���->���顼����ǡ����Ѵ� 
  ! eb_et, b_t :: �����ӥ�����->���顼�����Ѵ�
  ! et_eb, t_b :: ���顼����->�����ӥ������Ѵ�
  ! eb_ey, b_y :: �ʻ���->���顼����ǡ����Ѵ�
  ! ey_eb, y_b :: ���顼����->�ʻ����ǡ����Ѵ�
  !
  ! ep_yx      :: ���顼����ǡ���->�ʻ����Ѵ� 
  ! yx_ep      :: �ʻ���->���顼����ǡ����Ѵ� 
  ! ep_et, p_t :: �����ӥ�����->���顼�����Ѵ�
  ! et_ep, t_p :: ���顼����->�����ӥ������Ѵ�
  ! ep_ey, p_y :: �ʻ���->���顼����ǡ����Ѵ�
  ! ey_ep, y_p :: ���顼����->�ʻ����ǡ����Ѵ�
  !
  ! ef_yx      :: ���顼����ǡ���->�ʻ����Ѵ� 
  ! yx_ef      :: �ʻ���->���顼����ǡ����Ѵ� 
  ! ef_et, f_t :: �����ӥ�����->���顼�����Ѵ�
  ! et_ef, t_f :: ���顼����->�����ӥ������Ѵ�
  ! ef_ey, f_y :: �ʻ���->���顼����ǡ����Ѵ�
  ! ey_ef, y_f :: ���顼����->�ʻ����ǡ����Ѵ�
  !
  ! eh_yx      :: ���顼����ǡ���->�ʻ����Ѵ� 
  ! yx_eh      :: �ʻ���->���顼����ǡ����Ѵ� 
  ! eh_et      :: �����ӥ�����->���顼�����Ѵ�
  ! et_eh      :: ���顼����->�����ӥ������Ѵ�
  ! eh_ey      :: �ʻ���->���顼����ǡ����Ѵ�
  ! ey_eh      :: ���顼����->�ʻ����ǡ����Ѵ�
  !
  ! eq_yx      :: ���顼����ǡ���->�ʻ����Ѵ� 
  ! yx_eq      :: �ʻ���->���顼����ǡ����Ѵ� 
  ! eq_et, q_t :: �����ӥ�����->���顼�����Ѵ�
  ! et_eq, t_q :: ���顼����->�����ӥ������Ѵ�
  ! eq_ey, q_y :: �ʻ���->���顼����ǡ����Ѵ�
  ! ey_eq, y_q :: ���顼����->�ʻ����ǡ����Ѵ�
  !
  !==== ��ʬ
  !
  ! et_Lapla_et     :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! et_LaplaInv_et  :: ���ڥ��ȥ�ǡ����˵ե�ץ饷�������Ѥ�����
  ! et_Dx_et, ae_Dx_ae, e_Dx_e :: ���ڥ��ȥ�ǡ����� X ��ʬ����Ѥ�����
  ! et_Dy_et, at_Dy_at, t_Dy_t :: ���ڥ��ȥ�ǡ����� Y ��ʬ����Ѥ�����
  ! et_Jacobian_et_et :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����׻�����
  !
  ! ed_Dx_ed         :: X ��ʬ
  ! ed_Dy_ed, d_Dy_d :: Y ��ʬ
  ! ev_Dx_ev         :: X ��ʬ
  ! ev_Dy_ev, v_Dy_v :: Y ��ʬ
  ! eb_Dx_eb         :: X ��ʬ
  ! eb_Dy_eb, b_Dy_b :: Y ��ʬ
  ! ep_Dx_ep         :: X ��ʬ
  ! ep_Dy_ep, p_Dy_p :: Y ��ʬ
  ! ef_Dx_ef         :: X ��ʬ
  ! ef_Dy_ef, f_Dy_f :: Y ��ʬ
  ! eh_Dx_eh         :: X ��ʬ
  ! eh_Dy_eh         :: Y ��ʬ
  ! eq_Dx_eq         :: X ��ʬ
  ! eq_Dy_eq, q_Dy_q :: Y ��ʬ
  !
  !==== ����������
  !
  ! eq_Vor2Strm_eq :: ���٤���ή����׻�����
  !
  !==== ��ʬ��ʿ��
  !
  ! IntYX_yx, AvrYX_yx   :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! y_IntX_yx, y_AvrX_yx :: 2 �����ʻ����ǡ����� X ������ʬ�����ʿ��
  ! IntX_x, AvrX_x       :: 1 ����(X)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntY_yx, x_AvrY_yx :: 2 �����ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntY_y, AvrY_y       :: 1 ����(Y)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  !
  !== ���
  !
  ! Y ��ʬ�׻���Ϣ³���ƹԤ����ˤϥ��顼����ǡ����Ѵؿ����Ѥ���ΤǤʤ�
  ! �����ӥ����ե��ڥ��ȥ��Ѵؿ����Ѥ��뤳��
  !
  !   �� : ed_Dy_ed(ed_Dy_ed(ed)
  !   �� : ed_et(et_Dy_et(et_Dy_et(et_ed(ed))))
  !
  use dc_message
  use lumatrix
  use et_module
  use at_ad_galerkin_DD, only: at_ad_galerkin_DD_Initial,      &
                               ed_et => ad_at, et_ed => at_ad, &
                               ed_ey => ad_ag, ey_ed => ag_ad, &
                               ed_Dy_ed => ad_Dx_ad,           &
                               d_t, t_d,                       &
                               d_y => d_g, y_d => g_d,         &
                               d_Dy_d => d_Dx_d
  use at_av_galerkin_NN, only: at_av_galerkin_NN_Initial,      &
                               ev_et => av_at, et_ev => at_av, &
                               ev_ey => av_ag, ey_ev => ag_av, &
                               ev_Dy_ev => av_Dx_av,           &
                               v_t, t_v,                       &
                               v_y => v_g, y_v => g_v,         &
                               v_Dy_v => v_Dx_v
  use at_ab_galerkin_ND, only: at_ab_galerkin_ND_Initial,      &
                               eb_et => ab_at, et_eb => at_ab, &
                               eb_ey => ab_ag, ey_eb => ag_ab, &
                               eb_Dy_eb => ab_Dx_ab,           &
                               b_t, t_b,                       &
                               b_y => b_g, y_b => g_b,         &
                               b_Dy_b => b_Dx_b
  use at_ap_galerkin_DN, only: at_ap_galerkin_DN_Initial,      &
                               ep_et => ap_at, et_ep => at_ap, &
                               ep_ey => ap_ag, ey_ep => ag_ap, &
                               ep_Dy_ep => ap_Dx_ap,           &
                               p_t, t_p,                       &
                               p_y => p_g, y_p => g_p,         &
                               p_Dy_p => p_Dx_p
  use at_af_galerkin_MM, only: at_af_galerkin_MM_Initial,      &
                               ef_et => af_at, et_ef => at_af, &
                               ef_ey => af_ag, ey_ef => ag_af, &
                               ef_Dy_ef => af_Dx_af,           &
                               f_t, t_f,                       &
                               f_y => f_g, y_f => g_f,         &
                               f_Dy_f => f_Dx_f
  use at_ah_galerkin_MMex, only: at_ah_galerkin_MMex_Initial,    &
                                 eh_et => ah_at, et_eh => at_ah, &
                                 eh_ey => ah_ag, ey_eh => ag_ah, &
                                 eh_Dy_eh => ah_Dx_ah
  use at_aq_galerkin_RRFF, only: at_aq_galerkin_RRFF_Initial,    &
                                 eq_et => aq_at, et_eq => at_aq, &
                                 eq_ey => aq_ag, ey_eq => ag_aq, &
                                 eq_Dy_eq => aq_Dx_aq,           &
                                 q_t, t_q,                       &
                                 q_y => q_g, y_q => g_q,         &
                                 q_Dy_q => q_Dx_q,               &
                                 TQ, QT, alpha, beta

  implicit none
  private

  public et_galerkin_Initial                              ! �����
  public eq_Vor2Strm_et                                   ! ����->ή��

  !-- et_module 
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! ��ɸ�ѿ�
  public yx_et, et_yx                                     ! �����Ѵ�
  public e_x, x_e, ae_ax, ax_ae                           ! �����Ѵ�
  public t_y, y_t, at_ay, ay_at                           ! �����Ѵ�

  public et_Dx_et, e_Dx_e, ae_Dx_ae                       ! ��ʬ
  public et_Dy_et, t_Dy_t, at_Dy_at                       ! ��ʬ
  public et_Lapla_et                                      ! ��ʬ
  public et_LaplaInv_et                                   ! ��ʬ

  public et_Jacobian_et_et                                ! �������׻�

  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! ��ʬ
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! ʿ��

  !-- at_ad_galerkin_DD
  public :: ed_yx, yx_ed               ! �ʻ���<->���顼����ǡ����Ѵ�
  public :: ed_et, d_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: ed_ey, d_y                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: et_ed, t_d                 ! ���顼����->�����ӥ������Ѵ�
  public :: ey_ed, y_d                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ed_Dx_ed                   ! X ��ʬ
  public :: ed_Dy_ed, d_Dy_d           ! Y ��ʬ

  !-- at_av_galerkin_NN
  public :: ev_yx, yx_ev               ! �ʻ���<->���顼����ǡ����Ѵ�
  public :: ev_et, v_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: ev_ey, v_y                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: et_ev, t_v                 ! ���顼����->�����ӥ������Ѵ�
  public :: ey_ev, y_v                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ev_Dx_ev                   ! X ��ʬ
  public :: ev_Dy_ev, v_Dy_v           ! Y ��ʬ

  !-- at_ab_galerkin_ND
  public :: eb_yx, yx_eb               ! �ʻ���<->���顼����ǡ����Ѵ�
  public :: eb_et, b_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: eb_ey, b_y                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: et_eb, t_b                 ! ���顼����->�����ӥ������Ѵ�
  public :: ey_eb, y_b                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: eb_Dx_eb                   ! X ��ʬ
  public :: eb_Dy_eb, b_Dy_b           ! Y ��ʬ

  !-- at_ap_galerkin_DN
  public :: ep_yx, yx_ep               ! �ʻ���<->���顼����ǡ����Ѵ�
  public :: ep_et, p_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: ep_ey, p_y                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: et_ep, t_p                 ! ���顼����->�����ӥ������Ѵ�
  public :: ey_ep, y_p                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ep_Dx_ep                   ! X ��ʬ
  public :: ep_Dy_ep, p_Dy_p           ! Y ��ʬ

  !-- at_af_galerkin_MM
  public :: ef_yx, yx_ef               ! �ʻ���<->���顼����ǡ����Ѵ�
  public :: ef_et, f_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: ef_ey, f_y                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: et_ef, t_f                 ! ���顼����->�����ӥ������Ѵ�
  public :: ey_ef, y_f                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ef_Dx_ef                   ! X ��ʬ
  public :: ef_Dy_ef, f_Dy_f           ! Y ��ʬ

  !-- at_ah_galerkin_MMex
  public :: eh_yx, yx_eh               ! �ʻ���<->���顼����ǡ����Ѵ�
  public :: eh_et                      ! �����ӥ�����->���顼�����Ѵ�
  public :: eh_ey                      ! �ʻ���->���顼����ǡ����Ѵ�
  public :: et_eh                      ! ���顼����->�����ӥ������Ѵ�
  public :: ey_eh                      ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: eh_Dx_eh                   ! X ��ʬ
  public :: eh_Dy_eh                   ! Y ��ʬ

  !-- at_aq_galerkin_RRFF
  public :: eq_yx, yx_eq               ! �ʻ���<->���顼����ǡ����Ѵ�
  public :: eq_et, q_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: eq_ey, q_y                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: et_eq, t_q                 ! ���顼����->�����ӥ������Ѵ�
  public :: ey_eq, y_q                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: eq_Dx_eq                   ! X ��ʬ
  public :: eq_Dy_eq, q_Dy_q           ! Y ��ʬ


  !-- �����ѿ�--
  integer :: im=128, jm=64               ! �ʻ���������(X,Y)
  integer :: km=42,  lm=42               ! �����ȿ�������(X,Y)

  real(8) :: xl=2.0                      ! �ΰ���礭��
  real(8), parameter :: pi=3.1415926535897932385D0

  logical :: Set_DD    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_NN    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_DN    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_ND    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_MM    =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_MMex  =.false.        ! �⥸�塼���ɤ߹��ߥ����å�
  logical :: Set_RRFF  =.false.        ! �⥸�塼���ɤ߹��ߥ����å�

  save im, jm, km, lm, xl, &
       Set_DD, Set_NN, Set_DN, Set_ND, Set_MM, Set_MMex, Set_RRFF

  contains
  !--------------- ����� -----------------
    subroutine et_galerkin_Initial(i,j,k,l,xmin,xmax,ymin,ymax,&
         DD,NN,DN,ND,velBC,&
         MM_cfdy0_ymin, MM_cfdy1_ymin, MM_cfdy0_ymax, MM_cfdy1_ymax,&
         MMex_cfdy0_ymin, MMex_cfdy1_ymin, MMex_cfdy0_ymax, MMex_cfdy1_ymax)
      !
      !  2 ������ϩ�ΰ�
      !  Fourier Ÿ�� + �����ӥ����աݥ��顼����ˡ�ѥ⥸�塼��
      !
      !  ��������֥롼����
      !
      integer,intent(in) :: i, j           ! �ʻ���������(X,Y)
      integer,intent(in) :: k, l           ! �����ȿ�������(X,Y)

      real(8),intent(in) :: xmin, xmax     ! X ��ɸ�ϰ�
      real(8),intent(in) :: ymin, ymax     ! Y ��ɸ�ϰ�

      logical,intent(in),optional :: DD  ! �⥸�塼���ɤ߹��ߥ����å�
      logical,intent(in),optional :: NN  ! �⥸�塼���ɤ߹��ߥ����å�
      logical,intent(in),optional :: DN  ! �⥸�塼���ɤ߹��ߥ����å�
      logical,intent(in),optional :: ND  ! �⥸�塼���ɤ߹��ߥ����å�

      ! at_af_galerkin_MM_module ������ﷸ��
      real(8),intent(in),optional :: MM_cfdy0_ymin ! (0����ʬ@x=ymin)
      real(8),intent(In),optional :: MM_cfdy1_ymin ! (1����ʬ@x=ymin)
      real(8),intent(in),optional :: MM_cfdy0_ymax ! (0����ʬ@x=ymax)
      real(8),intent(in),optional :: MM_cfdy1_ymax ! (1����ʬ@x=ymax)

      ! at_ah_galerkin_MMex_module ������ﷸ��
      real(8),intent(in),optional :: MMex_cfdy0_ymin(-k:k) ! (0����ʬ@x=ymin)
      real(8),intent(in),optional :: MMex_cfdy1_ymin(-k:k) ! (1����ʬ@x=ymin)
      real(8),intent(in),optional :: MMex_cfdy0_ymax(-k:k) ! (0����ʬ@x=ymax)
      real(8),intent(in),optional :: MMex_cfdy1_ymax(-k:k) ! (1����ʬ@x=ymax)

      ! at_aq_galerkin_RRFF_module ®�پ춭�����
      character(LEN=2),intent(in),optional :: velBC  ! �������(RR/SS/RS/SR)

      !--------------- �������� -----------------
      im=i ; jm = j ; km=k ; lm=l ; xl = xmax-xmin

      if ( present(DD) ) Set_DD = DD
      if ( present(NN) ) Set_NN = NN
      if ( present(DN) ) Set_DN = DN
      if ( present(ND) ) Set_ND = ND

      if ( present(MM_cfdy0_ymin) .AND.present(MM_cfdy1_ymin) .AND.&
           present(MM_cfdy0_ymax) .AND.present(MM_cfdy1_ymax) ) &
           Set_MM=.true.

      if ( present(MMex_cfdy0_ymin) .AND.present(MMex_cfdy1_ymin) .AND.&
           present(MMex_cfdy0_ymax) .AND.present(MMex_cfdy1_ymax) ) &
           Set_MMex=.true.

      if ( present(velBC) ) Set_RRFF = .true.

      !--------------- �⥸�塼������ -----------------
      call et_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax)
      if ( Set_DD ) call at_ad_galerkin_DD_Initial(jm,lm)
      if ( Set_NN ) call at_av_galerkin_NN_Initial(jm,lm)
      if ( Set_DN ) call at_ap_galerkin_DN_Initial(jm,lm)
      if ( Set_ND ) call at_ab_galerkin_ND_Initial(jm,lm)
      if ( Set_MM ) call at_af_galerkin_MM_Initial      &
                               (jm,lm, MM_cfdy0_ymax, MM_cfdy1_ymax,&
                                       MM_cfdy0_ymin, MM_cfdy1_ymin   )
      if ( Set_MMex ) call at_ah_galerkin_MMex_Initial      &
                               (jm,lm,2*km+1, &
                                MMex_cfdy0_ymax, MMex_cfdy1_ymax,&
                                MMex_cfdy0_ymin, MMex_cfdy1_ymin   )

      if ( Set_RRFF ) call at_aq_galerkin_RRFF_Initial(jm,lm,velBC)

    end subroutine et_galerkin_initial


  !--------------- �����Ѵ� -----------------

    function yx_ed(ed)
      !
      ! ���顼����ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ed
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,2:lm), intent(in)  :: ed
      !(in) ���ڥ��ȥ�ǡ���

      yx_ed = yx_et(et_ed(ed))

    end function yx_ed

    function ed_yx(yx)
      !
      ! �ʻҥǡ������饬�顼����ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,2:lm)              :: ed_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      ed_yx = ed_et(et_yx(yx))

    end function ed_yx

    function yx_ev(ev)
      !
      ! ���顼����ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ev
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,2:lm), intent(in)  :: ev
      !(in) ���ڥ��ȥ�ǡ���

      yx_ev = yx_et(et_ev(ev))

    end function yx_ev

    function ev_yx(yx)
      !
      ! �ʻҥǡ������饬�顼����ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,2:lm)              :: ev_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      ev_yx = ev_et(et_yx(yx))

    end function ev_yx

    function yx_eb(eb)
      !
      ! ���顼����ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_eb
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,2:lm), intent(in)  :: eb
      !(in) ���ڥ��ȥ�ǡ���

      yx_eb = yx_et(et_eb(eb))

    end function yx_eb

    function eb_yx(yx)
      !
      ! �ʻҥǡ������饬�顼����ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,2:lm)              :: eb_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      eb_yx = eb_et(et_yx(yx))

    end function eb_yx

    function yx_ep(ep)
      !
      ! ���顼����ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ep
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,2:lm), intent(in)  :: ep
      !(in) ���ڥ��ȥ�ǡ���

      yx_ep = yx_et(et_ep(ep))

    end function yx_ep

    function ep_yx(yx)
      !
      ! �ʻҥǡ������饬�顼����ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,2:lm)              :: ep_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      ep_yx = ep_et(et_yx(yx))

    end function ep_yx

    function yx_ef(ef)
      !
      ! ���顼����ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ef
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,2:lm), intent(in)  :: ef
      !(in) ���ڥ��ȥ�ǡ���

      yx_ef = yx_et(et_ef(ef))

    end function yx_ef

    function ef_yx(yx)
      !
      ! �ʻҥǡ������饬�顼����ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,2:lm)              :: ef_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      ef_yx = ef_et(et_yx(yx))

    end function ef_yx

    function yx_eh(eh)
      !
      ! ���顼����ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_eh
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,2:lm), intent(in)  :: eh
      !(in) ���ڥ��ȥ�ǡ���

      yx_eh = yx_et(et_eh(eh))

    end function yx_eh

    function eh_yx(yx)
      !
      ! �ʻҥǡ������饬�顼����ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,2:lm)              :: eh_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      eh_yx = eh_et(et_yx(yx))

    end function eh_yx

    function yx_eq(eq)
      !
      ! ���顼����ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_eq
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,4:lm), intent(in)  :: eq
      !(in) ���ڥ��ȥ�ǡ���

      yx_eq = yx_et(et_eq(eq))

    end function yx_eq

    function eq_yx(yx)
      !
      ! �ʻҥǡ������饬�顼����ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,4:lm)              :: eq_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      eq_yx = eq_et(et_yx(yx))

    end function eq_yx

  !--------------- ��ʬ�׻� -----------------

    function ed_Dx_ed(ed)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,2:lm)                :: ed_Dx_ed
      real(8), dimension(-km:km,2:lm), intent(in)    :: ed
      integer k

      do k=-km,km
         ed_Dx_ed(k,:)  =  (-2*pi*k/xl)*ed(-k,:)
      enddo
    end function ed_Dx_ed

    function ev_Dx_ev(ev)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,2:lm)                :: ev_Dx_ev
      real(8), dimension(-km:km,2:lm), intent(in)    :: ev
      integer k

      do k=-km,km
         ev_Dx_ev(k,:)  =  (-2*pi*k/xl)*ev(-k,:)
      enddo
    end function ev_Dx_ev

    function eb_Dx_eb(eb)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,2:lm)                :: eb_Dx_eb
      real(8), dimension(-km:km,2:lm), intent(in)    :: eb
      integer k

      do k=-km,km
         eb_Dx_eb(k,:)  =  (-2*pi*k/xl)*eb(-k,:)
      enddo
    end function eb_Dx_eb

    function ep_Dx_ep(ep)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,2:lm)                :: ep_Dx_ep
      real(8), dimension(-km:km,2:lm), intent(in)    :: ep
      integer k

      do k=-km,km
         ep_Dx_ep(k,:)  =  (-2*pi*k/xl)*ep(-k,:)
      enddo
    end function ep_Dx_ep

    function ef_Dx_ef(ef)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,2:lm)                :: ef_Dx_ef
      real(8), dimension(-km:km,2:lm), intent(in)    :: ef
      integer k

      do k=-km,km
         ef_Dx_ef(k,:)  =  (-2*pi*k/xl)*ef(-k,:)
      enddo
    end function ef_Dx_ef

    function eh_Dx_eh(eh)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,2:lm)                :: eh_Dx_eh
      real(8), dimension(-km:km,2:lm), intent(in)    :: eh
      integer k

      do k=-km,km
         eh_Dx_eh(k,:)  =  (-2*pi*k/xl)*eh(-k,:)
      enddo
    end function eh_Dx_eh

    function eq_Dx_eq(eq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,4:lm)                :: eq_Dx_eq
      real(8), dimension(-km:km,4:lm), intent(in)    :: eq
      integer k

      do k=-km,km
         eq_Dx_eq(k,:)  =  (-2*pi*k/xl)*eq(-k,:)
      enddo
    end function eq_Dx_eq

   !------------------- ���٤���ή������� ----------------------
    function eq_Vor2Strm_et(et_Zeta)
      !
      !  2 ������ϩ�ΰ�
      !  Fourier Ÿ�� + �����ӥ����աݥ��顼����ˡ�ѥ⥸�塼��
      !
      !  ���٤���ή����׻�����.
      !
      real(8), intent(IN) :: et_Zeta(-km:km,0:lm)        !(in)  ���� ��=��^2��
      real(8)             :: eq_Vor2Strm_et(-km:km,4:lm) !(out) ή�� ��

      real(8), allocatable :: LaplaMT(:,:,:)
      real(8), allocatable :: LaplaInvMT(:,:,:)
      integer, allocatable :: kpvot(:,:)

      integer, parameter :: ls=4

      real(8) :: eq_work(-km:km,ls:lm)
      real(8) :: et_work(-km:km,0:lm)

      integer :: k, m, n, l, p
      logical :: first = .true.
      save LaplaInvMT, kpvot, first

      if ( .not. Set_RRFF ) &
           call MessageNotify('E','eq_Vor2Strm_et',&
                    'at_aq_galerkin_RRFF_module not initialized.')

      if ( first ) then
         first = .false.
         allocate(LaplaMT(-km:km,0:lm,0:lm))
         allocate(LaplaInvMT(-km:km,ls:lm,ls:lm),kpvot(-km:km,ls:lm))

         LaplaMT=0.0D0
         do l=0,lm
            et_work = 0.0D0 ; et_work(:,l) = 1.0D0
            et_work = et_Lapla_et(et_work)
            LaplaMT(:,:,l) = et_work
         enddo

         LaplaInvMT=0.0
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

      eq_work=0.0
      do m=ls,lm
         do k=0,lm
            eq_work(:,m)=eq_work(:,m) &
                 + alpha(k) * beta(k) * et_Zeta(:,k)* TQ(k,m) 
         enddo
      enddo

      eq_Vor2Strm_et = LUSolve(LaplaInvMT,kpvot,eq_work)

    end function eq_Vor2Strm_et

end module et_galerkin_module
