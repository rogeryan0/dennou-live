!--
!----------------------------------------------------------------------
! Copyright(c) 2008-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eq_module
!      2 ���������ΰ�����, Fourier Ÿ�� + ¿�༰Ÿ��ˡ
!
!      spml/eq_module �⥸�塼��� 2 ���������ΰ�Ǥ�ή�α�ư��
!      ���ڥ��ȥ�ˡ�ˤ����ͷ׻���¹Ԥ��뤿��� Fortran90 �ؿ����󶡤���. 
!      ����Ū�ʶ������򰷤���������̳������ؤΥա��ꥨ�Ѵ���
!      �����ɤ򰷤������ư��������¿�༰�Ѵ����Ѥ������
!      ���ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
!
!      ������ ae_module, aq_module ���Ѥ��Ƥ���. 
!      �ǲ����Ǥϥա��ꥨ�Ѵ�����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� 
!      ISPACK/FTPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
!
!      Matsushima and Marcus (1994) ��¿�༰�˴ؤ��������� 
!      doc/spectral_radial.tex �򻲾ȤΤ���. 
!
!
!����  2008/04/11  �ݹ�����  et_module ����¤
!      2008/05/02  �ݹ�����  �����Ƚ���
!      2008/10/29  �ݹ�����  eq_Vor2Strm_eq ���������롼�������
!      2009/01/09  �ݹ�����  eq_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ˽���
!      2009/07/31  �ݹ�����  �������׻�������� threadprivate ����(OpenMP)
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!++
module eq_module
  !
  != eq_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: eq_module.f90,v 1.8 2010-03-02 10:09:02 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/eq_module �⥸�塼��� 2 ���������ΰ�Ǥ�ή�α�ư��
  ! ���ڥ��ȥ�ˡ�ˤ����ͷ׻���¹Ԥ��뤿��� Fortran90 �ؿ����󶡤���. 
  ! ����Ū�ʶ������򰷤���������̳������ؤΥա��ꥨ�Ѵ���
  ! �����ɤ򰷤������ư��������¿�༰�Ѵ����Ѥ������
  ! ���ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
  !
  ! ������ ae_module, aq_module ���Ѥ��Ƥ���. 
  ! �ǲ����Ǥϥա��ꥨ�Ѵ�����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� 
  ! ISPACK/FTPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
  !
  ! Matsushima and Marcus (1994) ��¿�༰�˴ؤ��������� 
  ! ư�º�ɸ�Υ��ڥ��ȥ�ˡ(spectral_radial.pdf[link:./spectral_radial.pdf])��
  ! ���ȤΤ���. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (eq_, rp_, r_, p_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   eq_ :: 2�������ڥ��ȥ�ǡ���
  !   rp_ :: 2 �����ʻ����ǡ���
  !   r_  :: ư������ 1 �����ʻ����ǡ���
  !   p_  :: ���̳����� 1 �����ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dr, Dp, Lapla, LaplaInv, Jacobian)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_eq_eq,_eq,_rp, _r, _p) ��, �����ѿ��Υ��ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _eq    :: 2�������ڥ��ȥ�ǡ���
  !   _eq_eq :: 2 �Ĥ�2�������ڥ��ȥ�ǡ���
  !   _rp    :: 2 �����ʻ����ǡ���
  !   _r     :: ư������ 1 �����ʻ����ǡ���
  !   _p     :: ���̳����� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * rp : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(jm,0:im-1). 
  !   * im, jm �Ϥ��줾�����̳�, ư�º�ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� eq_initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !   * �� 1 ������ư�º�ɸ�γʻ��������ֹ�, �� 2 ���������̳Ѻ�ɸ��
  !     �ʻ��������ֹ�Ǥ���. 
  !
  ! * eq : 2 �������ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km,0:lm). 
  !   * km, lm �Ϥ��줾�����̳�, ư�������κ����ȿ��Ǥ���, 
  !     ���֥롼���� eq_initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ư�¥��ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ� 
  !     aq_module.f90 �򻲾ȤΤ���.
  !
  ! * p, r : X, Y ���� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1)
  !     ����� real(8), dimension(jm).
  !
  ! * e, q : 1 �������ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km) 
  !     ����� real(8), dimension(0:lm).
  !
  ! * ap, ar : 1 �����ʻ����ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,0:im-1) 
  !     ����� real(8), dimension(:,jm).
  !
  ! * ae, aq : 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,-km:km) 
  !     ����� real(8), dimension(:,0:lm).
  !
  ! * eq_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * rp_ �ǻϤޤ�ؿ����֤��ͤ� 2 �����ʻ����ǡ�����Ʊ��.
  !
  ! * p_, p_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! eq_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! p_Phi, r_Rad               ::  �ʻ�����ɸ(X,Y��ɸ)���Ǽ���� 1 ��������
  ! p_Phi_Weight, r_Rad_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! rp_Phi, rp_Rad             ::  �ʻ����ǡ����� XY ��ɸ(X,Y)
  !                                (�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! rp_eq :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! eq_rp :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! ap_ae, p_e :: ���̳������Υ��ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! ar_aq, r_q :: ư�������Υ��ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! ae_ap, e_p :: ���̳������γʻ����ǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! aq_ar, q_r :: ư�������γʻ����ǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! eq_Lapla_eq  :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! eq_DPhi_eq, ae_DPhi_ae, e_DPhi_e :: ���ڥ��ȥ�ǡ�����
  !                                     ���̳���ʬ����Ѥ�����
  ! eq_RadDRad_eq, aq_RadDRad_aq, q_RadDRad_q :: ���ڥ��ȥ�ǡ�����
  !                                              ư����ʬ����Ѥ�����
  ! eq_Jacobian_eq_eq :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����׻�����
  !
  !==== ����������
  !
  ! eq_Boundary    :: �ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ��
  ! eq_LaplaInv_eq :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! eq_Vor2Strm_eq :: ���٤���ή����׻�����
  !
  !==== ��ʬ��ʿ��
  !
  ! IntRadPhi_rp, AvrRadPhi_rp   :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! r_IntPhi_rp, r_AvrPhi_rp :: 2 �����ʻ����ǡ��������̳�������ʬ�����ʿ��
  ! IntPhi_p, AvrPhi_p       :: 1 ����(X)�ʻ����ǡ��������̳�������ʬ�����ʿ��
  ! p_IntRad_rp, p_AvrRad_rp :: 2 �����ʻ����ǡ�����ư��������ʬ�����ʿ��
  ! IntRad_r, AvrRad_r       :: 1 ����(Y)�ʻ����ǡ�����ư��������ʬ�����ʿ��
  !
  use dc_message
  use lumatrix
  use ae_module, p_Phi => g_X, p_Phi_weight => g_X_Weight, &
                 e_p => e_g, ae_ap => ae_ag, &
                 p_e => g_e, ap_ae => ag_ae, &
                 ae_DPhi_ae => ae_Dx_ae, e_DPhi_e => e_Dx_e
  use aq_module, r_Rad => g_R, r_Rad_Weight => g_R_Weight, &
                 aq_ar => aq_ag, q_r => q_g, &
                 ar_aq => ag_aq, r_q => g_q, &
                 q_RadDRad_q => q_rDr_q, aq_RadDRad_aq => aq_rDr_aq

  implicit none
  private

  public eq_Initial                                       ! �����

  public p_Phi, p_Phi_Weight, rp_Phi                      ! ��ɸ�ѿ�
  public r_Rad, r_Rad_Weight, rp_Rad, er_Rad              ! ��ɸ�ѿ�

  public rp_eq, eq_rp                                     ! �����Ѵ�
  public er_eq, eq_er                                     ! �����Ѵ�
  public er_rp, rp_er                                     ! �����Ѵ�
  public e_p, p_e, ae_ap, ap_ae                           ! �����Ѵ�
  public q_r, r_q, aq_ar, ar_aq                           ! �����Ѵ�

  public eq_DPhi_eq, e_DPhi_e, ae_DPhi_ae                 ! ��ʬ
  public eq_RadDRad_eq, q_RadDRad_q, aq_RadDRad_aq        ! ��ʬ
  public er_Lapla_eq, eq_Lapla_eq                         ! ��ʬ

  public eq_Jacobian_eq_eq                                ! �������׻�

  public eq_Boundary                                      ! ����������
  public aq_Boundary_D, aq_Boundary_N                     ! ����������
  public eq_LaplaInv_eq, eq_Vor2Strm_eq                   ! ����������

  public IntRadPhi_rp, r_IntPhi_rp, p_IntRad_rp, IntPhi_p, IntRad_r   ! ��ʬ
  public AvrRadPhi_rp, r_AvrPhi_rp, p_AvrRad_rp, AvrPhi_p, AvrRad_r   ! ʿ��

  integer            :: im=32, jm=8      ! �ʻ���������(Phi,Rad)
  integer            :: km=10, lm=5      ! �����ȿ�������(Phi,Rad)
  real(8)            :: ra=1.0           ! �ΰ���礭��
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), parameter :: alpha = 1.0D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8), parameter :: beta  = 1.0D0        ! Ÿ��¿�༰�ѥ�᥿��  0 < ��

  real(8), dimension(:,:), allocatable :: rp_Phi, rp_Rad
  real(8), dimension(:,:), allocatable :: er_Rad
  integer, dimension(:), allocatable   :: md

  save im, jm, km, lm, ra, md

  contains
  !--------------- ����� -----------------
    subroutine eq_Initial(i,j,k,l,ra_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭�������ꤹ��.
      !
      ! ¾�δؿ����ѿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
      ! �������򤷤ʤ���Фʤ�ʤ�.
      !
      integer,intent(in) :: i           ! �ʻ�����(X)
      integer,intent(in) :: j           ! �ʻ�����(Y)
      integer,intent(in) :: k           ! �����ȿ�(X)
      integer,intent(in) :: l           ! �����ȿ�(Y)

      real(8),intent(in) :: ra_in       ! Ⱦ��

      integer :: kk

      im = i       ; jm = j
      km = k       ; lm = l
      ra = ra_in

      allocate(md(-km:km))

      do kk=-km,km
         md(kk) = abs(kk)
      enddo

      call ae_initial(im,km,0.0D0,2*pi)
      call aq_Initial(jm,lm,ra,alpha,beta,md)

      allocate(rp_Phi(jm,0:im-1),rp_Rad(jm,0:im-1))
      rp_Phi = spread(p_Phi,1,jm)
      rp_Rad = spread(r_Rad,2,im)

      allocate(er_Rad(-km:km,jm))
      er_Rad = spread(r_Rad,1,2*km+1)

      call MessageNotify('M','eq_initial','eq_module (2009/07/31) is initialized')
    end subroutine eq_initial

  !--------------- �����Ѵ� -----------------

    function rp_eq(eq)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(jm,0:im-1)                :: rp_eq
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(in) ���ڥ��ȥ�ǡ���

      rp_eq = ap_ae(transpose(ar_aq(eq)))

    end function rp_eq

    function eq_rp(rp)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,0:lm)              :: eq_rp
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(jm,0:im-1), intent(in)    :: rp
      !(in) �ʻ����ǡ���

      eq_rp = aq_ar(transpose(ae_ap(rp)))

    end function eq_rp

    function eq_er(er)
      !
      ! ư�³ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,0:lm)              :: eq_er
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(-km:km,jm), intent(in)    :: er
      !(in) �ʻ����ǡ���

      eq_er = aq_ar(er)

    end function eq_er

    function er_eq(eq)
      !
      ! ư�³ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,jm)                :: er_eq
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(out) ���ڥ��ȥ�ǡ���

      er_eq = ar_aq(eq)

    end function er_eq

    function rp_er(er)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(jm,0:im-1)              :: rp_er
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,jm), intent(in)  :: er
      !(in) ���ڥ��ȥ�ǡ���

      rp_er = ap_ae(transpose(er))

    end function rp_er

    function er_rp(rp)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,jm)                :: er_rp
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(jm,0:im-1), intent(in)    :: rp
      !(in) �ʻ����ǡ���

      er_rp = transpose(ae_ap(rp))

    end function er_rp

  !--------------- ��ʬ�׻� -----------------

    function eq_DPhi_eq(eq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ��������̳���ʬ(�ߦ�)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Φ���ʬ�Ȥ�, �б�����ʻ����ǡ����˦���ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,0:lm)                :: eq_DPhi_eq
      real(8), dimension(-km:km,0:lm), intent(in)    :: eq
      integer k

      do k=-km,km
         eq_DPhi_eq(k,:)  =  -k*eq(-k,:)
      enddo
    end function eq_DPhi_eq

    function eq_RadDRad_eq(eq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ�����ư����ʬ(r��r)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ�����ư����ʬ�Ȥ�, �б�����ʻ����ǡ�����ư����ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(-km:km,0:lm)               :: eq_RadDRad_eq
      !(out) ���ڥ��ȥ�ǡ�����ư����ʬ

      real(8), dimension(-km:km,0:lm), intent(in)   :: eq
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      eq_RadDRad_eq = aq_RadDRad_aq(eq)

    end function eq_RadDRad_eq

    function er_Lapla_eq(eq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷���� 
      !  (1/r)(��r(r��r)+ (1/r^2) �ߦզ� ����Ѥ���.
      !
      real(8), dimension(-km:km,jm)                :: er_Lapla_eq
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(-km:km,0:lm)              :: eq_work

      integer k

      do k=-km,km
         eq_work(k,:) = -k**2 * eq(k,:)
      enddo

      er_Lapla_eq = er_eq(eq_work + eq_RadDRad_eq(eq_RadDRad_eq(eq)))/er_Rad**2

    end function er_Lapla_eq

    function eq_Lapla_eq(eq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷���� 
      !  (1/r)(��r(r��r)+ (1/r^2) �ߦզ� ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, 
      ! �б�����ʻ����ǡ����˥�ץ饷�������Ѥ������ǡ�����
      ! ���ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension(-km:km,0:lm)              :: eq_Lapla_eq
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      eq_Lapla_eq = eq_er(er_Lapla_eq(eq))

    end function eq_Lapla_eq

    function eq_Jacobian_eq_eq(eq_a,eq_b)
      !
      !  2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ���
      !
      !     J(A,B)=1/r[(��rA)(�ߦ�B)-(�ߦ�A)(��rB)]
      !
      !  ��׻�����. 1/r �Υե����������Ĥ��Ƥ��뤳�Ȥ����. 
      !
      !  2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���Ȥ�, �б����� 2 �Ĥ�
      !  �ʻ����ǡ����Υ䥳�ӥ���Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(-km:km,0:lm)                :: eq_Jacobian_eq_eq
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), dimension(-km:km,0:lm), intent(in)    :: eq_a
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(-km:km,0:lm), intent(in)    :: eq_b
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���

      eq_Jacobian_eq_eq = eq_rp(&
           (  rp_eq(eq_RadDRad_eq(eq_a)) * rp_eq(eq_DPhi_eq(eq_b))   &
             -rp_eq(eq_DPhi_eq(eq_a)) * rp_eq(eq_RadDRad_eq(eq_b)) ) &
           /rp_Rad**2)

    end function eq_Jacobian_eq_eq


  !--------------- ���������� -----------------

    subroutine eq_Boundary(eq,value,cond)
      !
      ! �ǥ��ꥯ��, �Υ��ޥ����Ŭ��. �����ӥ����ն��֤Ǥη׻�
      !
      ! �ºݤˤ���ǸƤФ�Ƥ��� aq_module �Υ��֥롼���� 
      ! aq_Boundary_D,, aq_Boundary_N ���Ѥ��Ƥ���. 
      ! ������ľ�ܸƤ֤��Ȥ�����.
      !
      real(8), dimension(-km:km,0:lm),intent(inout)      :: eq
              ! ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(-km:km), intent(in), optional   :: value
              ! �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              ! ��ά������/���� 0 �Ȥʤ�. 

      character(len=1), intent(in), optional             :: cond
              ! �������. ��ά���� 'D'
              !   D : ξü�ǥ��ꥯ��
              !   N : ξü�Υ��ޥ�

      if (.not. present(cond)) then
         if (present(value)) then
            call aq_Boundary_D(eq,value)
         else
            call aq_Boundary_D(eq)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call aq_Boundary_N(eq,value)
         else
            call aq_Boundary_N(eq)
         endif
      case ('D')
         if (present(value)) then
            call aq_Boundary_D(eq,value)
         else
            call aq_Boundary_D(eq)
         endif
      case default
         call MessageNotify('E','eq_Boundaries','B.C. not supported')
      end select

    end subroutine eq_Boundary

    function eq_LaplaInv_eq(eq,value)
      !
      ! �������ͤ�Ϳ������(�ǥ��ꥯ����)����, 
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����
      ! [(1/r)(��r(r��r)+ (1/r^2) �ߦզ�]^{-1} ����Ѥ���.
      !
      ! ����ˡ�ˤ��׻�
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: eq
      !(in) ���ڥ��ȥ�ǡ���

      real(8), dimension(-km:km,0:lm)             :: eq_LaplaInv_eq
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), dimension(-km:km), intent(in), optional :: value
      !(in) ������. ��ά���� 0 �����ꤵ���. 

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: eq_work
      real(8), dimension(-km:km,jm)           :: er_work
      real(8), dimension(-km:km)              :: value1       ! ������

      logical :: first = .true.
      integer :: k, l
      save    :: alu, kp, first

      if (.not. present(value)) then
         value1=0
      else
         value1 = value
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         do l=0,lm
            eq_work = 0.0 ; eq_work(:,l) = 1.0
            alu(:,:,l) = eq_er(er_Lapla_eq(eq_work))
         enddo

         ! 0 ��ʬ�ΤȤ���� 1 ������.
         do k=-km,km
            do l=0,md(k)-1
               alu(k,l,l) = 1.0D0
            enddo
            do l=md(k)+1,lm,2
               alu(k,l,l) = 1.0D0
            enddo
         enddo

         ! ������� r=ra ���ͤ�Ϳ����. 
         do k=-km,km
            do l=0,lm
               eq_work=0 ; eq_work(k,l)=1.0
               er_work=er_eq(eq_work)
               if ( mod(md(k),2) .eq. mod(lm,2) ) then
                  alu(k,lm,l) = er_work(k,jm)
               else
                  alu(k,lm-1,l) = er_work(k,jm)
               endif
            enddo
         enddo

         call ludecomp(alu,kp)
      endif

      eq_work = eq
      do k=-km,km
         if ( mod(md(k),2) .eq. mod(lm,2) ) then
            eq_work(k,lm)   = value1(k)
         else
            eq_work(k,lm-1) = value1(k)
         endif
      enddo
      eq_LaplaInv_eq = lusolve(alu,kp,eq_work)

    end function eq_LaplaInv_eq

    function eq_Vor2Strm_eq(eq,value,cond,new)
      !
      ! ���٤���ή�������. 
      !
      ! Chebyshev-tau ˡ�ˤ��׻�
      ! ���� \zeta ��Ϳ����ή�� \psi �����.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at the boundary
      ! Ǵ����
      !    \DP{\psi}{r} = 0 at the boundary
      ! ���Ϥʤ����
      !    r\DP{}{r}(1/r\DP{\psi}{r})  = 0 at the boundary
      !
      ! l=0,lm ��ʬ�μ�������˶�������Ϳ����. 
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: eq
              !(in) ���ϱ���ʬ��

      real(8), dimension(-km:km,0:lm)             :: eq_Vor2Strm_eq
              !(out) ����ή���ؿ�ʬ��

      real(8), intent(in), optional               :: value
              ! ή��������. �����ǰ���ʤΤ��ȿ� 0 ��ʬ�Τ�

      character(len=1), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'R'
              !     R    : ��¦Ǵ����
              !     F    : ��¦���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu, alub
      integer, dimension(:,:), allocatable    :: kp, kpb

      real(8), dimension(-km:km,0:lm)         :: eq_work
      real(8), dimension(-km:km,jm)           :: er_work
      real(8)                                 :: value1          ! ������
      logical                                 :: rigid

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k, l, ll
      save    :: alu, kp, first
      save    :: alub, kpb

      if (.not. present(value)) then
         value1 = 0
      else
         value1 = value
      endif

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','eq_Vor2Strm_eq','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         if ( allocated(alub) ) deallocate(alub)
         if ( allocated(kpb) ) deallocate(kpb)
         allocate(alub(-km:km,0:lm,0:lm),kpb(-km:km,0:lm))

         ! �������
         do l=0,lm
            eq_work = 0.0 ; eq_work(:,l) = 1.0
            alu(:,:,l) = eq_er(er_Lapla_eq(eq_work))
         enddo

         ! 0 ��ʬ�ΤȤ���� 1 ������.
         do k=-km,km
            do l=0,md(k)-1
               alu(k,l,l) = 1.0D0
            enddo
            do l=md(k)+1,lm,2
               alu(k,l,l) = 1.0D0
            enddo
         enddo
         
         ! alu(:,:,nd(k)) ��� 0 �ʤΤ� 1 �򤤤�Ƥ���. 
         ! l=md(k) ��ʬ�϶������Ƿ���. 
         do k=-km,km
            if ( mod(md(k),2) .eq. mod(lm,2) ) then
               alu(k,lm,md(k)) = 1.0D0
            else
               alu(k,lm-1,md(k)) = 1.0D0
            endif
         enddo

         call ludecomp(alu,kp)

         !---- �������׻��ѹ��� -----
         alub = 0.0
         do l=0,lm
            alub(:,l,l) = 1.0D0
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         !     l=nd(n) ��ʬ�򶭳����Ƿ���. 
         do l=0,lm
            eq_work = 0.0 ; eq_work(:,l)=1.0D0
            er_work = er_eq(eq_work)
            do k=-km,km
               alub(k,md(k),l) = er_work(k,jm)
            enddo
         enddo

         ! �ϳ�Ū���Ǵ���� 
         !     l=lm or lm-1 ��ʬ�򶭳����Ƿ���. 
         if ( rigid ) then
            do l=0,lm
               eq_work = 0.0 ;  eq_work(:,l)=1.0D0
               er_work=er_eq(eq_RadDRad_eq(eq_work))/er_Rad
               do k=-km,km
                  if ( mod(md(k),2) .eq. mod(lm,2) ) then
                     alub(k,lm,l) = er_work(k,jm)
                  else
                     alub(k,lm-1,l) = er_work(k,jm)
                  endif
               end do
            enddo
         else
            do l=0,lm
               eq_work = 0.0 ; eq_work(:,l)=1.0D0
               er_work=er_eq(eq_RadDRad_eq(eq_RadDRad_eq(eq_work)) &
                            -2*eq_RadDRad_eq(eq_work))/er_Rad**2
               do k=-km,km
                  if ( mod(md(k),2) .eq. mod(lm,2) ) then
                     alub(k,lm,l) = er_work(k,jm)
                  else
                     alub(k,lm-1,l) = er_work(k,jm)
                  endif
               end do
            enddo
         endif

         call ludecomp(alub,kpb)

         if ( rigid ) then
            call MessageNotify('M','eq_Vor2Strm_eq',&
                              'Matrix to apply rigid b.c. newly produced.')
         else
            call MessageNotify('M','eq_Vor2Strm_eq',&
                              'Matrix to apply stress-free b.c. newly produced.')
         endif
      endif

      ! �����ΰ�׻�
      eq_work = eq

      eq_work = lusolve(alu,kp,eq_work)

      ! �������׻�
      do k=-km,km
         eq_work(k,md(k)) = 0
         if ( mod(md(k),2) .eq. mod(lm,2) ) then
            eq_work(k,lm)   = 0
         else
            eq_work(k,lm-1) = 0
         endif
      enddo
      eq_work(0,0)   = value1*2     ! ��ư��Ū���. �ȿ� 0 �ϽŤ� 1/2

      eq_Vor2Strm_eq = lusolve(alub,kpb,eq_work)

    end function eq_Vor2Strm_eq

  !--------------- ��ʬ�׻� -----------------
    function IntRadPhi_rp(rp)
      !
      ! 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� p_Phi_Weight, r_Rad_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in)  2 �����ʻ����ǡ���

      real(8)                         :: IntRadPhi_rp
      !(out) ��ʬ��

      integer :: i, j

      IntRadPhi_rp = 0.0d0
      do i=0,im-1
         do j=1,jm
            IntRadPhi_rp = IntRadPhi_rp + rp(j,i) * r_Rad_Weight(j) * p_Phi_Weight(i)
         enddo
      enddo
    end function IntRadPhi_rp

    function r_IntPhi_rp(rp)
      !
      ! 2 �����ʻ����ǡ����� Phi ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� p_Phi_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(jm)          :: r_IntPhi_rp
      !(out) ��ʬ���줿 1 ����(Rad)�ʻ����ǡ���

      integer :: i
      ! ����ѿ�

      r_IntPhi_rp = 0.0d0
      do i=0,im-1
         r_IntPhi_rp(:) = r_IntPhi_rp(:) + rp(:,i) * p_Phi_Weight(i)
      enddo
    end function r_IntPhi_rp

    function p_IntRad_rp(rp)
      !
      ! 2 �����ʻ����ǡ����� Rad ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� r_Rad_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in)  2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)      :: p_IntRad_rp
      !(out) ��ʬ���줿 1 ����(Phi)�ʻ����ǡ���

      integer :: j
      ! ����ѿ�

      p_IntRad_rp = 0.0d0
      do j=1,jm
         p_IntRad_rp(:) = p_IntRad_rp(:) + rp(j,:) * r_Rad_Weight(j)
      enddo
    end function p_IntRad_rp

    function IntPhi_p(p)
      !
      ! 1 ����(Phi)�ʻ����ǡ����� Phi ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� p_Phi_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:im-1)   :: p         !(in)  1 �����ʻ����ǡ���
      real(8)                      :: IntPhi_p    !(out) ��ʬ��

      IntPhi_p = sum(p*p_Phi_Weight)
    end function IntPhi_p

    function IntRad_r(r) 
      !
      ! 1 ����(Rad)�ʻ����ǡ����� Rad ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� r_Rad_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(jm)   :: r          !(in)  1 �����ʻ����ǡ���
      real(8)                  :: IntRad_r     !(out) ��ʬ��

      IntRad_r = sum(r*r_Rad_Weight)
    end function IntRad_r

  !--------------- ʿ�ѷ׻� -----------------
    function AvrRadPhi_rp(rp)
      !
      ! 2 �����ʻ����ǡ��������ΰ�ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� p_Phi_Weight, r_Rad_Weight �򤫤���
      ! ���¤�׻���, p_Phi_Weight*r_Rad_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in)  2 �����ʻ����ǡ���

      real(8)                         :: AvrRadPhi_rp
      !(out) ʿ����

      AvrRadPhi_rp = IntRadPhi_rp(rp)/(sum(p_Phi_weight)*sum(r_Rad_weight))
    end function AvrRadPhi_rp

    function r_AvrPhi_rp(rp)
      !
      ! 2 �����ʻ����ǡ����� Phi ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� p_Phi_Weight �򤫤������¤�׻���, 
      ! p_Phi_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(jm)          :: r_AvrPhi_rp
      !(out) ʿ�Ѥ��줿 1 ����(Rad)�ʻ���

      r_AvrPhi_rp = r_IntPhi_rp(rp)/sum(p_Phi_weight)
    end function r_AvrPhi_rp

    function p_AvrRad_rp(rp)
      !
      ! 2 �����ʻ����ǡ����� Rad ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� r_Rad_Weight �򤫤������¤�׻���, 
      ! r_Rad_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)      :: p_AvrRad_rp
      !(out) ʿ�Ѥ��줿 1 ����(Phi)�ʻ���

      p_AvrRad_rp = p_IntRad_rp(rp)/sum(r_Rad_weight)
    end function p_AvrRad_rp

    function AvrPhi_p(p)
      !
      ! 1 ����(Phi)�ʻ����ǡ����� Phi ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� p_Phi_Weight �򤫤������¤�׻���, 
      ! p_Phi_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:im-1)   :: p          !(in)  1 �����ʻ����ǡ���
      real(8)                      :: AvrPhi_p     !(out) ʿ����

      AvrPhi_p = IntPhi_p(p)/sum(p_Phi_weight)
    end function AvrPhi_p

    function AvrRad_r(r)
      !
      ! 1 ����(Rad)�ʻ����ǡ����� Rad ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� r_Rad_Weight �򤫤������¤�׻���, 
      ! r_Rad_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(jm)   :: r            !(in)  1 �����ʻ����ǡ���
      real(8)                  :: AvrRad_r     !(out) ʿ����

      AvrRad_r = IntRad_r(r)/sum(r_Rad_weight)
    end function AvrRad_r

end module eq_module
