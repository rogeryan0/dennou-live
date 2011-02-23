!--
!----------------------------------------------------------------------
! Copyright(c) 2008-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wtu_module
!
!    spml/wtu_module �⥸�塼��ϵ�Ȥ��γ�¦�ε���ΰ�Ǥ�ή�α�ư��
!    ���ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤���
!    ��ΤǤ���. 
!
!    ��ʿ�����˵���Ĵ��ȡ���Ѵ������ư�������˥����ӥ������Ѵ����Ѥ���
!    ���Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
!
!    ������ wt_module, wu_module ���Ѥ��Ƥ���. �ǲ����Ǥϵ���Ĵ���Ѵ�
!    ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
!    ���֥롼������Ѥ��Ƥ���.
!
!
!����  2008/01/12  �ݹ�����  wt_module, wu_module ������
!      2008/07/04  ��������ʿ 0 -> 0.0D0, 1 -> 1.0D0 �˽���
!      2008/07/05  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/01/09  �ݹ�����  wtu_Initial ��å���������
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/07/31  �ݹ�����  �������׻�������� threadprivate ����(OpenMP)
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!����
!      �ǡ�������� index
!        x : ����         y : ����    z : ư��(�����)    r : ư��(����)
!        w : ����Ĵ�´ؿ����ڥ��ȥ�
!        n : ����Ĵ�´ؿ����ڥ��ȥ�(��ʿ���ȿ�)
!        m : ����Ĵ�´ؿ����ڥ��ȥ�(�Ӿ��ȿ�)
!        t : �����ӥ����մؿ����ڥ��ȥ�(�����)
!        u : �����ӥ����մؿ����ڥ��ȥ�(����)
!        a : Ǥ�դμ���
!
!        xyz : ����� 3 �����ʻ����ǡ���
!        xyr : ����� 3 �����ʻ����ǡ���
!        xy  : ��ʿ 2 �����ʻ����ǡ���
!        yr  : �Ҹ��� 2 �����ʻ����ǡ���
!        xr  : ������ 2 �����ʻ����ǡ���
!
!        wz  : ��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���(�����)
!        wr  : ��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���(����)
!        wt  : ���ڥ��ȥ�ǡ���(�����)
!        wu  : ���ڥ��ȥ�ǡ���(����)
!
!++
module wtu_module
  !
  != wtu_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wtu_module.f90,v 1.7 2010-03-02 10:09:07 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wtu_module �⥸�塼��ϵ�Ȥ��γ�¦�ε���ΰ�Ǥ�ή�α�ư��
  ! ���ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤���
  ! ��ΤǤ���. 
  !
  ! ��ʿ�����˵���Ĵ��ȡ���Ѵ������ư�������˥����ӥ������Ѵ����Ѥ���
  ! ���Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
  !
  ! ������ wt_module, wu_module ���Ѥ��Ƥ���. �ǲ����Ǥϵ���Ĵ���Ѵ�
  ! ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
  ! ���֥롼������Ѥ��Ƥ���.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ
  !   (wu_, nmr_, nr_, xyr_, wr_, w_, xy_, x_, y_, r_, a_, wt_, nmz_, nz_, 
  !   xyz_, wz_, w_, xy_, x_, y_, z_, a_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   wt_  :: ���ڥ��ȥ�ǡ���(����Ĵ��ȡ���������ӥ������Ѵ�-���)
  !   nmz_ :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, �Ӿ��ȿ�����ʬ, ư��-���)
  !   nz_  :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, ư��-���)
  !   xyz_ :: 3 �����ʻ����ǡ���(���١����١�ư��-���)
  !   wz_  :: ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���, ���
  !   wu_  :: ���ڥ��ȥ�ǡ���(����Ĵ��ȡ���������ӥ������Ѵ�-��)
  !   nmr_ :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, �Ӿ��ȿ�����ʬ, ư��-��)
  !   nr_  :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, ư��-��)
  !   xyr_ :: 3 �����ʻ����ǡ���(���١����١�ư��-��)
  !   wr_  :: ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���, ��
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   ��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ�
  !   (wt_, xyz_, wz_, w_, xy_, x_, y_, z_, a_,
  !   wu_, xyz_, wr_, w_, xy_, x_, y_, r_, a_) ��, �����ѿ��η���
  !   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _wt      :: ���ڥ��ȥ�ǡ���
  !   _xyz     :: 3 �����ʻ����ǡ���
  !   _xyz_xyz :: 2 �Ĥ�3 �����ʻ����ǡ���, ...
  !   _wu      :: ���ڥ��ȥ�ǡ���
  !   _xyr     :: 3 �����ʻ����ǡ���
  !   _xyr_xyr :: 2 �Ĥ�3 �����ʻ����ǡ���, ...
  !
  !=== �ƥǡ����μ��������
  !
  !
  ! * xyz : 3 �����ʻ����ǡ���(���١����١�ư��-���)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km �Ϥ��줾�����, ����, ư�º�ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  ! 
  ! * wt : ���ڥ��ȥ�ǡ���-���
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ�, lm �ϥ����ӥ�����¿�༰�κ��缡��
  !     �Ǥ���, ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ��ʿ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�Ĵ�٤�
  !     ���Ȥ��Ǥ���.
  ! 
  ! * nmz : ��ʿ���ڥ��ȥ�ǡ������¤�� 3 ��������(���).
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ�, �� 3 ������ư�º�ɸ��ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wt_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  ! 
  ! * nz : ���ڥ��ȥ�ǡ������¤�� 2 ��������(���).
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��. nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  ! 
  ! * wz : ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���(���).
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:km).
  ! 
  ! * xyr : 3 �����ʻ����ǡ���(���١����١�ư��-��)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km �Ϥ��줾�����, ����, ư�º�ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  ! 
  ! * wu : ���ڥ��ȥ�ǡ���(��)
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ�, lm �ϥ����ӥ�����¿�༰�κ��缡��
  !     �Ǥ���, ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ��ʿ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�Ĵ�٤�
  !     ���Ȥ��Ǥ���.
  ! 
  ! * nmr : ��ʿ���ڥ��ȥ�ǡ������¤�� 3 ��������(��).
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ�, �� 3 ������ư�º�ɸ��ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wt_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  ! 
  ! * nr : ���ڥ��ȥ�ǡ������¤�� 2 ��������(��).
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��. nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  ! 
  ! * wr : ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���(��).
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:km).
  ! 
  ! * wt_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  ! 
  ! * xyz_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  ! 
  ! * wz_ �ǻϤޤ�ؿ����֤��ͤϿ�ʿ���ڥ��ȥ�, ư�³ʻ����ǡ�����Ʊ��.
  ! 
  ! * wu_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  ! 
  ! * xyr_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  ! 
  ! * wr_ �ǻϤޤ�ؿ����֤��ͤϿ�ʿ���ڥ��ȥ�, ư�³ʻ����ǡ�����Ʊ��.
  ! 
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  ! 
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! wtu_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_Lon, y_Lat               :: �ʻ�����ɸ(����, ����)���Ǽ����1 ��������
  ! z_Rad                      :: �ʻ�����ɸ(ư��, ���)���Ǽ����1 ��������
  ! r_Rad                      :: �ʻ�����ɸ(ư��, ��)���Ǽ����1 ��������
  ! x_Lon_Weight, y_Lat_Weight :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! z_Rad_Weight               :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! r_Rad_Weight               :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! xyz_Lon, xyz_Lat           :: �ʻ����ǡ����η��١�����(X,Y) 
  !                               (�ʻ����ǡ����� 3 ��������)
  ! xyz_Rad                    :: �ʻ����ǡ�����ư��(���)(Z) 
  !                               (�ʻ����ǡ����� 3 ��������)
  ! xyr_Lon, xyr_Lat           :: �ʻ����ǡ����η��١�����(X,Y)
  !                               (�ʻ����ǡ����� 3 ��������)
  ! xyr_Rad                    :: �ʻ����ǡ�����ư��(��) 
  !                               (�ʻ����ǡ����� 3 ��������)
  !
  !==== �����Ѵ�
  !
  ! xyz_wt, wt_xyz :: ���ڥ��ȥ�ǡ����� 3 �����ʻҥǡ����δ֤��Ѵ�
  !                   (����Ĵ��ȡ��, �����ӥ������Ѵ�)
  ! xyz_wz, wz_xyz :: 3 �����ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (����Ĵ��ȡ��)
  ! wz_wt, wt_wz   :: ���ڥ��ȥ�ǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (�����ӥ������Ѵ�)
  ! xyr_wu, wu_xyr :: ���ڥ��ȥ�ǡ����� 3 �����ʻҥǡ�����
  !                   �֤��Ѵ� (����Ĵ��ȡ��, �����ӥ������Ѵ�)
  ! xyr_wr, wr_xyr :: 3 �����ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (����Ĵ��ȡ��)
  ! wr_wu, wu_wr   :: ���ڥ��ȥ�ǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (�����ӥ������Ѵ�)
  ! w_xy, xy_w     :: ���ڥ��ȥ�ǡ����� 2 ������ʿ�ʻҥǡ�����
  !                   �֤��Ѵ�(����Ĵ��ȡ���Ѵ�) 
  ! l_nm, nm_l     :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! wt_DRad_wt          :: ���ڥ��ȥ�ǡ�����ư����ʬ
  !                        ��/��r ����Ѥ�����
  ! wt_DivRad_wt        :: ���ڥ��ȥ�ǡ�����ȯ����ư����ʬ
  !                        1/r^2 ��/��r r^2 = ��/��r + 2/r ����Ѥ�����
  ! wt_RotRad_wt        :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ
  !                        1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  ! wt_Lapla_wt         :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! xyz_GradLon_wt      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! xyz_GradLat_wt      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/r����/�ߦդ���Ѥ�����
  ! wt_DivLon_xyz       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! wt_DivLat_xyz       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! wt_Div_xyz_xyz_xyz  :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyz_Div_xyz_xyz_xyz :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyz_RotLon_wt_wt    :: �٥��ȥ��β�ž�η�����ʬ��׻�����
  ! xyz_RotLat_wt_wt    :: �٥��ȥ��β�ž�ΰ�����ʬ��׻�����
  ! wt_RotRad_xyz_xyz   :: �٥��ȥ��β�ž��ư����ʬ��׻�����
  ! wr_DRad_wu          :: ���ڥ��ȥ�ǡ�����ư����ʬ
  !                        ��/��r ����Ѥ�����
  ! wr_DRad2_wu         :: ���ڥ��ȥ�ǡ�����ư����ʬ
  !                        ��^2/��r^2 ����Ѥ�����
  ! wr_DivRad_wu        :: ���ڥ��ȥ�ǡ�����ȯ����ư����ʬ
  !                        1/r^2 ��/��r r^2 = ��/��r + 2/r ����Ѥ�����
  ! wr_RotRad_wu        :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ
  !                        1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  ! wr_Lapla_wu         :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! xyr_GradLon_wu      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! xyr_GradLat_wu      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/r����/�ߦդ���Ѥ�����
  ! wr_DivLon_xyr       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! wr_DivLat_xyr       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! wr_Div_xyr_xyr_xyr  :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyr_Div_xyr_xyr_xyr :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyr_RotLon_wu_wu    :: �٥��ȥ��β�ž�η�����ʬ��׻�����
  ! xyr_RotLat_wu_wu    :: �٥��ȥ��β�ž�ΰ�����ʬ��׻�����
  ! wr_RotRad_xyr_xyr   :: �٥��ȥ��β�ž��ư����ʬ��׻�����
  !
  !==== �ȥ�����ݥ�����׻�����ʬ
  !
  ! wt_KxRGrad_wt            :: ���ڥ��ȥ�ǡ����˷�����ʬ
  !                             k��r���� = ��/�ߦˤ���Ѥ�����
  ! xyz_KGrad_wt             :: ���ڥ��ȥ�ǡ����˼�������ʬ
  !                             k���� = cos��/r ��/�ߦ� + sin�բ�/��r ��
  !                             ���Ѥ�����
  ! wt_L2_wt                 :: ���ڥ��ȥ�ǡ�����
  !                             L2 �黻�� = -��ʿ��ץ饷�����
  !                             ���Ѥ�����
  ! wt_L2Inv_wt              :: ���ڥ��ȥ�ǡ����� 
  !                             L2 �黻�Ҥε� = -�տ�ʿ��ץ饷�����
  !                             ���Ѥ�����
  ! wt_QOperator_wt          :: ���ڥ��ȥ�ǡ����˱黻��
  !                             Q=(k����-1/2(L2 k����+ k����L2)) ��
  !                             ���Ѥ�����
  ! wt_RadRot_xyz_xyz        :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������
  !                             r��(����v) ��׻�����
  ! wt_RadRotRot_xyz_xyz_xyz :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����
  ! wt_Potential2Vector      :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��
  !                             �٥��ȥ���׻�����
  ! wt_Potential2Rotation    :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�����
  !                             ��ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����
  ! wu_L2_wu                 :: ���ڥ��ȥ�ǡ�����
  !                             L2 �黻�� = -��ʿ��ץ饷�������Ѥ�����
  ! wu_L2Inv_wu              :: ���ڥ��ȥ�ǡ�����
  !                             L2 �黻�Ҥε� = -�տ�ʿ��ץ饷�����
  !                             ���Ѥ�����
  ! wu_QOperator_wu          :: ���ڥ��ȥ�ǡ����˱黻��
  !                             Q=(k����-1/2(L2 k����+ k����L2)) ��
  !                             ���Ѥ�����
  ! wu_RadRot_xyr_xyr        :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������
  !                             r��(����v) ��׻�����
  ! wr_RadRotRot_xyr_xyr_xyr :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����
  ! wu_Potential2Vector      :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��
  !                             �٥��ȥ���׻�����
  ! wu_Potential2Rotation    :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�����
  !                             ��ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����
  !
  !==== �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ����
  !
  ! nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt   ::
  !     �ȥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  ! nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt   :: 
  !     �ݥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  ! nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu   :: 
  !     �ȥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  ! nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu   :: 
  !     �ݥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  !
  !==== ����������
  !
  ! wt_BoundariesTau, wt_BoundariesGrid, wt_Boundaries                   ::
  !     ����ˡ, ����ˡ
  ! wt_TorBoundariesTau, wt_TorBoundariesGrid, wt_TorBoundaries          :: 
  !     ®�٥ȥ�����ݥƥ󥷥��ζ����������ΰ��Ŭ�Ѥ���
  !     (����ˡ,����ˡ)
  ! wz_LaplaPol2Pol_wz, wt_LaplaPol2Pol_wt                               :: 
  !     ����ΰ��®�٥ݥ�����ݥƥ󥷥�릵��^2���������
  !     (�����Ϥ����줾������ӥ����ճʻ���,�����ӥ����շ���)
  ! wt_TorMagBoundariesTau, wt_TorMagBoundariesGrid, wt_TorMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥��ζ����������ΰ��Ŭ�Ѥ���
  !     (����ˡ, ����ˡ)
  ! wt_PolMagBoundariesTau, wt_PolMagBoundariesGrid, wt_PolMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ����������ΰ��Ŭ�Ѥ���
  !     (����ˡ, ����ˡ)
  ! wu_BoundaryTau, wu_BoundaryGrid, wu_Boundary                         ::
  !     ���ΰ�˥ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ�Ѥ��� (����ˡ, ����ˡ)
  ! wu_TorBoundaryTau, wu_TorBoundaryGrid, wu_TorBoundary                ::
  !     ���ΰ��®�٥ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ,����ˡ)
  ! wr_LaplaPol2Pol_wr, wu_LaplaPol2Pol_wu                               ::
  !     ���ΰ��®�٥ݥ�����ݥƥ󥷥�릵��^2���������
  !     (�����Ϥ����줾������ӥ����ճʻ���,�����ӥ����շ���)
  ! wu_TorMagBoundaryTau, wu_TorMagBoundaryGrid, wu_TorMagBoundary       ::
  !    ����ȥ�����ݥƥ󥷥��ζ���������ΰ��Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wu_PolMagBoundaryTau, wu_PolMagBoundaryGrid, wu_PolMagBoundary       ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ���������ΰ��Ŭ�Ѥ���
  !     (����ˡ, ����ˡ)
  ! wtu_TormagBoundariesTau, wtu_TormagBoundariesGrid                    ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ�������太��ӵ���ΰ����Τ�
  !     Ŭ�Ѥ���(����ˡ, ����ˡ)
  !
  !==== ��ʬ��ʿ��(3 �����ǡ���)
  !
  ! IntLonLatRad_xyz, AvrLonLatRad_xyz :: 3 �����ʻ����ǡ�����
  !                                       ���ΰ���ʬ�����ʿ��
  ! z_IntLonLat_xyz, z_AvrLonLat_xyz   :: 3 �����ʻ����ǡ�����
  !                                       ���ٷ���(��ʿ������)��ʬ�����ʿ��
  ! y_IntLonRad_xyz, y_AvrLonRad_xyz   :: 3 �����ʻ����ǡ�����
  !                                       ����ư����ʬ�����ʿ��
  ! z_IntLatRad_xyz, z_AvrLatRad_xyz   :: 3 �����ʻ����ǡ�����
  !                                       ����ư��(�Ҹ���)��ʬ�����ʿ��
  ! yz_IntLon_xyz, yz_AvrLon_xyz       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntLat_xyz, xz_AvrLat_xyz       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntRad_xyz, xz_AvrRad_xyz       :: 3 �����ʻ����ǡ�����
  !                                       ư��������ʬ�����ʿ��
  ! IntLonLatRad_xyr, AvrLonLatRad_xyr :: 3 �����ʻ����ǡ�����
  !                                       ���ΰ���ʬ�����ʿ��
  ! z_IntLonLat_xyr, z_AvrLonLat_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ���ٷ���(��ʿ������)��ʬ�����ʿ��
  ! y_IntLonRad_xyr, y_AvrLonRad_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ����ư����ʬ�����ʿ��
  ! z_IntLatRad_xyr, z_AvrLatRad_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ����ư��(�Ҹ���)��ʬ�����ʿ��
  ! yz_IntLon_xyr, yz_AvrLon_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntLat_xyr, xz_AvrLat_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntRad_xyr, xz_AvrRad_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ư��������ʬ�����ʿ��
  !
  !==== ��ʬ��ʿ��(2 �����ǡ���)
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 �����ʻ����ǡ����ο�ʿ(����)��ʬ�����ʿ��
  !
  ! IntLonRad_xz, AvrLonRad_xz :: 2 ����(XZ)�ʻ����ǡ����η���ư����ʬ
  !                               �����ʿ��
  ! IntLatRad_yz, AvrLatRad_yz :: 2 ����(YZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)
  !                               ��ʬ�����ʿ�� 
  ! y_IntLon_xy, y_AvrLon_xy   :: ��ʿ 2 ����(����)�ʻ����ǡ����η�������
  !                               ��ʬ�����ʿ��
  ! x_IntLat_xy, x_AvrLat_xy   :: ��ʿ2 ����(����)�ʻ����ǡ����ΰ���������ʬ
  !                               �����ʿ��
  ! z_IntLon_xz, z_AvrLon_xz   :: 2 ����(XZ)�ʻ����ǡ����η���������ʬ�����
  !                               ʿ��
  ! x_IntRad_xz, x_AvrRad_xz   :: 2 ����(XZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��
  ! z_IntLat_yz, z_AvrLat_yz   :: 2 ����(YZ)�ʻ����ǡ����ΰ���������ʬ�����
  !                               ʿ��
  ! y_IntRad_yz, y_AvrRad_yz   :: 2 ����(YZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��                  
  ! IntLonRad_xr, AvrLonRad_xr :: 2 ����(XZ)�ʻ����ǡ����η���ư����ʬ
  !                               �����ʿ��
  ! IntLatRad_yr, AvrLatRad_yr :: 2 ����(YZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)
  !                               ��ʬ�����ʿ�� 
  ! r_IntLon_xr, r_AvrLon_xr   :: 2 ����(XZ)�ʻ����ǡ����η���������ʬ�����
  !                               ʿ��
  ! x_IntRad_xr, x_AvrRad_xr   :: 2 ����(XZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��
  ! r_IntLat_yr, r_AvrLat_yr   :: 2 ����(YZ)�ʻ����ǡ����ΰ���������ʬ�����
  !                               ʿ��
  ! y_IntRad_yr, y_AvrRad_yr   :: 2 ����(YZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��                  
  !
  !==== ��ʬ��ʿ��(1 �����ǡ���)
  !
  ! IntLon_x, AvrLon_x  :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��
  ! IntLat_y, AvrLat_y  :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! IntRad_z, AvrRad_z  :: 1 ����(Z)�ʻ����ǡ�����ư��������ʬ�����ʿ��
  ! IntRad_r, AvrRad_r  :: 1 ����(Z)�ʻ����ǡ�����ư��������ʬ�����ʿ��
  !
  ! 
  use dc_message
  use lumatrix
  use wt_module
  use wu_module, only : wu_initial, &
       r_Rad, r_Rad_Weight, xyr_Lon, xyr_Lat, xyr_Rad, wr_Rad, wu_VMiss, &
       wr_DRad_wu, r_DRad_t, wr_wu, wu_wr, wr_DRad2_wu, r_DRad2_t, &
       xyr_wu, wu_xyr, xyr_wr, wr_xyr, &
       wr_DivRad_wu, wr_RotRad_wu, wr_Lapla_wu, &
       xyr_GradLon_wu, xyr_gradlat_wu, wr_DivLon_xyr, wr_DivLat_xyr, &
       wr_Div_xyr_xyr_xyr, xyr_Div_xyr_xyr_xyr, &
       xyr_RotLon_wu_wu, xyr_RotLat_wu_wu, wr_RotRad_xyr_xyr, &
       yr_IntLon_xyr, xr_IntLat_xyr, xy_IntRad_xyr, &
       x_IntLatRad_xyr, y_IntLonRad_xyr, r_IntLonLat_xyr, &
       IntLonLatRad_xyr, &
       r_IntLat_yr, y_IntRad_yr, IntLatRad_yr, &
       r_IntLon_xr, x_IntRad_xr, IntLonRad_xr, &
       IntRad_r, &
       yr_AvrLon_xyr, xr_AvrLat_xyr, xy_AvrRad_xyr, &
       x_AvrLatRad_xyr, y_AvrLonRad_xyr, r_AvrLonLat_xyr, &
       AvrLonLatRad_xyr, &
       r_AvrLat_yr, y_AvrRad_yr, AvrLatRad_yr, &
       r_AvrLon_xr, x_AvrRad_xr, AvrLonRad_xr, &
       AvrLon_x, AvrLat_y, AvrRad_r, &
       wu_KxRGrad_wu, xyr_KGrad_wu, wu_L2_wu, wu_L2Inv_wu, wu_QOperator_wu, &
       wu_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr, &
       wu_Potential2vector, wu_Potential2Rotation, &
       nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu,&
       nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu, &
       wu_Boundary, wu_TorBoundary, wr_LaplaPol2Pol_wr, &
       wu_TormagBoundary, wu_PolmagBoundary, &
       wu_BoundaryTau, wu_TorBoundaryTau, &
       wu_TormagBoundaryTau, wu_PolmagBoundaryTau, &
       wu_BoundaryGrid, wu_TorBoundaryGrid, wu_LaplaPol2PolGrid_wu, &
       wu_TormagBoundaryGrid, wu_PolmagBoundaryGrid

  implicit none
  private

  public wtu_Initial

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

  public Interpolate_wt

  public nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt
  public nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt

  public wt_Boundaries, wt_TorBoundaries, wz_LaplaPol2Pol_wz
  public wt_TormagBoundaries, wt_PolmagBoundaries

  public wt_BoundariesTau, wt_TorBoundariesTau
  public wt_TormagBoundariesTau, wt_PolmagBoundariesTau

  public wt_BoundariesGrid, wt_TorBoundariesGrid, wt_LaplaPol2PolGrid_wt
  public wt_TormagBoundariesGrid, wt_PolmagBoundariesGrid

  public r_Rad, r_Rad_Weight
  public xyr_Lon, xyr_Lat, xyr_Rad
  public wr_Rad
  public wu_VMiss

  public wr_DRad_wu, r_DRad_t, wr_wu, wu_wr
  public wr_DRad2_wu, r_DRad2_t
  public xyr_wu, wu_xyr, xyr_wr, wr_xyr
  public wr_DivRad_wu, wr_RotRad_wu, wr_Lapla_wu
  public xyr_GradLon_wu, xyr_gradlat_wu
  public wr_DivLon_xyr, wr_DivLat_xyr
  public wr_Div_xyr_xyr_xyr, xyr_Div_xyr_xyr_xyr
  public xyr_RotLon_wu_wu, xyr_RotLat_wu_wu, wr_RotRad_xyr_xyr

  public yr_IntLon_xyr, xr_IntLat_xyr, xy_IntRad_xyr
  public x_IntLatRad_xyr, y_IntLonRad_xyr, r_IntLonLat_xyr
  public IntLonLatRad_xyr

  public r_IntLat_yr, y_IntRad_yr, IntLatRad_yr
  public r_IntLon_xr, x_IntRad_xr, IntLonRad_xr
  public IntRad_r

  public yr_AvrLon_xyr, xr_AvrLat_xyr, xy_AvrRad_xyr
  public x_AvrLatRad_xyr, y_AvrLonRad_xyr, r_AvrLonLat_xyr
  public AvrLonLatRad_xyr

  public r_AvrLat_yr, y_AvrRad_yr, AvrLatRad_yr
  public r_AvrLon_xr, x_AvrRad_xr, AvrLonRad_xr
  public AvrRad_r

  public wu_KxRGrad_wu, xyr_KGrad_wu, wu_L2_wu, wu_L2Inv_wu, wu_QOperator_wu
  public wu_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr
  public wu_Potential2vector, wu_Potential2Rotation

  public nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu
  public nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu

  public wu_Boundary, wu_TorBoundary, wr_LaplaPol2Pol_wr
  public wu_TormagBoundary, wu_PolmagBoundary

  public wu_BoundaryTau, wu_TorBoundaryTau
  public wu_TormagBoundaryTau, wu_PolmagBoundaryTau

  public wu_BoundaryGrid, wu_TorBoundaryGrid, wu_LaplaPol2PolGrid_wu
  public wu_TormagBoundaryGrid, wu_PolmagBoundaryGrid

  public wtu_TormagBoundaries, wtu_TormagBoundariesTau, wtu_TormagBoundariesGrid
  public wtu_PolmagBoundaries, wtu_PolmagBoundariesTau, wtu_PolmagBoundariesGrid

  interface wtu_TorMagBoundaries
     module procedure wtu_TorMagBoundariesTau
  end interface

  interface wtu_PolMagBoundaries
     module procedure wtu_PolMagBoundariesTau
  end interface

  integer            :: im=64, jm=32        ! ��ʿ�ʻ���������(����, ����)
  integer            :: nm=21               ! ��ʿ�����ȿ�������
  integer            :: kmo=16, kmi=16      ! ��ľ�ʻ���������(���, ��)
  integer            :: lmo=16, lmi=16      ! ��ľ�����ȿ�������(���, ��)  
                                     
  real(8)            :: ri=1.0D0, ro=2.0D0  ! �⳰��Ⱦ��
  real(8), parameter :: pi=3.1415926535897932385D0

  save im, jm, kmo, kmi, nm, lmo, lmi, ri, ro

  contains
  !--------------- ����� -----------------
   subroutine wtu_Initial(i,j,ki,ko,n,li,lo,r_in,r_out,np)
     !
     ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �太��ӵ��ư�º�ɸ���ϰϤ����ꤹ��.
     !
     ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
     ! ���ʤ���Фʤ�ʤ�. 
     !
     ! np �� 1 ����礭���ͤ���ꤹ��� ISPACK �ε���Ĵ��ȡ���Ѵ� 
     ! OPENMP ����׻��롼�����Ѥ�����. ����׻���¹Ԥ���ˤ�, 
     ! �¹Ի��˴Ķ��ѿ� OMP_NUM_THREADS �� np �ʲ��ο��������ꤹ������
     ! �����ƥ�˱�����������ɬ�פȤʤ�. 
     !
     ! np �� 1 ����礭���ͤ���ꤷ�ʤ��������׻��롼����ϸƤФ�ʤ�.
     !
     !
     integer,intent(in) :: i              ! �ʻ�����(���٦�)
     integer,intent(in) :: j              ! �ʻ�����(���٦�)
     integer,intent(in) :: ki             ! �ʻ�����(ư�� r)
     integer,intent(in) :: ko             ! �ʻ�����(ư�� z)
     integer,intent(in) :: n              ! �����ȿ�(��ʿ���ȿ�)
     integer,intent(in) :: li             ! �����ȿ�(��ư���ȿ�)
     integer,intent(in) :: lo             ! �����ȿ�(���ư���ȿ�)

     real(8),intent(in) :: r_in, r_out    ! ��Ⱦ��

     integer,intent(in), optional :: np   ! OPENMP �Ǥκ��祹��åɿ�

     im = i  ; jm = j ; kmo = ko ; kmi = ki
     nm = n  ; lmo = lo ; lmi=li
     ri = r_in ; ro = r_out

     if ( present(np) ) then
        if ( kmo > kmi ) then
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro,np)
           call wu_Initial(im,jm,kmi,nm,lmi,ri,np,wa_init=.false.)
        else
           call wu_Initial(im,jm,kmi,nm,lmi,ri,np)
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro,np,wa_init=.false.)
        endif
     else
        if ( kmo > kmi ) then
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro)
           call wu_Initial(im,jm,kmi,nm,lmi,ri,wa_init=.false.)
        else
           call wu_Initial(im,jm,kmi,nm,lmi,ri)
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro,wa_init=.false.)
        endif
     endif

     call MessageNotify('M','wtu_initial','wtu_module (2009/07/31) is initialized')

   end subroutine wtu_Initial

  !--------------- ���������� -----------------
    subroutine wtu_TormagBoundariesTau(wt_TOR,wu_TOR,Pmo,Pmi,new)
      !
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ���¦����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    Psi_o = 0   at the outer boundary
      ! ��--��̶���
      !    Psi_o = Psi_i, Pm_o DrDPsi_o = Pm_i DrDPsi_i    at the boundary
      ! 
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8),intent(in)           :: Pmo
              !(in) ��̤μ����ץ��ɥ��

      real(8),intent(in)           :: Pmi
              !(in) ���μ����ץ��ɥ��

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lmo+lmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo+lmi+1
            wt_I = 0.0D0
            alu(:,l,l) = 1.0D0      ! ���ȿ��Ϥ��Τޤ�
         enddo

         ! ��--��̶���
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,lmo-1,l)   = wz_PSI(:,0)
            alu(:,lmo,l)   = Pmo * wz_DPSIDR(:,kmo) 
            alu(:,lmo+lmi+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,lmo,lmo+1+l)   = -Pmi * wr_DPSIDR(:,0) 
            alu(:,lmo+lmi+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','TormagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wtu_PSI(:,0:lmo)            = wt_TOR
      wtu_PSI(:,lmo+1:lmo+lmi+1)  = wu_TOR
      wtu_PSI(:,lmo-1)      = 0.0D0
      wtu_PSI(:,lmo)        = 0.0D0
      wtu_PSI(:,lmo+lmi+1)  = 0.0D0
      wtu_PSI = lusolve(alu,kp,wtu_PSI)

      wt_TOR = wtu_PSI(:,0:lmo)
      wu_TOR = wtu_PSI(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_TormagBoundariesTau

    subroutine wtu_TormagBoundariesGrid(wt_TOR,wu_TOR,Pmo,Pmi,new)
      !
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��.
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wtu_Initial �ˤ����ꤹ������ӥ����������ȿ�(lmo,lmi)��
      ! ��ľ�ʻ�����(kmo,kmi)�����������Ƥ���ɬ�פ�����. 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    Psi_o = 0   at the outer boundary
      ! ��--��̶���
      !    Psi_o = Psi_i, Pm_o DrDPsi_o = Pm_i DrDPsi_i    at the boundary
      ! 
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8),intent(in)           :: Pmo
              !(in) ��̤μ����ץ��ɥ��

      real(8),intent(in)           :: Pmi
              !(in) ���μ����ץ��ɥ��

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_PSI
      real(8), dimension((nm+1)*(nm+1),0:kmo+kmi+1) :: wzr_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lmo /= kmo .OR. lmi /= kmi ) then
            call MessageNotify('E','wtu_TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:kmo+kmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l)=1.0D0
            alu(:,0:kmo,l) = wz_wt(wt_I)              ! �����ΰ���ͤ��Τޤ�.
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,kmo+1:kmo+kmi+1,lmo+1+l) = wr_wu(wu_I)! �����ΰ���ͤ��Τޤ�.
         enddo

         ! ��--��̶���
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,kmo,l)   = Pmo * wz_DPSIDR(:,kmo) 
            alu(:,kmo+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,kmo,lmo+1+l)   = -Pmi * wr_DPSIDR(:,0) 
            alu(:,kmo+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','TormagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wzr_PSI(:,0:kmo)            = wz_wt(wt_TOR)
      wzr_PSI(:,kmo+1:kmo+kmi+1)  = wr_wu(wu_TOR)
      wzr_PSI(:,0)      = 0.0D0
      wzr_PSI(:,kmo)    = 0.0D0
      wzr_PSI(:,kmo+1)  = 0.0D0
      wtu_PSI = lusolve(alu,kp,wzr_PSI)

      wt_TOR = wtu_PSI(:,0:lmo)
      wu_TOR = wtu_PSI(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_TormagBoundariesGrid

    subroutine wtu_PolmagBoundariesTau(wt_Pol,wu_Pol,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ���¦����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ���ߤΤȤ���¦����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    dPol_o/dr +(n+1)/r Pol_o= 0   at the outer boundary
      ! ��--��̶���
      !    Pol_o = Phi_i, DrDPol_o = DrDPol_i    at the boundary
      ! 
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_Pol
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_Pol
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_Pol

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n,l,nn(2)
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lmo+lmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo+lmi+1
            alu(:,l,l) = 1.0d0   ! ���ȿ��Ϥ��Τޤ�.
         enddo

         ! ��̳�¦����
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,lmo-1,l) = wz_DPSIDR(n,0) + (nn(1)+1)*wz_PSI(n,0)/z_RAD(0)
            enddo
         enddo

         ! ��--��̶���
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,lmo,l)       = wz_DPSIDR(:,kmo) 
            alu(:,lmo+lmi+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,lmo,lmo+1+l)   = - wr_DPSIDR(:,0) 
            alu(:,lmo+lmi+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wtu_Pol(:,0:lmo)            = wt_Pol
      wtu_Pol(:,lmo+1:lmo+lmi+1)  = wu_Pol
      wtu_Pol(:,lmo-1)  = 0.0D0
      wtu_Pol(:,lmo)    = 0.0D0
      wtu_Pol(:,lmo+1)  = 0.0D0
      wtu_Pol = lusolve(alu,kp,wtu_Pol)

      wt_Pol = wtu_Pol(:,0:lmo)
      wu_Pol = wtu_Pol(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_PolmagBoundariesTau

    subroutine wtu_PolmagBoundariesGrid(wt_Pol,wu_Pol,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��.
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wtu_Initial �ˤ����ꤹ������ӥ����������ȿ�(lmo,lmi)��
      ! ��ľ�ʻ�����(kmo,kmi)�����������Ƥ���ɬ�פ�����. 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    dPol_o/dr +(n+1)/r Pol_o= 0   at the outer boundary
      ! ��--��̶���
      !    Pol_o = Phi_i, DrDPol_o = DrDPol_i    at the boundary
      ! 
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_Pol
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_Pol
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_Pol
      real(8), dimension((nm+1)*(nm+1),0:kmo+kmi+1) :: wzr_Pol

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n,l,nn(2)
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lmo /= kmo .OR. lmi /= kmi ) then
            call MessageNotify('E','wtu_TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:kmo+kmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l)=1.0D0
            alu(:,0:kmo,l) = wz_wt(wt_I)              ! �����ΰ���ͤ��Τޤ�.
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,kmo+1:kmo+kmi+1,lmo+1+l) = wr_wu(wu_I)! �����ΰ���ͤ��Τޤ�.
         enddo

         ! ��̳�¦����
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,0,l) = wz_DPSIDR(n,0) + (nn(1)+1) * wz_PSI(n,0)/z_RAD(0)
            enddo
         enddo

         ! ��--��̶���
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,kmo,l)   = wz_DPSIDR(:,kmo) 
            alu(:,kmo+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,kmo,lmo+1+l)   = - wr_DPSIDR(:,0) 
            alu(:,kmo+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wzr_Pol(:,0:kmo)            = wz_wt(wt_Pol)
      wzr_Pol(:,kmo+1:kmo+kmi+1)  = wr_wu(wu_Pol)
      wzr_Pol(:,0)      = 0.0D0
      wzr_Pol(:,kmo)    = 0.0D0
      wzr_Pol(:,kmo+1)  = 0.0D0
      wtu_Pol = lusolve(alu,kp,wzr_Pol)

      wt_Pol = wtu_Pol(:,0:lmo)
      wu_Pol = wtu_Pol(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_PolmagBoundariesGrid

end module wtu_module
