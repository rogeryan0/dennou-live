!--
!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wt_mpi_module
!
!    spml/wt_mpi_module �⥸�塼��ϵ��̾太��ӵ����Ǥ�ή�α�ư��
!    ���ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�äƿ��ͷ׻����뤿��� Fortran90 
!    �ؿ����󶡤����ΤǤ���. 
!
!    ��ʿ�����˵���Ĵ��ȡ���Ѵ�����Ӿ岼�ζ����ɤ򰷤������
!    �����ӥ������Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
!    �ؿ����󶡤���. 
!
!    ������ wa_mpi_module, wt_module ���Ѥ��Ƥ���. �ǲ����Ǥϵ���Ĵ���Ѵ�
!    ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
!    ���֥롼������Ѥ��Ƥ���.
!
!
!����  2002/05/19  �ݹ�����  yt_module ���⥸�塼��̾�ѹ�.
!                            �ʻ�����������̤���ɬ�פ����뤿��. 
!      2002/06/10  �ݹ�����  �ݥ����뼧�춭�����롼�����ɲ�
!      2002/11/10  �ݹ�����  �¶��֤Ǥζ������롼�����ɲ�
!      2002/11/24  �ݹ�����  �����ӥ����շ��������Ϥ�
!                            ��ץ饷����ղ򤭥롼������ɲ�
!      2002/11/28  �ݹ�����  VGradV �η׻��ѹ�.
!      2005/01/09  �ݹ�����  msgdmp -> MessageNotify ���ѹ�
!      2005/01/09  �ݹ�����  �٥��ȥ��β�ž�γ���ʬ��׻�����ؿ����ɲ�.
!      2005/02/19  �ݹ�����  �ȥ�����Ǵ�嶭�����ˤ�����®�پ��
!                             Ϳ������褦�˥��ץ������ɲ�
!      2005/02/19  �ݹ�����  �ѿ� xy_Lon, xy_Lat ���ɲ�
!      2005/03/18  �ݹ�����  ��ʬ��ʿ�Ѵؿ����ɲ�
!                            z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
!                            z_InRad_xz, y_IntRad_yz, IntLatRad_yz
!                            IntRad_z 
!                            z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
!                            z_InRad_xz, y_IntRad_yz, IntLatRad_yz
!                            IntRad_z 
!      2005/04/24  �ݹ�����  ���ڥ��ȥ���Ϸ׻��롼������ɲ�
!                            nmz_ToroidalEnergySpectrum_wt
!                            nz_ToroidalEnergySpectrum_wt
!                            nmz_PoloidalEnergySpectrum_wt
!                            nz_PoloidalEnergySpectrum_wt
!                            �����ѿ���»�� wt_VMiss ���ɲ�
!      2005/07/09  �ݹ�����  OPENMP ���Ѵ��롼������б�
!      2006/03/03  �ݹ�����  ���󥵥����ߥ�����
!                            wt_wz, nmz_ToroidalEnergySpectrum_wt,
!                            nmz_PoloidalEnergySpectrum_wt
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2006/03/19  �ݹ�����  �ѿ�����³����������򥳥��Ȥ��ɲ�
!      2007/08/11  �ݹ�����  �����ͥ롼����Υ����Ȥ�����
!      2007/09/15  �ݹ�����  ��ʬ�롼����Υ����Ȥ���
!      2007/11/02  �ݹ�����  ��ַ׻��롼�����ɲ�
!      2007/11/11  �ݹ�����  �������ؿ����֥롼����˹������ꥹ���å�Ƴ��
!      2007/11/21  �ݹ�����  ��������֥롼�����å���������
!      2008/01/07  �ݹ�����  xyz_RotLon_wt_wt, xyz_RotLat_wt_wt �Х�����
!      2008/01/13  �ݹ�����  wa_initial �����å�Ƴ��
!      2008/05/29  �ݹ�����  MPI ����
!      2008/06/11  �ݹ�����  OPENMP �����å�Ƴ��
!      2008/06/11  �ݹ�����  ����ź�����ѹ�
!      2009/02/27  ��������ʿ RDoc �ѤΥ����Ȥ���
!      2010/01/07  ��������ʿ  RDoc �ѤΥɥ�����Ƚ���, 
!                              include 'mpif.h' -> use mpi

!����
!      �ǡ�������� index
!        x : ����         y : ����    v : ����(ʬ���ʻ�)     z : ư��
!        w : ����Ĵ�´ؿ����ڥ��ȥ�
!        n : ����Ĵ�´ؿ����ڥ��ȥ�(��ʿ���ȿ�)
!        m : ����Ĵ�´ؿ����ڥ��ȥ�(�Ӿ��ȿ�)
!        t : �����ӥ����մؿ����ڥ��ȥ�
!        a : Ǥ�դμ���
!
!        xyz : 3 �����ʻ����ǡ���
!        xvz : 3 ����ʬ���ʻ����ǡ���
!        xy  : ��ʿ 2 �����ʻ����ǡ���
!        yz  : �Ҹ��� 2 �����ʻ����ǡ���
!        xz  : ������ 2 �����ʻ����ǡ���
!
!        wz  : ��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���
!        wt  : ���ڥ��ȥ�ǡ���
!
!++
module wt_mpi_module
  !
  != wt_mpi_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wt_mpi_module.f90,v 1.9 2010-02-18 15:28:27 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wt_mpi_module �⥸�塼��ϵ��̾太��ӵ����Ǥ�ή�α�ư��
  ! ���ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�äƿ��ͷ׻����뤿��� Fortran90 
  ! �ؿ����󶡤����ΤǤ���. 
  !
  ! ��ʿ�����˵���Ĵ��ȡ���Ѵ�����Ӿ岼�ζ����ɤ򰷤������
  ! �����ӥ������Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
  ! �ؿ����󶡤���. 
  !
  ! ������ wt_module, wa_mpi_module ���Ѥ��Ƥ���. �ǲ����Ǥϵ���Ĵ���Ѵ�
  ! ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
  ! ���֥롼������Ѥ��Ƥ���.
  !
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ(wt_, nmz_, nz_, xyz_, xvz_ wz_, w_, xy_, x_, y_, z_, a_)��,
  !   �֤��ͤη��򼨤��Ƥ���.
  !      wt_ :: ���ڥ��ȥ�ǡ���(����Ĵ��ȡ���������ӥ������Ѵ�)
  !      nmz_:: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, �Ӿ��ȿ�����ʬ, ư��)
  !      nz_ :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, ư��)
  !     xyz_ :: 3 �����ʻ����ǡ���(���١����١�ư��)
  !     xvz_ :: 3 ����ʬ���ʻ����ǡ���(���١����١�ư��)
  !      wz_ :: ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   ��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_wt, _xyz, _xvz, _wz_, _w, _xy, _xv, _x, _y, _z, _a) ��, 
  !   �����ѿ��η������ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !         _wt :: ���ڥ��ȥ�ǡ���
  !        _xyz :: 3 �����ʻ����ǡ���
  !    _xyz_xyz :: 2 �Ĥ�3 �����ʻ����ǡ���, ...
  !        _xvz :: 3 ����ʬ���ʻ����ǡ���
  !    _xvz_xvz :: 2 �Ĥ�3 ����ʬ���ʻ����ǡ���, ...
  !
  !=== �ƥǡ����μ��������
  !
  ! * xyz : 3 �����ʻ����ǡ���(���١����١�ư��)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,jm,0:km). 
  !   * im, jm, km �Ϥ��줾�����, ����, ư�º�ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� wt_mpi_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * xvz : 3 ����ʬ���ʻ����ǡ���(���١����١�ư��)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,jc,0:km). 
  !   * im, km �Ϥ��줾�����, ư�º�ɸ�γʻ������Ǥ���, 
  !   * jc �Ϥ��Υץ�������ͭ������ٳʻ������Ǥ���. 
  !     ���֥롼���� wt_mpi_Initial ��Ƥ֤� jc �����ꤵ���. 
  !
  ! * wt : ���ڥ��ȥ�ǡ���
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ�, lm �ϥ����ӥ�����¿�༰�κ��缡��
  !     �Ǥ���, ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ��ʿ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�Ĵ�٤�
  !     ���Ȥ��Ǥ���.
  !
  ! * nmz : ��ʿ���ڥ��ȥ�ǡ������¤�� 3 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ�, �� 3 ������ư�º�ɸ��ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wt_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * nz : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,0:km). 
  !     �� 1 ��������ʿ���ȿ���ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wz : ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:km).
  !
  ! * wt_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * xyz_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  !
  ! * xvz_ �ǻϤޤ�ؿ����֤��ͤ� 3 ����ʬ���ʻ����ǡ�����Ʊ��.
  !
  ! * wz_ �ǻϤޤ�ؿ����֤��ͤϿ�ʿ���ڥ��ȥ�, ư�³ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! wt_mpi_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_Lon, x_Lon_Weight,  ::  �ʻ�����ɸ(����)�ȽŤߤ��Ǽ���� 1 ��������
  ! y_Lat, y_Lat_Weight   ::  �ʻ�����ɸ(����)�ȽŤߤ��Ǽ���� 1 ��������
  ! v_Lat, v_Lat_Weight   ::  ʬ���ʻ�����ɸ(����)�ȽŤߤ��Ǽ���� 1 ��������
  ! z_Rad, z_Rad_Weight   ::  �ʻ�����ɸ(Ʊ��)�ȽŤߤ��Ǽ���� 1 ��������
  ! xyz_Lon, xyz_Lat, xyz_Rad :: �ʻ����ǡ����η��١����١�ư�º�ɸ(X,Y,Z) (�ʻ����ǡ����� 3 ��������)
  ! xvz_Lon, xvz_Lat, xvz_Rad :: �ʻ����ǡ����η��١����١�ư�º�ɸ(X,Y,Z) (ʬ���ʻ����ǡ����� 3 ��������)
  !
  !==== �����Ѵ�
  !
  ! xyz_wt, wt_xyz :: ���ڥ��ȥ�ǡ����� 3 �����ʻҥǡ����δ֤��Ѵ� (����Ĵ��ȡ��, �����ӥ������Ѵ�)
  !
  ! xvz_wt, wt_xvz :: ���ڥ��ȥ�ǡ����� 3 ����ʬ���ʻҥǡ����δ֤��Ѵ� (����Ĵ��ȡ��, �����ӥ������Ѵ�)
  !
  ! xyz_wz, wz_xyz :: 3 �����ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥδ֤��Ѵ� (����Ĵ��ȡ��)
  !
  ! xvz_wz, wz_xvz :: 3 ����ʬ���ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥδ֤��Ѵ� (����Ĵ��ȡ��)
  !
  ! wz_wt, wt_wz   :: ���ڥ��ȥ�ǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥδ֤��Ѵ� (�����ӥ������Ѵ�)
  !
  ! w_xy, xy_w     :: ���ڥ��ȥ�ǡ����� 2 ������ʿ�ʻҥǡ����δ֤��Ѵ�(����Ĵ��ȡ���Ѵ�) 
  ! w_xv, xv_w     :: ���ڥ��ȥ�ǡ����� 2 ������ʿʬ���ʻҥǡ����δ֤��Ѵ�(����Ĵ��ȡ���Ѵ�) 
  !
  ! az_at, at_az   :: Ʊ����ʣ���ĹԤ� (�����ӥ������Ѵ�)�ʻҥǡ����ȥ����ӥ����եǡ����δ֤��Ѵ���
  !
  ! l_nm, nm_l     :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! wt_DRad_wt     :: ���ڥ��ȥ�ǡ�����ư����ʬ��/��r ����Ѥ�����
  ! wt_DivRad_wt   :: ���ڥ��ȥ�ǡ�����ȯ����ư����ʬ 1 /r^2 ��/��r r^2 = ��/��r + 2/r ����Ѥ�����
  !
  ! wt_RotRad_wt   :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ 1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  !
  ! wt_Lapla_wt    :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  !
  ! xyz_GradLon_wt :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/rcos�ա���/�ߦˤ���Ѥ�����
  ! xvz_GradLon_wt :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/rcos�ա���/�ߦˤ���Ѥ�����
  !
  ! xyz_GradLat_wt :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/r����/�ߦդ���Ѥ�����
  ! xvz_GradLat_wt :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/r����/�ߦդ���Ѥ�����(ʬ���ʻ�)
  ! wt_DivLon_xyz  :: �ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���/�ߦˤ���Ѥ�����
  ! wt_DivLon_xvz  :: ʬ���ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���/�ߦˤ���Ѥ�����a
  
  ! wt_DivLat_xyz  :: �ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! wt_DivLat_xvz  :: ʬ���ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���(g cos��)/�ߦդ���Ѥ�����(ʬ���ʻ�)
  !
  ! wt_Div_xyz_xyz_xyz  :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����ȯ������Ѥ�����
  ! wt_Div_xvz_xvz_xvz  :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥ�ʬ���ʻҥǡ�����ȯ������Ѥ�����
  ! xyz_Div_xyz_xyz_xyz :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����ȯ������Ѥ�����
  ! xvz_Div_xvz_xvz_xvz :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥ�ʬ���ʻҥǡ�����ȯ������Ѥ�����
  ! xyz_RotLon_wt_wt  :: �٥��ȥ��β�ž�η�����ʬ��׻�����
  ! xvz_RotLon_wt_wt  :: �٥��ȥ��β�ž�η�����ʬ��׻�����(ʬ���ʻ�)
  ! xyz_RotLat_wt_wt  :: �٥��ȥ��β�ž�ΰ�����ʬ��׻�����
  ! xvz_RotLat_wt_wt  :: �٥��ȥ��β�ž�ΰ�����ʬ��׻�����(ʬ���ʻ�)
  ! wt_RotRad_xyz_xyz :: �٥��ȥ��β�ž��ư����ʬ��׻�����
  ! wt_RotRad_xvz_xvz :: �٥��ȥ��β�ž��ư����ʬ��׻�����(ʬ���ʻ�)
  !
  !==== �ȥ�����ݥ�����׻�����ʬ
  !
  ! wt_KxRGrad_wt   :: ���ڥ��ȥ�ǡ����˷�����ʬ k��r���� = ��/�ߦˤ���Ѥ�����
  !
  ! xyz_KGrad_wt    :: ���ڥ��ȥ�ǡ����˼�������ʬ k���� = cos��/r ��/�ߦ� + sin�բ�/��r ����Ѥ�����
  ! xvz_KGrad_wt    :: ���ڥ��ȥ�ǡ����˼�������ʬ k���� = cos��/r ��/�ߦ� + sin�բ�/��r ����Ѥ�����(ʬ���ʻ�)
  ! wt_L2_wt        :: ���ڥ��ȥ�ǡ����� L2 �黻�� = -��ʿ��ץ饷�������Ѥ�����
  !
  ! wt_L2Inv_wt     :: ���ڥ��ȥ�ǡ����� L2 �黻�Ҥε� = -�տ�ʿ��ץ饷�������Ѥ�����
  !
  ! wt_QOperator_wt :: ���ڥ��ȥ�ǡ����˱黻�� Q=(k����-1/2(L2 k����+ k����L2)) ����Ѥ�����
  !
  ! wt_RadRot_xyz_xyz :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������ r��(����v) ��׻�����
  ! wt_RadRot_xvz_xvz :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������ r��(����v) ��׻�����(ʬ���ʻ�)
  ! wt_RadRotRot_xyz_xyz_xyz :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����
  ! wt_RadRotRot_xvz_xvz_xvz :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����(ʬ���ʻ�)
  ! wt_Potential2Vector      :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��٥��ȥ���׻�����
  ! wt_Potential2VectorMPI   :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��٥��ȥ���׻�����(ʬ���ʻ�)
  !
  ! wt_Potential2Rotation    :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�������ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����
  ! wt_Potential2RotationMPI :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�������ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����(ʬ���ʻ�)
  !
  !
  !==== �������׻�
  !
  ! wt_VGradV    ::  �٥��ȥ� v ���� v����v ��׻�����
  ! wt_VGradVMPI ::  �٥��ȥ� v ���� v����v ��׻�����(ʬ���ʻ�)
  !
  !==== �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ����
  !
  ! nmz_ToroidalEnergySpectrum_wt, :: �ȥ�����ݥƥ󥷥�뤫�饨�ͥ륮����
  ! nz_ToroidalEnergySpectrum_wt   :: ����Ĵ��ȡ������ʬ��׻�����
  ! 
  ! nmz_PoloidalEnergySpectrum_wt, :: �ݥ�����ݥƥ󥷥�뤫�饨�ͥ륮����
  ! nz_PoloidalEnergySpectrum_wt   :: ����Ĵ��ȡ������ʬ��׻�����
  !
  !==== ����������
  !
  ! wt_BoundariesTau,    :: �ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ�Ѥ��� 
  ! wt_BoundariesGrid,   ::(����ˡ, ����ˡ)
  ! wt_Boundaries        ::
  !
  ! wt_TorBoundariesTau,  :: ®�٥ȥ�����ݥƥ󥷥��ζ�������
  ! wt_TorBoundariesGrid, :: Ŭ�Ѥ���(����ˡ,����ˡ)            ��
  ! wt_TorBoundaries      ::
  !
  ! wz_LaplaPol2Pol_wz,   :: ®�٥ݥ�����ݥƥ󥷥�릵��^2������
  ! wt_LaplaPol2Pol_wt    :: ���� (�����Ϥ����줾������ӥ����ճʻ���,
  !                       :: �����ӥ����շ���)
  !
  ! wt_TorMagBoundariesTau,  :: ����ȥ�����ݥƥ󥷥��ζ�������
  ! wt_TorMagBoundariesGrid, :: Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wt_TorMagBoundaries      ::
  !
  ! wt_PolMagBoundariesTau,  :: ����ȥ�����ݥƥ󥷥�붭���ζ�������
  ! wt_PolMagBoundariesGrid, :: Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wt_PolMagBoundaries      ::                                         
  !
  !==== ��ʬ��ʿ��(3 �����ǡ���)
  !
  ! IntLonLatRad_xyz, AvrLonLatRad_xyz :: 3 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  !
  ! z_IntLonLat_xyz, z_AvrLonLat_xyz :: 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ������)��ʬ�����ʿ��               
  !
  ! y_IntLonRad_xyz, y_AvrLonRad_xyz :: 3 �����ʻ����ǡ����ΰ���ư����ʬ�����ʿ��
  !
  ! z_IntLatRad_xyz, z_AvrLatRad_xyz :: 3 �����ʻ����ǡ����η���ư��(�Ҹ���)��ʬ�����ʿ��              
  !
  ! yz_IntLon_xyz, yz_AvrLon_xyz :: 3 �����ʻ����ǡ����η���������ʬ�����ʿ��
  ! xz_IntLat_xyz, xz_AvrLat_xyz :: 3 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! xz_IntRad_xyz, xz_AvrRad_xyz :: 3 �����ʻ����ǡ�����ư��������ʬ�����ʿ��
  !
  ! IntLonLatRad_xvz, AvrLonLatRad_xvz :: 3 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  !
  ! z_IntLonLat_xvz, z_AvrLonLat_xvz :: 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ������)��ʬ�����ʿ��               
  !
  ! v_IntLonRad_xvz, v_AvrLonRad_xvz :: 3 �����ʻ����ǡ����ΰ���ư����ʬ�����ʿ��
  !
  ! z_IntLatRad_xvz, z_AvrLatRad_xvz :: 3 �����ʻ����ǡ����η���ư��(�Ҹ���)��ʬ�����ʿ��              
  !
  ! vz_IntLon_xvz, vz_AvrLon_xvz :: 3 �����ʻ����ǡ����η���������ʬ�����ʿ��
  ! xz_IntLat_xvz, xz_AvrLat_xvz :: 3 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! xy_IntRad_xvz, xy_AvrRad_xvz :: 3 �����ʻ����ǡ�����ư��������ʬ�����ʿ��
  !
  !==== ��ʬ��ʿ��(2 �����ǡ���)
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 �����ʻ����ǡ����ο�ʿ(����)��ʬ�����ʿ��
  ! IntLonRad_xz, AvrLonRad_xz :: 2 ����(XZ)�ʻ����ǡ����η���ư����ʬ
  !                            :: �����ʿ��
  ! IntLatRad_yz, AvrLatRad_yz :: 2 ����(YZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)
  !                            :: ��ʬ�����ʿ�� 
  ! y_IntLon_xy, y_AvrLon_xy   :: ��ʿ 2 ����(����)�ʻ����ǡ����η�������
  !                            :: ��ʬ�����ʿ��
  ! x_IntLat_xy, x_AvrLat_xy   :: ��ʿ2 ����(����)�ʻ����ǡ����ΰ���������ʬ
  !                            :: �����ʿ��
  ! z_IntLon_xz, z_AvrLon_xz   :: 2 ����(XZ)�ʻ����ǡ����η���������ʬ�����
  !                            :: ʿ��
  ! x_IntRad_xz, x_AvrRad_xz   :: 2 ����(XZ)�ʻ����ǡ�����ư��������ʬ�����
  !                            :: ʿ��
  ! z_IntLat_yz, z_AvrLat_yz   :: 2 ����(YZ)�ʻ����ǡ����ΰ���������ʬ�����
  !                            :: ʿ��
  ! y_IntRad_yz, y_AvrRad_yz   :: 2 ����(YZ)�ʻ����ǡ�����ư��������ʬ�����
  !                            :: ʿ��                  
  !
  ! IntLonLat_xv, AvrLonLat_xv :: 2 �����ʻ����ǡ����ο�ʿ(����)��ʬ�����ʿ��
  ! IntLonRad_xz, AvrLonRad_xz :: 2 ����(XZ)�ʻ����ǡ����η���ư����ʬ
  !                            :: �����ʿ��
  ! IntLatRad_vz, AvrLatRad_vz :: 2 ����(YZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)
  !                            :: ��ʬ�����ʿ�� 
  ! v_IntLon_xv, v_AvrLon_xv   :: ��ʿ 2 ����(����)�ʻ����ǡ����η�������
  !                            :: ��ʬ�����ʿ��
  ! v_IntLat_xv, x_AvrLat_xv   :: ��ʿ2 ����(����)�ʻ����ǡ����ΰ���������ʬ
  !                            :: �����ʿ��
  ! z_IntLon_xz, z_AvrLon_xz   :: 2 ����(XZ)�ʻ����ǡ����η���������ʬ�����
  !                            :: ʿ��
  ! x_IntRad_xz, x_AvrRad_xz   :: 2 ����(XZ)�ʻ����ǡ�����ư��������ʬ�����
  !                            :: ʿ��
  ! z_IntLat_vz, z_AvrLat_vz   :: 2 ����(YZ)�ʻ����ǡ����ΰ���������ʬ�����
  !                            :: ʿ��
  ! v_IntRad_vz, v_AvrRad_vz   :: 2 ����(YZ)�ʻ����ǡ�����ư��������ʬ�����
  !                            :: ʿ��                  
  !
  !==== ��ʬ��ʿ��(1 �����ǡ���)
  !
  ! IntLon_x, AvrLon_x  :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��
  ! IntLat_y, AvrLat_y  :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! IntRad_z, AvrRad_z  :: 1 ����(Z)�ʻ����ǡ�����ư��������ʬ�����ʿ��
  !
  ! IntLat_v, AvrLat_v  :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  !
  !==== ��ַ׻�
  !
  ! Interpolate_wt :: ���ڥ��ȥ�ǡ�������Ǥ�դ������ͤ���֤���. 
  ! 
  use dc_message
  use lumatrix
  use mpi
  use w_base_mpi_module
  use w_deriv_mpi_module
  use w_integral_mpi_module
  use wa_base_mpi_module
  use wa_deriv_mpi_module
  use wt_module

  implicit none
  integer :: ierr

  private

  public wt_mpi_Initial
  public jc

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public v_Lat, v_Lat_Weight
  public z_Rad, z_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xv_Lon, xv_Lat
  public xyz_Lon, xyz_Lat, xyz_Rad
  public xvz_Lon, xvz_Lat, xvz_Rad
  public wz_Rad
  public wt_VMiss

  public w_xy, xy_w
  public w_xv, xv_w
  public at_Dr_at, t_Dr_t, az_at, at_az
  public xyz_wt, wt_xyz, xyz_wz, wz_xyz, wz_wt, wt_wz
  public xvz_wt, wt_xvz, xvz_wz, wz_xvz
  public wt_DRad_wt, wt_DivRad_wt, wt_RotRad_wt, wt_Lapla_wt
  public xyz_GradLon_wt, xyz_Gradlat_wt
  public xvz_GradLon_wt, xvz_Gradlat_wt
  public wt_DivLon_xyz, wt_DivLat_xyz
  public wt_DivLon_xvz, wt_DivLat_xvz
  public wt_Div_xyz_xyz_xyz, xyz_Div_xyz_xyz_xyz
  public wt_Div_xvz_xvz_xvz, xvz_Div_xvz_xvz_xvz
  public xyz_RotLon_wt_wt, xyz_RotLat_wt_wt, wt_RotRad_xyz_xyz
  public xvz_RotLon_wt_wt, xvz_RotLat_wt_wt, wt_RotRad_xvz_xvz

  public yz_IntLon_xyz, xz_IntLat_xyz, xy_IntRad_xyz
  public x_IntLatRad_xyz, y_IntLonRad_xyz, z_IntLonLat_xyz
  public IntLonLatRad_xyz

  public vz_IntLon_xvz, xz_IntLat_xvz, xv_IntRad_xvz
  public x_IntLatRad_xvz, v_IntLonRad_xvz, z_IntLonLat_xvz
  public IntLonLatRad_xvz

  public x_IntLat_xy, y_IntLon_xy, IntLonLat_xy
  public z_IntLat_yz, y_IntRad_yz, IntLatRad_yz
  public z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
  public IntLon_x, IntLat_y, IntRad_z

  public x_IntLat_xv, v_IntLon_xv, IntLonLat_xv
  public z_IntLat_vz, v_IntRad_vz, IntLatRad_vz
  public IntLat_v

  public yz_AvrLon_xyz, xz_AvrLat_xyz, xy_AvrRad_xyz
  public x_AvrLatRad_xyz, y_AvrLonRad_xyz, z_AvrLonLat_xyz
  public AvrLonLatRad_xyz

  public vz_AvrLon_xvz, xz_AvrLat_xvz, xv_AvrRad_xvz
  public x_AvrLatRad_xvz, v_AvrLonRad_xvz, z_AvrLonLat_xvz
  public AvrLonLatRad_xvz

  public x_AvrLat_xy, y_AvrLon_xy, AvrLonLat_xy
  public z_AvrLat_yz, y_AvrRad_yz, AvrLatRad_yz
  public z_AvrLon_xz, x_AvrRad_xz, AvrLonRad_xz
  public AvrLon_x, AvrLat_y, AvrRad_z

  public x_AvrLat_xv, v_AvrLon_xv, AvrLonLat_xv
  public z_AvrLat_vz, v_AvrRad_vz, AvrLatRad_vz
  public AvrLat_v

  public wt_KxRGrad_wt, xvz_KGrad_wt, wt_L2_wt, wt_L2Inv_wt, wt_QOperator_wt
  public wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz
  public wt_RadRot_xvz_xvz, wt_RadRotRot_xvz_xvz_xvz
  public wt_Potential2vector, wt_Potential2Rotation
  public wt_Potential2vectorMPI, wt_Potential2RotationMPI
  public wt_VGradV
  public wt_VGradVMPI

  public Interpolate_wt

  public nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt
  public nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt

  public wt_Boundaries, wt_TorBoundaries, wz_LaplaPol2Pol_wz
  public wt_TormagBoundaries, wt_PolmagBoundaries

  public wt_BoundariesTau, wt_TorBoundariesTau
  public wt_TormagBoundariesTau, wt_PolmagBoundariesTau

  public wt_BoundariesGrid, wt_TorBoundariesGrid, wt_LaplaPol2PolGrid_wt
  public wt_TormagBoundariesGrid, wt_PolmagBoundariesGrid

  integer            :: im=64, jm=32, km=16  ! �ʻ���������(����, ����, ư��)
  integer            :: nm=21, lm=16         ! �����ȿ�������(��ʿ, ư��)
  real(8)            :: ri=0.0, ro=1.0       ! ����⳰Ⱦ��
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: xvz_LON, xvz_LAT, xvz_RAD ! ��ɸ

  save im, jm, km, nm, lm, ri, ro

  contains
  !--------------- ����� -----------------
   subroutine wt_mpi_Initial(i,j,k,n,l,r_in,r_out,np)
     !
     ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, ư�º�ɸ���ϰϤ����ꤹ��.
     !
     ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
     ! ���ʤ���Фʤ�ʤ�. 
     !
     integer,intent(in) :: i              ! �ʻ�����(���٦�)
     integer,intent(in) :: j              ! �ʻ�����(���٦�)
     integer,intent(in) :: k              ! �ʻ�����(ư�� r)
     integer,intent(in) :: n              ! �����ȿ�(��ʿ���ȿ�)
     integer,intent(in) :: l              ! �����ȿ�(ư���ȿ�)

     real(8),intent(in) :: r_in           ! �����Ⱦ��
     real(8),intent(in) :: r_out          ! ��̳�Ⱦ��

     integer,intent(in), optional :: np   ! OPENMP �Ǥκ��祹��åɿ�

     im = i  ; jm = j ; km = k
     nm = n  ; lm = l
     ri = r_in ; ro = r_out

     if ( present(np) ) then
        call wt_Initial(im,jm,km,nm,lm,ri,ro,np=np)
     else
        call wt_Initial(im,jm,km,nm,lm,ri,ro)
     endif
     call w_base_mpi_Initial
     call w_deriv_mpi_Initial
     call wa_base_mpi_Initial

     allocate(xvz_Lon(0:im-1,jc,0:km))
     allocate(xvz_Lat(0:im-1,jc,0:km))
     allocate(xvz_Rad(0:im-1,jc,0:km))

     xvz_Lon = spread(xv_Lon,3,km+1)
     xvz_Lat = spread(xv_Lat,3,km+1)
     xvz_Rad = spread(spread(z_Rad,1,jc),1,im)

   end subroutine wt_mpi_Initial

  !--------------- �����Ѵ� -----------------

    function xvz_wt(wt)
      !
      ! ���ڥ��ȥ�ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_wt
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      xvz_wt = xva_wa(az_at(wt))

    end function xvz_wt

    function wt_xvz(xvz)
      !
      ! 3 �����ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wt_xvz
      !(out) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,jc,0:km), intent(in)         :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      wt_xvz = at_az(wa_xva(xvz))

    end function wt_xvz

    function xvz_wz(wz)
      !
      ! ��ʿ���ڥ��ȥ롦ư�³ʻ����ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_wz
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),0:km), intent(in) :: wz
      !(in) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      xvz_wz = xva_wa(wz)

    end function xvz_wz

    function wz_xvz(xvz)
      !
      ! 3 �����ʻҥǡ��������ʿ���ڥ��ȥ롦ư�³ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:km)             :: wz_xvz
      !(out) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,jc,0:km), intent(in)         :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      wz_xvz = wa_xva(xvz)

    end function wz_xvz

  !--------------- ��ʬ�׻� -----------------
    function xvz_GradLon_wt(wt)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/rcos�ա���/�ߦ�
      ! ����Ѥ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_GradLon_wt
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xvz_GradLon_wt = xva_GradLon_wa(wz_wt(wt))/xvz_Rad

    end function xvz_GradLon_wt

    function xvz_GradLat_wt(wt) 
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/r ��/�ߦ� ����Ѥ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_GradLat_wt
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xvz_GradLat_wt = xva_GradLat_wa(wz_wt(wt))/xvz_Rad
    end function xvz_GradLat_wt

    function wt_DivLon_xvz(xvz)
      ! 
      ! �ʻ����ǡ�����ȯ����������ʬ 1/rcos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in)   :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)       :: wt_DivLon_xvz
      !(out) ȯ����������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_DivLon_xvz = wt_wz(wa_DivLon_xva(xvz/xvz_Rad))
    end function wt_DivLon_xvz

    function wt_DivLat_xvz(xvz)
      !
      ! �ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���(f cos��)/�ߦ� ��
      ! ���Ѥ��������ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in)   :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)       :: wt_DivLat_xvz
      !(out) ȯ����������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_DivLat_xvz = wt_wz(wa_divlat_xva(xvz/xvz_Rad))
    end function wt_DivLat_xvz

    function wt_Div_xvz_xvz_xvz(xvz_Vlon,xvz_Vlat,xvz_Vrad)
      !
      ! �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����ȯ������Ѥ�����
      ! ���ڥ��ȥ�ǡ������֤�.
      !
      ! �� 1, 2 ,3 ����(u,v,w)�����줾��٥��ȥ�η�����ʬ, ������ʬ, 
      ! ư����ʬ��ɽ��, ȯ���� 
      !
      !      1/rcos�ա���u/�ߦ� + 1/rcos�ա���(v cos��)/�ߦ� 
      !    + 1/r^2 ��/��r (r^2 w)
      !
      ! �ȷ׻������.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_Div_xvz_xvz_xvz
      !(out) �٥��ȥ���ȯ��

      wt_Div_xvz_xvz_xvz =   wt_DivLon_xvz(xvz_Vlon) &
                           + wt_DivLat_xvz(xvz_Vlat) &
                           + wt_DivRad_wt(wt_xvz(xvz_Vrad))

    end function wt_Div_xvz_xvz_xvz

    function xvz_Div_xvz_xvz_xvz(xvz_Vlon,xvz_Vlat,xvz_Vrad)
      !
      ! �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����ȯ������Ѥ�����.
      !
      ! �� 1, 2 ,3 ����(u,v,w)�����줾��٥��ȥ�η�����ʬ, ������ʬ, 
      ! ư����ʬ��ɽ��.
      !
      ! �ˤ��ð�������򤹤뤿��˥٥��ȥ��� cos��/r �νŤߤ򤫤���
      ! �׻����Ƥ���. 
      !
      !      div V = (r/cos��)��div (Vcos��/r) + V_��tan��/r + V_r/r
      ! 
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,jc,0:km)             :: xvz_Div_xvz_xvz_xvz
      !(out) �٥��ȥ���ȯ��

      xvz_Div_xvz_xvz_xvz &
           = xvz_Rad/cos(xvz_Lat) &
                * xvz_wt(wt_Div_xvz_xvz_xvz(xvz_VLon*cos(xvz_Lat)/xvz_Rad,  &
                                            xvz_VLat*cos(xvz_Lat)/xvz_Rad,  &
                                            xvz_VRad*cos(xvz_Lat)/xvz_Rad ))&
             + xvz_VLat*tan(xvz_Lat)/xvz_Rad &
             + xvz_VRad/xvz_Rad

    end function xvz_Div_xvz_xvz_xvz

    function xvz_RotLon_wt_wt(wt_Vrad,wt_Vlat) 
      !
      ! �٥��ȥ���ư����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vrad, Vlat ����
      ! ��ž�η�����ʬ 
      !
      !    1/r ��Vrad/�ߦ�-1/r ��(r Vlat)/��r ��׻�����.
      !
      ! ��׻�����
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_RotLon_wt_wt
      !(out) �٥��ȥ��β�ž�η�����ʬ

        xvz_RotLon_wt_wt =   xvz_GradLat_wt(wt_Vrad) &
                           - xvz_wt(wt_RotRad_wt(wt_Vlat))

    end function xvz_RotLon_wt_wt

    function xvz_RotLat_wt_wt(wt_Vlon,wt_Vrad) 
      !
      ! �٥��ȥ��η�����ʬ, ư����ʬ�Ǥ����� 1, 2 ���� Vlon, Vrad ����
      ! ��ž�ΰ�����ʬ 
      !
      !    1/r ��(r Vlon)/��r - 1/rcos�ա���Vrad/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_RotLat_wt_wt
      !(out) �٥��ȥ��β�ž�ΰ�����ʬ

        xvz_RotLat_wt_wt =   xvz_wt(wt_RotRad_wt(wt_Vlon)) &
                           - xvz_GradLon_wt(wt_Vrad) 

    end function xvz_RotLat_wt_wt

    function wt_RotRad_xvz_xvz(xvz_Vlat,xvz_Vlon) 
      !
      ! �٥��ȥ�ΰ�����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vlat, Vlon ���Ф���
      ! �٥��ȥ��β�ž��ư����ʬ 
      !
      !    1/rcos�ա���Vlat/�ߦ� - 1/rcos�ա���(Vlon cos��)/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_RotRad_xvz_xvz
      !(out) �٥��ȥ��β�ž��ư����ʬ

        wt_RotRad_xvz_xvz =   wt_DivLon_xvz(xvz_Vlat) &
                            - wt_DivLat_xvz(xvz_Vlon)

    end function wt_RotRad_xvz_xvz

  !--------------- �ݥ�����/�ȥ������ǥ�����ʬ -----------------

    function xvz_KGrad_wt(wt)    ! k���� = cos��/r ��/�ߦ� + sin�բ�/��r
      !
      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻҥǡ����˼�������ʬ 
      !
      !    k���� = cos��/r ��/�ߦ� + sin�բ�/��r 
      !
      ! ����Ѥ������ʻҥǡ������֤����. 
      ! �����ǥ٥��ȥ� k �ϵ���濴�����̶˸�����ñ�̥٥��ȥ�Ǥ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_KGrad_wt
      !(out) ��������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xvz_KGrad_wt =  cos(xvz_Lat)*xvz_GradLat_wt(wt) &
                    + sin(xvz_Lat)*xvz_wt(wt_Drad_wt(wt))
    end function xvz_KGrad_wt

    function wt_RadRot_xvz_xvz(xvz_VLON,xvz_VLAT)  ! r��(����v)
      !
      ! �٥��ȥ�α��٤�ư�¥٥��ȥ������ r��(����v) ��׻�����.
      !
      ! �� 1, 2 ����(v[��], v[��])�����줾��٥��ȥ�η�����ʬ, ������ʬ��ɽ��.
      !
      !    r��(����v) = 1/cos�ա���v[��]/�ߦ� - 1/cos�ա���(v[��] cos��)/�ߦ�
      !
      ! �Υ��ڥ��ȥ� �ǡ������֤����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_RadRot_xvz_xvz
      !(out) �٥��ȥ�α��٤�ư�¥٥��ȥ������

      wt_RadRot_xvz_xvz = wt_wz(wa_DivLon_xva(xvz_VLAT) &
                                - wa_DivLat_xva(xvz_VLON))
      
    end function wt_RadRot_xvz_xvz

    function wt_RadRotRot_xvz_xvz_xvz(xvz_VLON,xvz_VLAT,xvz_VRAD) 
      ! 
      ! �٥��ȥ� v ���Ф��� r��(���ߢ���v) ��׻�����.
      !
      ! �� 1, 2, 3 ����(v[��], v[��], v[r])�����줾��٥��ȥ�η�����ʬ, 
      ! ������ʬ, ư����ʬ��ɽ��. 
      !
      !    r��(���ߢ���v)  = 1/r ��/��r (r��( 1/cos�ա���v[��]/�ߦ� 
      !                                  + 1/cos�ա���(v[��] cos��)/�ߦ� ) ) 
      !                     + L^2 v[r]/r 
      !
      ! �Υ��ڥ��ȥ�ǡ������֤����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VRAD
      !(in) �٥��ȥ��ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_RadRotRot_xvz_xvz_xvz
      !(out) �٥��ȥ� v �� r��(���ߢ���v) 

      wt_RadRotRot_xvz_xvz_xvz = &
               wt_RotRad_wt(wt_wz( &
                   (wa_DivLon_xva(xvz_VLON)+ wa_DivLat_xva(xvz_VLAT)))) &
             + wt_L2_wt(wt_xvz(xvz_VRAD/xvz_RAD))

    end function wt_RadRotRot_xvz_xvz_xvz

    subroutine wt_Potential2VectorMPI(&
         xvz_VLON,xvz_VLAT,xvz_VRAD,wt_TORPOT,wt_POLPOT)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��r) + ��x��x(��r) 
      !
      ! �γ���ʬ��׻�����
      !
      real(8), dimension(0:im-1,jc,0:km)     :: xvz_VLON
      !(out) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,jc,0:km)     :: xvz_VLAT
      !(out) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km)     :: xvz_VRAD
      !(out) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      xvz_VLON =   xvz_RAD * xvz_GradLat_wt(wt_TORPOT) &
                 + xva_GradLon_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xvz_VLAT = - xvz_RAD * xvz_GradLon_wt(wt_TORPOT) &
                 + xva_GradLat_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xvz_VRAD = xvz_wt(wt_L2_wt(wt_POLPOT))/xvz_RAD

    end subroutine wt_Potential2VectorMPI

    subroutine wt_Potential2RotationMPI(&
       xvz_RotVLON,xvz_RotVLAT,xvz_RotVRAD,wt_TORPOT,wt_POLPOT)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��r) + ��x��x(��r) 
      !
      ! ���Ф���, ���β�ž
      !
      !     ��xv = ��x��x(��r) + ��x��x��x(��r) = ��x��x(��r) - ��x((��^2��)r)
      !
      ! ��׻�����. 
      
      ! �٥��ȥ��β�ž
      real(8), dimension(0:im-1,jc,0:km), intent(OUT) :: xvz_RotVLON
      !(out) ��ž�η�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(OUT) :: xvz_RotVLAT
      !(out) ��ž�ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km), intent(OUT) :: xvz_RotVRAD
      !(out) ��ž��ư����ʬ

      ! ���ϥ٥��ȥ���ɽ���ݥƥ󥷥��
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      call wt_Potential2VectorMPI( &
           xvz_RotVLON,xvz_RotVLAT,xvz_RotVRAD, &
           -wt_Lapla_wt(wt_POLPOT), wt_TORPOT)

    end subroutine wt_Potential2RotationMPI

 !------------------- ��������׻� ----------------------
    subroutine wt_VGradVMPI(xvz_VGRADV_LON,xvz_VGRADV_LAT,xvz_VGRADV_RAD, &
                            xvz_VLON,xvz_VLAT,xvz_VRAD )
      !
      ! �٥��ȥ��� v����v ��׻�����.
      !
      ! �٥��ȥ�� v=(v[��],v[��],v[r]) ���Ф���v����v�γ���ʬ��
      ! ���Τ褦�˷׻������.
      !
      !   (v����v)[��] = ����(v[��]v) + v[��]v[r]/r - v[��]v[��]tan(��)/r
      !   (v����v)[��] = ����(v[��]v) + v[��]v[r]/r - v[��]^2tan(��)/r
      !   (v����v)[r] = ����(v[r]v) + (v[��]^2+v[��]^2)/r
      !
      ! ��ȯ��®�پ���Ф��Ƥϥݥƥ󥷥�뤫�� wt_Potential2Rotation ��
      ! �Ѥ��Ʋ�ž��׻���, ������ v����v = ��(v[2^/2) - vx��xv ��
      ! �Ѥ��������褤����.
      !
      real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LON
      !(out) (v����v) ������ʬ

      real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LAT
      !(out) (v����v) ������ʬ

      real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_RAD
      !(out) (v����v) ư����ʬ

      real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLON
      !(in) �٥��ȥ�� v �η�����ʬ

      real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLAT
      !(in) �٥��ȥ�� v �ΰ�����ʬ

      real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VRAD
      !(in) �٥��ȥ�� v ��ư����ʬ

      xvz_VGRADV_LON = &
              xvz_Div_xvz_xvz_xvz( &
                  xvz_VLON * xvz_VLON, xvz_VLON*xvz_VLAT, xvz_VLON*xvz_VRAD ) &
            + xvz_VLON*xvz_VRAD/xvz_RAD              &
            - xvz_VLON*xvz_VLAT*tan(xvz_LAT)/xvz_RAD 

      xvz_VGRADV_LAT = &
              xvz_Div_xvz_xvz_xvz( &
                  xvz_VLAT*xvz_VLON, xvz_VLAT*xvz_VLAT, xvz_VLAT*xvz_VRAD ) &
            + xvz_VLAT*xvz_VRAD/xvz_RAD        &
            + xvz_VLON**2*tan(xvz_LAT)/xvz_RAD 

      xvz_VGRADV_RAD = &
              xvz_Div_xvz_xvz_xvz( &
                  xvz_VRAD*xvz_VLON, xvz_VRAD*xvz_VLAT, xvz_VRAD*xvz_VRAD ) &
            - (xvz_VLON**2 + xvz_VLAT**2)/xvz_RAD 

    end subroutine wt_VGradVMPI

  !--------------- ��ʬ�׻� -----------------
    !----(���ϥǡ��� xvz)---
    function vz_IntLon_xvz(xvz)  ! ����(�Ӿ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η�������(�Ӿ�)��ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)d�� ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(jc,0:km)  :: vz_IntLon_xvz
      !(out) ��������(�Ӿ�)��ʬ���줿 2 �����Ҹ��̳ʻ����ǡ���

      integer :: i

      vz_IntLon_xvz = 0.0d0
      do i=0,im-1
         vz_IntLon_xvz(:,:) = vz_IntLon_xvz(:,:) &
                       + xvz(i,:,:) * x_Lon_Weight(i)
      enddo
    end function vz_IntLon_xvz

    function xz_IntLat_xvz(xvz)
      !
      ! 3 �����ʻ����ǡ����ΰ�����������ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) cos�� d�� ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,0:km)  :: xz_IntLat_xvz        ! ���ٱ߳ʻ����ǡ���
      !(out) ������ʬ���줿 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,0:km)  :: xz_IntLatTMP
      integer :: j

      xz_IntLat_xvz = 0.0d0
      do j=1,jc
         xz_IntLat_xvz(:,:) = xz_IntLat_xvz(:,:) &
                       + xvz(:,j,:) * v_Lat_Weight(j)
      enddo
      xz_IntLatTmp=xz_IntLat_xvz
      CALL MPI_ALLREDUCE(xz_IntLatTMP,xz_IntLat_xvz,im*(km+1),MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function xz_IntLat_xvz

    function xv_IntRad_xvz(xvz)  ! ư����ʬ
      !
      ! 3 �����ʻ����ǡ�����ư����������ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) r^2dr ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,jc)  :: xv_IntRad_xvz
      !(out) ư����ʬ���줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���

      integer :: k

      xv_IntRad_xvz = 0.0d0
      do k=0,km
         xv_IntRad_xvz(:,:) = xv_IntRad_xvz(:,:) &
                       + xvz(:,:,k) * z_Rad_Weight(k) 
      enddo
    end function xv_IntRad_xvz

    function x_IntLatRad_xvz(xvz)
      !
      ! 3 �����ʻ����ǡ����ΰ���ư��(�Ҹ���)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) r^2cos�� d��dr 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_IntLatRad_xvz
      !(out) ����ư��(�Ҹ���)��ʬ���줿 1 �������ٳʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_IntLatRadTMP
      integer :: j, k

      x_IntLatRad_xvz = 0
      do k=0,km
         do j=1,jc
            x_IntLatRad_xvz = x_IntLatRad_xvz &
                 + xvz(:,j,k) * v_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo

      x_IntLatRadTmp=x_IntLatRad_xvz
      CALL MPI_ALLREDUCE(x_IntLatRadTMP,x_IntLatRad_xvz,im,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function x_IntLatRad_xvz

    function v_IntLonRad_xvz(xvz)
      !
      ! 3 �����ʻ����ǡ����η���ư��(���ٱ�)��ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) r^2d��dr ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(jc)       :: v_IntLonRad_xvz
      !(out) ����ư��(���ٱ�)��ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: i, k

      v_IntLonRad_xvz = 0
      do k=0,km
         do i=0,im-1
            v_IntLonRad_xvz = v_IntLonRad_xvz &
                 + xvz(i,:,k) * x_Lon_Weight(i) * z_Rad_Weight(k)
         enddo
      enddo
    end function v_IntLonRad_xvz

    function z_IntLonLat_xvz(xvz)  ! ���ٷ���(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ, ����)��ʬ
      ! 
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) cos�� d��d�� 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:km)     :: z_IntLonLat_xvz
      !(out) ���ٷ���(��ʿ, ����)��ʬ���줿 1 ����ư�³ʻ����ǡ���

      real(8), dimension(0:km)     :: z_IntLonLatTMP
      integer :: i, j

      z_IntLonLat_xvz = 0
      do j=1,jc
         do i=0,im-1
            z_IntLonLat_xvz = z_IntLonLat_xvz &
                 + xvz(i,j,:) * x_Lon_Weight(i) * v_Lat_Weight(j)
         enddo
      enddo

      z_IntLonLatTmp=z_IntLonLat_xvz
      CALL MPI_ALLREDUCE(z_IntLonLatTMP,z_IntLonLat_xvz,km+1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntLonLat_xvz

    function IntLonLatRad_xvz(xvz) ! ���ٷ���ư��(����)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���ư��(����)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !     ��f(��,��,r) r^2cos�� d��d��dr 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz 
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8)                     :: IntLonLatRad_xvz 
      !(out) ������ʬ��

      real(8)                     :: IntLonLatRadTMP
      integer :: i, j, k

      IntLonLatRad_xvz = 0
      do k=0,km
         do j=1,jc
            do i=0,im-1
               IntLonLatRad_xvz = IntLonLatRad_xvz &
                    + xvz(i,j,k) * x_Lon_Weight(i) &
                         * v_Lat_Weight(j) * z_Rad_Weight(k)
            enddo
         enddo
      enddo

      IntLonLatRadTmp=IntLonLatRad_xvz
      CALL MPI_ALLREDUCE(IntLonLatRadTMP,IntLonLatRad_xvz,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntLonLatRad_xvz

    !----(���ϥǡ��� vz)---
    function z_IntLat_vz(vz)  ! ������ʬ
      !
      ! 2 ����(VZ)�ʻ����ǡ����ΰ�����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) cos�� d�� ��׻�����.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_IntLat_vz
      !(out) ������ʬ���줿 1 ����ư�³ʻ����ǡ���

      real(8), dimension(0:km)  :: z_IntLatTMP
      integer :: j

      z_IntLat_vz = 0.0d0
      do j=1,jc
         z_IntLat_vz(:) = z_IntLat_vz(:) + vz(j,:) * v_Lat_Weight(j)
      enddo
      z_IntLatTmp=z_IntLat_vz
      CALL MPI_ALLREDUCE(z_IntLatTMP,z_IntLat_vz,km+1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntLat_vz

    function v_IntRad_vz(vz)  ! ư����ʬ
      !
      ! 2 ����(VZ)�ʻ����ǡ�����ư����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) r^2dr ��׻�����.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(jc)  :: v_IntRad_vz
      !(out) ư����ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: k

      v_IntRad_vz = 0.0d0
      do k=0,km
         v_IntRad_vz(:) = v_IntRad_vz(:) &
                       + vz(:,k) * z_Rad_Weight(k) 
      enddo
    end function v_IntRad_vz

    function IntLatRad_vz(vz)
      !
      ! 2 ����(VZ)�ʻ����ǡ����ΰ���ư����ʬ(�Ҹ���)�����ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2cos�� d��dr ��׻�����.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: IntLatRad_vz
      !(out) ��ʬ��

      real(8)                   :: IntLatRadTMP
      integer :: j, k

      IntLatRad_vz = 0
      do k=0,km
         do j=1,jc
            IntLatRad_vz = IntLatRad_vz &
                 + vz(j,k) * v_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo

      IntLatRadTmp=IntLatRad_vz
      CALL MPI_ALLREDUCE(IntLatRadTMP,IntLatRad_vz,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntLatRad_vz

  !--------------- ʿ�ѷ׻� -----------------
    !----(���ϥǡ��� xvz)---
    function vz_AvrLon_xvz(xvz)  ! ����(�Ӿ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η�������(�Ӿ�)ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)d��/2�� ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(jc,0:km)  :: vz_AvrLon_xvz
      !(out) ��������(�Ӿ�)ʿ�Ѥ��줿 2 �����Ҹ��̳ʻ����ǡ���

      vz_AvrLon_xvz = vz_IntLon_xvz(xvz)/sum(x_Lon_Weight)

    end function vz_AvrLon_xvz

    function xz_AvrLat_xvz(xvz)  ! ������ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ���������ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)cos�� d��/2 ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(im,0:km)  :: xz_AvrLat_xvz
      !(out) ����ʿ�Ѥ��줿 2 ��������ư�³ʻ����ǡ���

      xz_AvrLat_xvz = xz_IntLat_xvz(xvz)/sum(y_Lat_Weight)

    end function xz_AvrLat_xvz

    function xv_AvrRad_xvz(xvz)
      !
      ! 3 �����ʻ����ǡ�����ư��������ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� 
      !
      !    ��f(��,��,r) r^2dr/((r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,jc)  :: xv_AvrRad_xvz          ! ��ʿ�ʻ����ǡ���
      !(out) ư��ʿ�Ѥ��줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���

      xv_AvrRad_xvz = xv_IntRad_xvz(xvz)/sum(z_Rad_Weight)

    end function xv_AvrRad_xvz

    function x_AvrLatRad_xvz(xvz)  ! ����ư��(�Ҹ���)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ���ư��(�Ҹ���)ʿ��
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,,r) r^2cos�� d��dr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_AvrLatRad_xvz
      !(out) ����ư��(�Ҹ���)ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      x_AvrLatRad_xvz = x_IntLatRad_xvz(xvz) &
                   /( sum(y_Lat_Weight)*sum(z_Rad_Weight) )

    end function x_AvrLatRad_xvz

    function v_AvrLonRad_xvz(xvz)  ! ����ư��(���ٱ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η���ư��(���ٱ�)ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !     ��f(��,��,r) r^2d��dr /(2��(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(jc)       :: v_AvrLonRad_xvz
      !(out) ����ư��(���ٱ�)ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      v_AvrLonRad_xvz = v_IntLonRad_xvz(xvz) &
                 /(sum(x_Lon_Weight)*sum(z_Rad_Weight))

    end function v_AvrLonRad_xvz

    function z_AvrLonLat_xvz(xvz)  ! ���ٷ���(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ, ����)��ʬ
      ! 
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) cos�� d��d�� /4�� 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:km)     :: z_AvrLonLat_xvz
      !(out) ���ٷ���(��ʿ, ����)ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      z_AvrLonLat_xvz = z_IntLonLat_xvz(xvz) &
                 /(sum(x_Lon_Weight)*sum(y_Lat_Weight))

    end function z_AvrLonLat_xvz

    function AvrLonLatRad_xvz(xvz) ! ���ٷ���ư��(����)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���ư��(����)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) r^2cos�� d��d��dr /(4��(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8)                     :: AvrLonLatRad_xvz
      !(out) ����ʿ����

      AvrLonLatRad_xvz = IntLonLatRad_xvz(xvz) &
            /(sum(x_Lon_Weight)*sum(y_Lat_Weight) * sum(z_Rad_Weight))

    end function AvrLonLatRad_xvz

    !----(���ϥǡ��� vz)---
    function z_AvrLat_vz(vz)
      !
      ! 2 ����(VZ)�ʻ����ǡ����ΰ���������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) cos�� d��/2 ��׻�����.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_AvrLat_vz
      !(out) ����ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      z_AvrLat_vz = z_IntLat_vz(vz)/sum(y_Lat_Weight)
    end function z_AvrLat_vz

    function v_AvrRad_vz(vz)
      !
      ! 2 ����(VZ)�ʻ����ǡ�����ư��������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      ! ��׻�����.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(jc)  :: v_AvrRad_vz
      !(out) ư��ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      v_AvrRad_vz = v_IntRad_vz(vz)/sum(z_Rad_Weight)

    end function v_AvrRad_vz

    function AvrLatRad_vz(vz)  ! ����ư��(�Ҹ���)��ʬ
      !
      ! 2 ����(VZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф���
      !
      !    ��f(��,r) r^2cos�� d��dr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: AvrLatRad_vz
      !(out) ʿ����

      AvrLatRad_vz = IntLatRad_vz(vz)/(sum(y_Lat_Weight)*sum(z_Rad_Weight))

    end function AvrLatRad_vz

end module wt_mpi_module
