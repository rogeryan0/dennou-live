!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_mpi_module
!
!   spml/wa_mpi_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!   ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�ä�
!   ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
!
!   ���̾�� 1 �إ�ǥ��� w_module �⥸�塼���¿�إ�ǥ��Ѥ�
!   ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!   �Ф����Ѵ����Ԥ���.
!
!   wa_mpi_module �ϼºݤˤϴ����Ѵ�, ��ʬ�׻�, ��ʬ��ʿ�ѷ׻�, ���ڥ��ȥ����
!   �򤽤줾��ô�äƤ��벼���⥸�塼�� wa_base_mpi_module, wa_deriv_mpi_module, 
!   wa_integral_module, wa_spectrum_module ����� 1 ���ѤΥ⥸�塼��
!   w_mpi_module ����ʤäƤ���.
!
!   ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
!   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!   �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2008/05/28  �ݹ�����  wa_module �� MPI ��
!
module wa_mpi_module
  !
  != wa_mpi_module
  !
  !== ����
  !
  !   spml/wa_mpi_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  !   ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�ä�
  !   ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
  !
  !   ���̾�� 1 �إ�ǥ��� w_module �⥸�塼���¿�إ�ǥ��Ѥ�
  !   ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  !   �Ф����Ѵ����Ԥ���.
  !
  !   wa_mpi_module �ϼºݤˤϴ����Ѵ�, ��ʬ�׻�, ��ʬ��ʿ�ѷ׻�, ���ڥ��ȥ����
  !   �򤽤줾��ô�äƤ��벼���⥸�塼�� wa_base_mpi_module, wa_deriv_mpi_module, 
  !   wa_integral_module, wa_spectrum_module ����� 1 ���ѤΥ⥸�塼��
  !   w_mpi_module ����ʤäƤ���.
  !
  !   ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
  !   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !   �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !   w_mpi_module �ǤΤ�Τ��ɲä����ؿ����ѿ���̾���ȷ��ˤĤ��ƤΤ���������. 
  !   w_mpi_module �Ǥδؿ����ѿ���̾���ȷ��ˤĤ��Ƥ� w_mpi_module �γ�����򻲾�
  !   ���줿��.
  !
  !=== ̿̾ˡ
  !
  !   * �ؿ�̾����Ƭ (wa_, nma_, na_, xya_, xa_, ya_, w_, xy_, x_, y_, a_) ��, 
  !     �֤��ͤη��򼨤��Ƥ���.
  !        wa_ : ��ʿ���ڥ��ȥ�ǡ������¤�� 2 ��������(���ڥ��ȥ�ǡ�����
  !              �¤Ӥ� SNPACK/ISPACK �˽��ä����)
  !       nma_ : ���ڥ��ȥ�ǡ������¤�� 3 �������� (���ڥ��ȥ�ǡ������¤Ӥ�
  !              ���ȿ� n, �Ӿ��ȿ� m �ǻ��ꤵ��� 2 ��������)
  !        na_ : ���ڥ��ȥ�ǡ������¤�� 2 �������� (���ڥ��ȥ�ǡ������¤Ӥ�
  !              ���ȿ� n �ǻ��ꤵ��� 1 ��������)
  !       xya_ : 2 �����ʻ����ǡ������¤�� 3 ��������
  !       xva_ : 2 ����ʬ���ʻ����ǡ������¤�� 3 ��������
  !        xa_ : �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !        ya_ : �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !
  !   * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !     LaplaInv, Jacobian)��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  !   * �ؿ�̾�κǸ� (_wa_wa, _wa, _xya, _xa, _ya, _w_w, _w, _xy, _x, _y) ��, 
  !     �����ѿ��η����ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !          _wa : ���ڥ��ȥ�ǡ������¤�� 2 ��������
  !       _wa_wa : 2 �ĤΥ��ڥ��ȥ�ǡ������¤�� 2 ��������
  !         _xva : 2 ����ʬ���ʻ����ǡ������¤�� 3 ��������
  !          _xa : �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !          _ya : �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !
  !=== �ƥǡ����μ��������
  !
  !   * xya : 2 �����ʻ����ǡ������¤�� 3 ��������.
  !     �ѿ��μ���ȼ����� real(8), dimension(im,jm,:). 
  !     im, jm �Ϥ��줾�����, ���ٺ�ɸ�γʻ������Ǥ���, ���֥롼����
  !     wa_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !     ������ 3 �������礭���κ����ͤ� wa_Initial �����ꤷ�Ƥ���.
  !
  !   * xva : 2 ����ʬ���ʻ����ǡ������¤�� 3 ��������.
  !     �ѿ��μ���ȼ����� real(8), dimension(im,jc,:). 
  !     im, jc �Ϥ��줾����٤γʻ����������
  !     �ץ�������ͭ������������γʻ������Ǥ���.  
  !     jc ���֥롼���� wa_mpi_Initial �ˤ����ꤷ��
  !     �������Ȥˤ�����ꤵ��� public �ѿ��Ǥ���. 
  !     ������ 3 �������礭���κ����ͤ� wa_mpi_Initial �����ꤷ�Ƥ���.
  !
  !   * wa : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !     �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),:). 
  !     nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wa_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� 
  !     l_nm, nm_l �ˤ�ä�Ĵ�٤뤳�Ȥ��Ǥ���. ������ 3 �������礭����
  !     �����ͤ� wa_Initial �����ꤷ�Ƥ���.
  !
  !   * xa, ya : ����, �������� 1 �����ʻ����ǡ���.
  !     �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(im,:) 
  !     ����� real(8), dimension(jm,:).
  !
  !   * nma : ���ڥ��ȥ�ǡ������¤�� 3 ��������.
  !     �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,:). 
  !     �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ���ɽ��. nm �ϵ���Ĵ��ȡ����
  !     �������ȿ��Ǥ���, ���֥롼���� wa_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  !   * na : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !     �ѿ��μ���ȼ����� real(8), dimension(0:nm,:). 
  !     �� 1 ��������ʿ���ȿ���ɽ��.nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wa_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  !   * wa_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ������¤�� 2 ���������Ʊ��.
  !
  !   * nma_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ������¤�� 3 ���������Ʊ��.
  !
  !   * na_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ������¤�� 2 ���������Ʊ��.
  !
  !   * xya_ �ǻϤޤ�ؿ����֤��ͤ� 2 �����ʻ����ǡ������¤�� 3 ���������
  !     Ʊ��.
  !
  !   * xva_ �ǻϤޤ�ؿ����֤��ͤ� 2 ����ʬ���ʻ����ǡ������¤�� 3 ���������
  !     Ʊ��.
  !
  !   * xa_, ya_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ������¤�� 2 ����
  !     �����Ʊ��.
  !
  !   * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !     ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! wa_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��, 
  !            :: �����Ʊ���˷׻�����ǡ����θĿ��κ���������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_Lon, x_Lon_Weight,  ::  �ʻ�����ɸ(����)�ȽŤߤ��Ǽ���� 1 ��������
  ! y_Lat, y_Lat_Weight   ::  �ʻ�����ɸ(����)�ȽŤߤ��Ǽ���� 1 ��������
  ! v_Lat, v_Lat_Weight   ::  ʬ���ʻ�����ɸ(����)�ȽŤߤ��Ǽ���� 1 ��������
  ! xy_Lon, xy_Lat        ::  �ʻ����ǡ����η��١����ٺ�ɸ(X,Y)
  !                           (�ʻ����ǡ����� 2 ��������)
  ! xv_Lon, xv_Lat        ::  ʬ���ʻ����ǡ����η��١����ٺ�ɸ(X,Y)
  !                           (�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! xya_wa, xy_w :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�(¿��, 1 ����)
  ! wa_xya, w_xy :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�(¿��, 1 ����)
  ! xva_wa, xv_w :: ���ڥ��ȥ�ǡ�������ʬ���ʻҥǡ����ؤ��Ѵ�(¿��, 1 ����)
  ! wa_xva, w_xv :: ʬ���ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�(¿��, 1 ����)
  ! l_nm, nm_l :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! wa_Lapla_wa, w_Lapla_w        :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����(¿��, 1 ����)
  ! rn                            :: ���ڥ��ȥ�ǡ����Υ�ץ饷�����׻����뤿��η���
  ! irm                           :: ������ʬ�黻������
  ! wa_LaplaInv_wa, w_LaplaInv_w  :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����(¿��, 1 ����)
  ! wa_DLon_wa, w_DLon_w          :: ���ڥ��ȥ�ǡ����˷�����ʬ��/�ߦˤ���Ѥ�����(¿��, 1 ����)
  ! xya_GradLon_wa, xy_GradLon_w  :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����(¿��, 1 ����)
  ! xya_GradLat_wa, xy_GradLat_w  :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����(¿��, 1 ����)
  ! wa_DivLon_xya, w_DivLon_xy    :: ʬ���ʻҥǡ�����ȯ����������ʬ 1/cos�ա���/�ߦˤ���Ѥ�����(¿��, 1 ����)
  ! wa_DivLat_xya, w_DivLat_xy    :: ʬ���ʻҥǡ�����ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ���Ѥ����� (¿��, 1 ����)
  ! wa_Div_xya_xya, w_Div_xy_xy   :: �٥��ȥ���ʬ�Ǥ��� 2 �Ĥ�ʬ���ʻҥǡ�����ȯ������Ѥ�����(¿��, 1 ����)
  ! wa_Jacobian_wa_wa,            :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����
  ! w_Jacobian_w_w                :: �׻�����(¿��, 1 ����)
  !
  ! xva_GradLon_wa, xv_GradLon_w  :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����(¿��, 1 ����)
  ! xva_GradLat_wa, xv_GradLat_w  :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����(¿��, 1 ����)
  ! wa_DivLon_xva, w_DivLon_xv    :: ʬ���ʻҥǡ�����ȯ����������ʬ 1/cos�ա���/�ߦˤ���Ѥ�����(¿��, 1 ����)
  ! wa_DivLat_xva, w_DivLat_xv    :: ʬ���ʻҥǡ�����ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ���Ѥ����� (¿��, 1 ����)
  ! wa_Div_xva_xva, w_Div_xv_xv   :: �٥��ȥ���ʬ�Ǥ��� 2 �Ĥ�ʬ���ʻҥǡ�����ȯ������Ѥ�����(¿��, 1 ����)
  ! wa_JacobianMPI_wa_wa,         :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����
  ! w_JacobianMPI_w_w             :: �׻�����(¿��, 1 ����)
  !
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! xya_GradLambda_wa,  :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  ! xy_GradLambda_w     :: ��/�ߦˤ���Ѥ�����(¿��, 1 ����)
  !
  ! xya_GradMu_wa,      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  ! xy_GradMu_w         :: (1-��^2)��/�ߦ̤���Ѥ�����(¿��, 1 ����)
  !
  ! wa_DivLambda_xya,   :: ʬ���ʻҥǡ�����ȯ����������ʬ
  ! w_DivLambda_xy      :: 1 /(1-��^2)����/�ߦˤ���Ѥ�����(¿��, 1 ����)
  !
  ! wa_DivMu_xya,       :: ʬ���ʻҥǡ�����ȯ����������ʬ
  ! w_DivMu_xy          :: ��/�ߦ̤���Ѥ�����(¿��, 1 ����)
  !
  ! xva_GradLambda_wa,  :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  ! xv_GradLambda_w     :: ��/�ߦˤ���Ѥ�����(¿��, 1 ����)
  !
  ! xva_GradMu_wa,      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  ! xv_GradMu_w         :: (1-��^2)��/�ߦ̤���Ѥ�����(¿��, 1 ����)
  !
  ! wa_DivLambda_xva,   :: ʬ���ʻҥǡ�����ȯ����������ʬ
  ! w_DivLambda_xv      :: 1 /(1-��^2)����/�ߦˤ���Ѥ�����(¿��, 1 ����)
  !
  ! wa_DivMu_xva,       :: ʬ���ʻҥǡ�����ȯ����������ʬ
  ! w_DivMu_xv          :: ��/�ߦ̤���Ѥ�����(¿��, 1 ����)
  !
  !==== ��ַ׻�
  !
  ! a_Interpolate_wa, Interpolate_w :: ���ڥ��ȥ�ǡ�������Ǥ�դ����δؿ��ͤ�׻�����.
  !
  !==== ��ʬ��ʿ��
  !
  ! a_IntLonLat_xya, a_AvrLonLat_xya :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��(¿����)
  ! IntLonLat_xy, AvrLonLat_xy    :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��(1 ����)
  ! ya_IntLon_xya, ya_AvrLon_xya  :: 2 �����ʻ����ǡ����η���������ʬ�����ʿ��(¿����)
  ! y_IntLon_xy, y_AvrLon_xy   :: 2 �����ʻ����ǡ����η���������ʬ�����ʿ��(1 ����)
  ! a_IntLon_xa, a_AvrLon_xa   :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��(¿����)
  ! IntLon_x, AvrLon_x         :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��(1 ����)
  ! xa_IntLat_xya, x_AvrLat_xy  :: 2 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��(¿����)
  ! x_IntLat_xy, x_AvrLat_xy   :: 2 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��(1 ����)
  ! a_IntLat_ya, a_AvrLat_ya   :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��(¿����)
  ! IntLat_y, AvrLat_y         :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��(1 ����)
  !
  ! a_IntLonLat_xva, a_AvrLonLat_xva  :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��(¿����)
  ! va_IntLon_xva, va_AvrLon_xva      :: 2 �����ʻ����ǡ����η���������ʬ�����ʿ��(¿����)
  ! xa_IntLat_xva, xa_AvrLat_xva      :: 2 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��(¿����)
  ! a_IntLat_va, a_AvrLat_va          :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��(¿����)
  !
  !==== ���ڥ��ȥ����
  !
  ! nma_EnergyFromStreamfunc_wa :: ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����
  ! nm_EnergyFromStreamfunc_w   :: (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)(¿��, 1 ����)
  ! 
  ! na_EnergyFromStreamfunc_wa   :: ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����
  ! n_EnergyFromStreamfunc_w     :: (��ʿ���ȿ� n ����) (¿��, 1 ����)
  ! 
  ! nma_EnstrophyFromStreamfunc_wa :: ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��
  ! nm_EnstrophyFromStreamfunc_w   :: �׻�����(��ʿ���ȿ� n, �Ӿ��ȿ� m ����)(¿��, 1 ����)
  ! 
  ! na_EnstrophyFromStreamfunc_wa :: ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��
  ! n_EnstrophyFromStreamfunc_w   :: �׻�����(��ʿ���ȿ� n ����)(¿��, 1 ����)
  ! 
  ! w_spectrum_VMiss              ::  ��»��
  !
  !
  use dc_message, only : MessageNotify
  use wa_module
  use w_base_mpi_module
  use w_deriv_mpi_module
  use w_integral_mpi_module
  use wa_base_mpi_module
  use wa_deriv_mpi_module
  use wa_integral_mpi_module

  private

  public wa_mpi_Initial                       ! �����
  public jc                                   ! ��������ʬ���ʻ�����

  public x_Lon, x_Lon_Weight                  ! ���ٳʻҺ�ɸ���Ť�
  public v_Lat, v_Lat_Weight                  ! ����ʬ���ʻҺ�ɸ���Ť�
  public y_Lat, y_Lat_Weight                  ! ���ٳʻҺ�ɸ���Ť�
  public xy_Lon, xy_Lat                       ! �ʻҺ�ɸ(im,jm)
  public xv_Lon, xv_Lat                       ! ʬ���ʻҺ�ɸ(im,jc)
  public xy_w, w_xy, l_nm, nm_l               ! �Ѵ��ؿ�
  public xv_w, w_xv                           ! �Ѵ��ؿ�
  public xya_wa, wa_xya                       ! �Ѵ��ؿ�
  public xva_wa, wa_xva                       ! �Ѵ��ؿ�
  
  public rn, irm                              ! ��ץ饷����η���, ������ʬ������
  public w_Lapla_w, w_LaplaInv_w              ! ��ץ饷����ȵձ黻
  public w_DLon_w                             ! ������ʬ
  public xv_GradLon_w, xv_GradLat_w           ! ���۷���ʬ
  public w_DivLon_xv, w_DivLat_xv             ! ȯ������ʬ
  public w_Div_xv_xv                          ! ȯ������ʬ
  public w_JacobianMPI_w_w                    ! �䥳�ӥ���
  public xv_GradLambda_w, xv_GradMu_w         ! ���۷���ʬ(��,�̺�ɸ)
  public w_DivLambda_xv, w_DivMu_xv           ! ȯ������ʬ(��,�̺�ɸ)

  public wa_Lapla_wa, wa_LaplaInv_wa          ! ��ץ饷����ȵձ黻
  public wa_DLon_wa                           ! ������ʬ
  public xva_GradLon_wa, xva_GradLat_wa       ! ���۷���ʬ
  public wa_DivLon_xva, wa_DivLat_xva         ! ȯ������ʬ
  public wa_Div_xva_xva                       ! ȯ������ʬ
  public wa_JacobianMPI_wa_wa                 ! �䥳�ӥ���
  public xva_GradLambda_wa, xva_GradMu_wa     ! ���۷���ʬ(��,�̺�ɸ)
  public wa_DivLambda_xva, wa_DivMu_xva       ! ȯ������ʬ(��,�̺�ɸ)

  public a_Interpolate_wa, Interpolate_w      ! ��ִؿ�

  public IntLonLat_xy                         ! ���ٷ�����ʬ
  public y_IntLon_xy, IntLon_x                ! ������ʬ    
  public x_IntLat_xy, IntLat_y                ! ������ʬ    
  public AvrLonLat_xy                         ! ���ٷ���ʿ��
  public y_AvrLon_xy, AvrLon_x                ! ����ʿ��    
  public x_AvrLat_xy, AvrLat_y                ! ����ʿ��    

  public IntLonLat_xv                         ! ���ٷ�����ʬ
  public v_IntLon_xv                          ! ������ʬ    
  public x_IntLat_xv, IntLat_v                ! ������ʬ    
  public AvrLonLat_xv                         ! ���ٷ���ʿ��
  public v_AvrLon_xv                          ! ����ʿ��    
  public x_AvrLat_xv, AvrLat_v                ! ����ʿ��    

  public a_IntLonLat_xya                      ! ���ٷ�����ʬ
  public ya_IntLon_xya, a_IntLon_xa           ! ������ʬ    
  public xa_IntLat_xya, a_IntLat_ya           ! ������ʬ    
  public a_AvrLonLat_xya                      ! ���ٷ���ʿ��
  public ya_AvrLon_xya, a_AvrLon_xa           ! ����ʿ��    
  public xa_AvrLat_xya, a_AvrLat_ya           ! ����ʿ��    

  public a_IntLonLat_xva                      ! ���ٷ�����ʬ
  public va_IntLon_xva                        ! ������ʬ    
  public xa_IntLat_xva, a_IntLat_va           ! ������ʬ    
  public a_AvrLonLat_xva                      ! ���ٷ���ʿ��
  public va_AvrLon_xva                        ! ����ʿ��    
  public xa_AvrLat_xva, a_AvrLat_va           ! ����ʿ��    

  public nma_EnergyFromStreamfunc_wa          ! ���ͥ륮�����ڥ��ȥ�
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public na_EnergyFromStreamfunc_wa           ! ���ͥ륮�����ڥ��ȥ�
                                              ! (��ʿ���ȿ� n ����)
  public nma_EnstrophyFromStreamfunc_wa       ! ���󥹥ȥ�ե������ڥ��ȥ�
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public na_EnstrophyFromStreamfunc_wa        ! ���󥹥ȥ�ե������ڥ��ȥ�
                                              !  (��ʿ���ȿ� n ����)
  public wa_spectrum_VMiss                    ! ��»��

  public nm_EnergyFromStreamfunc_w          ! ���ͥ륮�����ڥ��ȥ�           
                                            ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnergyFromStreamfunc_w           ! ���ͥ륮�����ڥ��ȥ�
                                            ! (��ʿ���ȿ� n ����) 
  public nm_EnstrophyFromStreamfunc_w       ! ���󥹥ȥ�ե������ڥ��ȥ�     
                                            ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnstrophyFromStreamfunc_w        ! ���󥹥ȥ�ե������ڥ��ȥ�  
                                            !  (��ʿ���ȿ� n ����)
  public w_spectrum_VMiss                   ! ��»��

contains
  subroutine wa_mpi_Initial(n_in,i_in,j_in,k_in)
    !
    ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, ����ǡ����������ꤹ��.
    !
    ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
    ! ���ʤ���Фʤ�ʤ�. 
    !
    integer,intent(in) :: i_in                ! �ʻ�����(����)
    integer,intent(in) :: j_in                ! �ʻ�����(����)
    integer,intent(in) :: n_in                ! �����ȿ�
    integer,intent(in) :: k_in                ! ����ǡ�����(�ؿ�)

    call wa_Initial(n_in,i_in,j_in,k_in)
    call w_base_mpi_Initial
    call w_deriv_mpi_initial
    call wa_base_mpi_Initial

    call MessageNotify('M','wa_mpi_initial','wa_mpi_module is initialized')

  end subroutine wa_mpi_Initial

end module wa_mpi_module
