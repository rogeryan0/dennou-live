!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 SPMDOEL Development Group
!----------------------------------------------------------------------
!ɽ��  wa_base_module
!
!  spml/wa_base_module_sjpack �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� wa_module_sjpack �β����⥸�塼��Ǥ���, ���ڥ��ȥ�׻���
!  ����Ū�� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_base_module_sjpack �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!  �Ф����Ѵ����Ԥ���.
!
!  ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!  ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_base_initial ��Ƥ��
!  �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
!
!
!����  2009/09/05  �ݹ�����  wa_base_module ��� SJPACK �б���¤
!
!      * �Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
!++
module wa_base_module_sjpack
  !
  != wa_base_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_base_module_sjpack.f90,v 1.3 2009-09-17 16:05:24 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wa_base_module_sjpack �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� wa_module_sjpack �β����⥸�塼��Ǥ���, ���ڥ��ȥ�׻���
  ! ����Ū�� Fortran90 �ؿ����󶡤���. 
  !
  ! ���̾�� 1 �إ�ǥ��� w_base_module_sjpack �⥸�塼���¿�إ�ǥ��Ѥ�
  ! ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  ! �Ф����Ѵ����Ԥ���.
  !
  ! ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  ! ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_base_initial ��Ƥ��
  ! �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
  !
  use dc_message
  use w_base_module_sjpack, only : im, jm, nm=>nn, xy_w, w_xy
  implicit none

  integer               :: km=16         ! Ʊ���˽����������ǡ�����(�ؤο�).
                                         ! SNPACK �ѥ롼����Ȥθߴ����Τ���.
                                         ! wa_base_module_sjpack �Ǥ�
                                         ! ���Υѥ�᥿�ˤ�����¤��ʤ�.

  private

  public km                                    ! �ؿ�
  public wa_base_Initial                       ! ��������֥롼����
  public xya_wa, wa_xya                        ! �Ѵ��ؿ�

  save km                                      ! ����ǡ�����(�ؿ�)�򵭲�

  contains
  !--------------- ����� -----------------
    subroutine wa_base_initial(k_in)
      ! 
      ! SNPACK �� wa_base_initial �θߴ��Τ���Υ��ߡ����֥롼����
      !
      integer,intent(in) :: k_in               !(in) ����ǡ�����(�ؿ�)

      km = k_in

      call MessageNotify('M','wa_base_initial',&
        'No need to set maximum level number and in wa_base_module_sjpack (2009/09/05) ')
            
    end subroutine wa_base_Initial

  !--------------- �����Ѵ� -----------------

    function xya_wa(wa_data,ipow,iflag)    ! ����Ĵ�´ؿ����ڥ��ȥ� -> �ʻ���
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(¿����).
      !
      real(8), intent(in)   :: wa_data(:,:)
      !(in) ���ڥ��ȥ�ǡ���((nm+1)*(nm+1),:)
      !
      real(8)               :: xya_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) �ʻ����ǡ���(0:im-1,1:jm,:)
      !
      integer, intent(in), optional  :: ipow
      !(in) ���Ѥ����� 1/cos�� �μ���. ��ά���� 0. 
      integer, intent(in), optional  :: iflag
      !(in) �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ�
      !    1 : ������ʬ cos�ա���/�ߦ� ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !    ��ά���� 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0
      integer ipval, ifval
      integer k

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      do k=1,size(wa_data,2)
        xya_wa(:,:,k) = xy_w(wa_data(:,k),iflag=ifval,ipow=ipval)
      enddo

    end function xya_wa

    function wa_xya(xya_data,ipow,iflag) ! �ʻ��� -> ����Ĵ�´ؿ����ڥ��ȥ�
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����(¿����).
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) �ʻ����ǡ���(0:im-1,1:jm,:)

      real(8)               :: wa_xya((nm+1)*(nm+1),size(xya_data,3))
      !(out) ���ڥ��ȥ�ǡ���((nm+1)*(nm+1),:)

      integer, intent(in), optional  :: ipow
      !(in) �Ѵ�����Ʊ���˺��Ѥ����� 1/cos�� �μ���. ��ά���� 0.

      integer, intent(in), optional  :: iflag
      ! �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ� 
      !    1 : ������ʬ 1/cos�ա���(f cos^2��)/�ߦ� ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !  ��ά���� 0.
      !
      integer, parameter  :: ipow_default  = 0      ! �����å��ǥե������
      integer, parameter  :: iflag_default = 0      ! �����å��ǥե������

      integer ipval, ifval
      integer k

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      do k=1,size(xya_data,3)
         wa_xya(:,k) = w_xy(xya_data(:,:,k),iflag=ifval,ipow=ipval)
      enddo

    end function wa_xya

end module wa_base_module_sjpack
