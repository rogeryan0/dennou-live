!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_base_mpi_module
!
!  spml/wa_base_mpi_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�ä�
!  ���ͷ׻����뤿��� �⥸�塼�� wa_mpi_module �β����⥸�塼��Ǥ���, 
!  ���ڥ��ȥ�׻��δ���Ū�� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_base_mpi_module �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!  �Ф����Ѵ����Ԥ���.
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2008/05/26  �ݹ�����  wa_base_module.f90 �� mpi ��
!
module wa_base_mpi_module
  !
  ! wa_base_mpi_module
  !
  !  spml/wa_base_mpi_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  !  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�ä�
  !  ���ͷ׻����뤿��� �⥸�塼�� wa_mpi_module �β����⥸�塼��Ǥ���, 
  !  ���ڥ��ȥ�׻��δ���Ū�� Fortran90 �ؿ����󶡤���. 
  !
  !  ���̾�� 1 �إ�ǥ��� w_base_mpi_module �⥸�塼���¿�إ�ǥ��Ѥ�
  !  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  !  �Ф����Ѵ����Ԥ���.
  !
  !  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  !  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !
  use dc_message
  use w_base_module, only : im, jm, nm
  use w_base_mpi_module, only : id, jd, jc, &
                                it, t, y, ip, p, r, ia, a
  use wa_base_module, only : km
  implicit none

  integer, allocatable  :: ipk(:,:)            ! �Ѵ�������(¿����)
  real(8), allocatable  :: pk(:,:), rk(:,:)    ! �Ѵ�������(¿����)

  real(8), allocatable  :: q(:)                ! �������
  real(8), allocatable  :: ww(:), ws(:)        ! �������
  real(8), allocatable  :: w(:)                ! �������

  real(8), allocatable  :: xva_work(:,:,:)     ! wa_xva,xva_wa �Ѵ�������

  real(8), parameter    :: pi=3.14159265358979

  private
  private im, jm, nm                           ! Intel Fortran �к�

  public wa_base_mpi_Initial                   ! ��������֥롼����
  public xva_wa, wa_xva                        ! �Ѵ��ؿ�

  save ipk, pk, rk                             ! �Ѵ�������򵭲�

  contains
  !--------------- ����� -----------------
    subroutine wa_base_mpi_initial
      ! 
      ! ���ڥ��ȥ��Ѵ��κ���ǡ�����(�ؿ�)�����ꤹ��.
      !
      ! ���Υ��֥롼�����ñ�Ȥ��Ѥ���ΤǤʤ�, 
      ! ��̥��֥롼���� wa_Initial ����Ѥ��뤳��.
      !
      integer :: iw

      allocate(ipk(km,((nm+1)/2+nm+1)*2))      ! �Ѵ�������(¿����)
      allocate(pk(km,((nm+1)/2+nm+1)*jm))      ! �Ѵ�������(¿����)
      allocate(rk(km,((nm+1)/2*2+3)*(nm/2+1))) ! �Ѵ�������(¿����)

      allocate(q(km*((nm+1)/2+nm+1)*jm))       ! �������(¿����)

      allocate(xva_work(id,jd,km))                ! �Ѵ�������

      iw=km * max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)

      allocate(ws(iw),ww(iw),w((nm+1)*(nm+1)*km))    ! ���������(¿����)

      call snkini(nm,jm,km,ip,p,r,ipk,pk,rk)

      call MessageNotify('M','wa_base_mpi_initial',&
                         'wa_base_mpi_module is initialized')

    end subroutine wa_base_mpi_initial

  !--------------- �����Ѵ� -----------------

    function xva_wa(wa_data,ipow,iflag)    ! ����Ĵ�´ؿ����ڥ��ȥ� -> �ʻ���
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(¿����).
      !
      real(8), intent(in)   :: wa_data(:,:)
      !(in) ���ڥ��ȥ�ǡ���

      real(8)               :: xva_wa(im,jc,size(wa_data,2))
      !(out) �ʻ����ǡ���

      integer, intent(in), optional  :: ipow
      !(in) ���Ѥ����� 1/cos�� �μ���. ��ά���� 0. 

      integer, intent(in), optional  :: iflag
      !(in) �Ѵ��μ���
      !     0 : �̾�����Ѵ�
      !     1 : ������ʬ����Ѥ��������Ѵ�
      !    -1 : ������ʬ����Ѥ��������Ѵ�
      !     2 : sin�դ���Ѥ��������Ѵ�
      !     ��ά���� 0.
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

      k= size(wa_data,2)
      if  ( k > km ) then
         call MessageNotify('E','xva_wa','Size of 3rd dimension invalid.')
      else
         call snts2g(nm,im,id,jc,jd,k,wa_data, xva_work,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval)
      endif
      xva_wa=xva_work(1:im,1:jc,1:k)

    end function xva_wa

    function wa_xva(xva_data,ipow,iflag) ! �ʻ��� -> ����Ĵ�´ؿ����ڥ��ȥ�
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����(¿����).
      !
      real(8), intent(in)   :: xva_data(:,:,:)
      !(in) �ʻ����ǡ���(im,jm,*)

      real(8)               :: wa_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) ���ڥ��ȥ�ǡ���

      integer, intent(in), optional  :: ipow
      !(in) �Ѵ�����Ʊ���˺��Ѥ����� 1/cos�� �μ���. ��ά���� 0.

      integer, intent(in), optional  :: iflag
      ! �Ѵ��μ���
      !     0 : �̾�����Ѵ�
      !     1 : ������ʬ����Ѥ��������Ѵ�
      !    -1 : ������ʬ����Ѥ��������Ѵ�
      !     2 : sin�դ���Ѥ��������Ѵ�
      !   ��ά���� 0.

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

      k = size(xva_data,3)
      if ( k > km ) then
         call MessageNotify('E','wa_xva','Size of 3rd dimension invalid.')
      endif

      xva_work = 0.0
      xva_work(1:im,1:jc,1:k) = xva_data

      call sntgms(nm,im,id,jc,jd,k,xva_work,wa_xva,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval,w)

    end function wa_xva

end module wa_base_mpi_module
