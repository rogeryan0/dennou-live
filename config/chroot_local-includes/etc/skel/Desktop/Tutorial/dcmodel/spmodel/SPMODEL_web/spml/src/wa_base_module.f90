!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 SPMDOEL Development Group
!----------------------------------------------------------------------
!ɽ��  wa_base_module
!
!  spml/wa_base_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�׻���
!  ����Ū�� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_base_module �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!  �Ф����Ѵ����Ԥ���.
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!  ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_base_initial ��Ƥ��
!  �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
!
!
!����  2002/02/02  �ݹ�����  ¿���Ѥ˲�¤
!      2002/03/30  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/05/25  �ݹ�����  �ʻ�����ɸ����̤��٤�̿̾ˡ�ѹ�
!      2005/01/09  �ݹ�����  msgdmp -> MessageNotify ���ѹ�
!      2005/07/09  �ݹ�����  OPENMP ���Ѵ��롼������б�
!                            �Х󥯶�����򤱤뤿��κ�������ɲ�
!      2005/07/10  �ݹ�����  OpenMP ���åȥ��åפΥ�å���������
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2007/11/21  �ݹ�����  ��������֥롼�����å���������
!      2008/05/31  �ݹ�����  ��������֥롼����ʬΥ
!      2008/06/22  ��������ʿ ��ʿ�����γʻ����ǡ�������λ�����
!                              1 ���� 0 ���ѹ�
!      2008/06/28  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2008/07/07  �ݹ�����  ����ǽ�����������������Ƥ����ѹ�
!      2008/12/29  �ݹ�����  xya_wa, wa_xya �����Ƚ���
!      2009/01/09  �ݹ�����  wa_base_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/07/30  �ݹ�����   ����ΰ��������ѿ����ѹ�(for OpenMP)
!
!      * �Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
!++
module wa_base_module
  !
  != wa_base_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_base_module.f90,v 1.16 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wa_base_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�׻���
  ! ����Ū�� Fortran90 �ؿ����󶡤���. 
  !
  ! ���̾�� 1 �إ�ǥ��� w_base_module �⥸�塼���¿�إ�ǥ��Ѥ�
  ! ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  ! �Ф����Ѵ����Ԥ���.
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  ! ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_base_initial ��Ƥ��
  ! �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
  !
  use dc_message
  use w_base_module, only : im, jm, nm, it, t, y, ip, p, r, ia, a, openmp, np
  implicit none

  integer               :: km=16         ! Ʊ���˽����������ǡ�����(�ؤο�)

  integer, allocatable  :: ipk(:,:)            ! �Ѵ�������(¿����)
  real(8), allocatable  :: pk(:,:), rk(:,:)    ! �Ѵ�������(¿����)

  integer               :: id=65, jd=33        ! xya_work ���礭��
  integer               :: iw                  ! ww, ws ���礭��

  real(8), parameter    :: pi=3.14159265358979

  private

  public km                                    ! �ؿ�
  public wa_base_Initial                       ! ��������֥롼����
  public xya_wa, wa_xya                        ! �Ѵ��ؿ�

  save km                                      ! ����ǡ�����(�ؿ�)�򵭲�
  save ipk, pk, rk                             ! �Ѵ�������򵭲�
  save id, jd, iw                              ! �Ѵ���������礭��

  contains
  !--------------- ����� -----------------
    subroutine wa_base_initial(k_in)
      ! 
      ! ���ڥ��ȥ��Ѵ��κ���ǡ�����(�ؿ�)�����ꤹ��.
      !
      ! ���Υ��֥롼�����ñ�Ȥ��Ѥ���ΤǤʤ�, 
      ! ��̥��֥롼���� wa_Initial ����Ѥ��뤳��.
      !
      integer,intent(in) :: k_in               !(in) ����ǡ�����(�ؿ�)

      km = k_in

      allocate(ipk(km,((nm+1)/2+nm+1)*2))      ! �Ѵ�������(¿����)
      allocate(pk(km,((nm+1)/2+nm+1)*jm))      ! �Ѵ�������(¿����)
      allocate(rk(km,((nm+1)/2*2+3)*(nm/2+1))) ! �Ѵ�������(¿����)

      if ( im/2*2 .eq. im ) then
         id = im+1 
      else
         id = im
      endif
      if ( openmp ) then
         jd = jm
      else if ( jm/2*2 .eq. jm ) then
         jd = jm+1
      else
         jd = jm
      endif

      if ( openmp ) then
         iw=km*(im+nm+1)*3*jm/2
         call MessageNotify('M','wa_base_Initial', &
              'OpenMP computation was set up.')
      else
         iw=km * max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)
      endif

      call snkini(nm,jm,km,ip,p,r,ipk,pk,rk)

      call MessageNotify('M','wa_base_initial',&
           'wa_base_module (2009/07/30) is initialized')

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
      !    1 : ������ʬ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !    ��ά���� 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0
      integer ipval, ifval
      integer k, i

      real(8) :: xya_work(id,jd,km)              ! �Ѵ�������
      real(8) :: q(km*((nm+1)/2+nm+1)*jm)        ! �������(¿����)
      real(8) :: ws(iw),ww(iw)                   ! ���������(¿����)
      real(8) :: wv(km*(nm+4)*(nm+3)*np)         ! ���������(OPENMP��)

      logical :: first=.true.                    ! ���Ƚ�ꥹ���å�
      save first

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

      if  ( size(wa_data,1) /= (nm+1)**2 ) then
         call MessageNotify('E','xya_wa','Size of 1st dimension invalid.')
      end if

      k=size(wa_data,2)
      if  ( k > km ) then
         call MessageNotify('E','xya_wa','Size of 2nd dimension invalid.')
      else  if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','xya_wa', &
                 'OpenMP routine SNTSOG/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntsog(nm,im,id,jm,k,wa_data,xya_work,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),&
              ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call snts2g(nm,im,id,jm,jd,k,wa_data, xya_work,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval)
      endif

      do i=0,im-1
        xya_wa(i,1:jm,1:k) = xya_work(i+1,1:jm,1:k)
      enddo
      first = .false.

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
      !    1 : ������ʬ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !  ��ά���� 0.

      integer, parameter  :: ipow_default  = 0      ! �����å��ǥե������
      integer, parameter  :: iflag_default = 0      ! �����å��ǥե������

      integer ipval, ifval
      integer i,k

      real(8) :: xya_work(id,jd,km)               ! �Ѵ�������
      real(8) :: q(km*((nm+1)/2+nm+1)*jm)         ! �������(¿����)
      real(8) :: ws(iw),ww(iw)                    ! ���������(¿����)
      real(8) :: wv(km*(nm+4)*(nm+3)*np)          ! ���������(OPENMP��)

      logical :: first=.true.                     ! ���Ƚ�ꥹ���å�
      save first

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

      if ( size(xya_data,1) /= im ) then
         call MessageNotify('E','wa_xya','Size of 1st dimension invalid.')
      endif

      if ( size(xya_data,2) /= jm ) then
         call MessageNotify('E','wa_xya','Size of 2nd dimension invalid.')
      endif

      k = size(xya_data,3)
      if ( k > km ) then
         call MessageNotify('E','wa_xya','Size of 3rd dimension invalid.')
      endif

      do i=0,im-1
        xya_work(i+1,1:jm,1:k) = xya_data(i,1:jm,1:k)
      enddo

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','wa_xya', &
                 'OpenMP routine SNTGOS/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntgos(nm,im,id,jm,k,xya_work,wa_xya,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),&
              ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call sntg2s(nm,im,id,jm,jd,k,xya_work,wa_xya,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval)
      endif
      first = .false.

    end function wa_xya

  end module wa_base_module
