!----------------------------------------------------------------------
! Copyright (c) 2008-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_base_module
!
!  spml/w_base_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� w_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�׻���
!  ����Ū�� Fortran90 �ؿ����󶡤���. 
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2008/05/26  �ݹ�����  w_base_module �� MPI ��
!      2010/01/07  ��������ʿ  RDoc �ѤΥɥ�����Ƚ���, 
!
module w_base_mpi_module
  !
  ! w_base_mpi_module
  !
  !  spml/w_base_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  !  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI �ˤ�äƿ��ͷ׻����뤿��� 
  !  �⥸�塼�� w_mpi_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
  !  ����Ū�ʤ� Fortran90 �ؿ����󶡤���. 
  !
  !  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  !  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use dc_message
  use w_base_module, only : im, jm, nm, x_Lon

  implicit none

  integer               :: it(6)            ! �Ѵ�������(ʬ���ʻ�����)
  real(8), allocatable  :: t(:)             ! �Ѵ�������(ʬ���ʻ�����)
  integer, allocatable  :: ip(:)            ! �Ѵ�������(ʬ���ʻ�����)
  real(8), allocatable  :: p(:), r(:)       ! �Ѵ�������(ʬ���ʻ�����)
  integer, allocatable  :: ia(:)            ! �Ѵ�������(ʬ���ʻ�����)
  real(8), allocatable  :: a(:)             ! �Ѵ�������(ʬ���ʻ�����)
  real(8), allocatable  :: y(:)             ! �Ѵ�������(ʬ���ʻ�����)

  integer               :: jc               ! ʬ���������ѿ�
  real(8), allocatable  :: yy(:,:)          ! �Ѵ�������
  
  real(8), allocatable  :: q(:)             ! �������
  real(8), allocatable  :: ww(:), ws(:)     ! �������
  real(8), allocatable  :: w(:)             ! �������

  real(8), allocatable  :: v_Lat(:),v_Lat_Weight(:)      ! ���ٷ���

  real(8), allocatable  :: xv_Lon(:,:), xv_Lat(:,:)

  real(8), allocatable  :: xv_work(:,:)     ! w_xv,xv_w �Ѵ�������

  integer               :: id=65, jd=33     ! xv_work ���礭��

  real(8), parameter    :: pi=3.1415926535897932385D0

  private
  private im, jm, nm                          ! Intel Fortran �к�

  public it, t, y, ip, p, r, ia, a            ! �Ѵ��Ѻ������
  public id, jd                               ! �����������礭��
  public jc                                   ! ʬ�����־���

  public w_base_mpi_Initial                   ! ��������֥롼����
  public v_Lat, v_Lat_Weight                  ! ����ʬ���ʻҺ�ɸ���Ť�
  public xv_Lon, xv_Lat                       ! ʬ���ʻҺ�ɸ(im,jc)
  public xv_w, w_xv                           ! �Ѵ��ؿ�

  save it, t, y, ip, p, r, ia, a              ! �Ѵ�������򵭲�
  save jc                                     ! ʬ���ʻ���������礭��
  save id, jd                                 ! �Ѵ���������礭��

  contains
  !--------------- ����� -----------------
    subroutine w_base_mpi_Initial
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ꤹ��.
      !
      ! �ºݤλ��ѤˤϾ�̥��֥롼���� w_mpi_Initial ���Ѥ��뤳��.
      !
      integer :: iw, i, j

      allocate(t(im*2))                       ! �Ѵ�������(ʬ������)
      allocate(ip(((nm+1)/2+nm+1)*2))         ! �Ѵ�������(ʬ������)
      allocate(p(((nm+1)/2+nm+1)*jm))         ! �Ѵ�������(ʬ������)
      allocate(r(((nm+1)/2*2+3)*(nm/2+1)))    ! �Ѵ�������(ʬ������)
      allocate(ia((nm+1)*(nm+1)*4))           ! �Ѵ�������(ʬ������)
      allocate(a((nm+1)*(nm+1)*6))            ! �Ѵ�������(ʬ������)
      allocate(y(jm*2))                       ! �Ѵ�������(ʬ������)

      ! ��� : �̥롼����ˤ�ä� w_base_Initial ���Ƥ�Ǥ��뤳�Ȥ���
      call snmini(nm,im,jm,jc,it,t,y,ip,p,r,ia,a)

      if ( im/2*2 .eq. im ) then
         id = im+1 
      else
         id = im
      endif
      if ( jc/2*2 .eq. jc ) then
         jd = jc+1
      else
         jd = jc
      endif
      allocate(xv_work(id,jd))                ! �Ѵ�������

      allocate(q(((nm+1)/2+nm+1)*jm))         ! �������
 
      iw=max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)
      allocate(ws(iw),ww(iw), w((nm+1)*(nm+1)))    ! ���������
      allocate(yy(jc/2,4))                         ! �Ѵ�������

      allocate(v_Lat(jc),v_Lat_Weight(jc))             ! �ʻ�����ɸ��Ǽ����

      allocate(xv_Lon(0:im-1,jc),xv_Lat(0:im-1,jc))   ! �ʻ�����ɸ��Ǽ����

      yy = reshape(y(1:2*jc),(/jc/2,4/))

      do j=1,jc/2
         v_Lat(jc/2+j)   =  asin(yy(j,1))        ! ���ٺ�ɸ
         v_Lat(jc/2-j+1) = -asin(yy(j,1))        ! ���ٺ�ɸ
         v_Lat_Weight(jc/2+j)   = 2*yy(j,2)      ! ���ٽŤ�(Gauss grid)
         v_Lat_Weight(jc/2-j+1) = 2*yy(j,2)      ! ���ٽŤ�(Gauss grid)
      enddo
  
      do j=1,jc
         xv_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xv_Lat(i,:) = v_Lat
      enddo

      call MessageNotify('M','w_base_mpi_initial',&
                         'w_base_mpi_module is initialized')
    end subroutine w_base_mpi_Initial

  !--------------- �����Ѵ�(�ʻ���ʬ������) -----------------

    function xv_w(w_data,ipow,iflag)
      !
      ! ���ڥ��ȥ�ǡ�������ʬ���ʻҥǡ������Ѵ�����(1 ����).
      !
      real(8)               :: xv_w(0:im-1,jc)
      !(out) �ʻ����ǡ���

      real(8), intent(in)   :: w_data((nm+1)*(nm+1))
      !(in) ���ڥ��ȥ�ǡ���

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

      call snts2g(nm,im,id,jc,jd,1,w_data,xv_work,&
                  it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)

      xv_w=xv_work(1:im,1:jc)

    end function xv_w

    function w_xv(xv_data,ipow,iflag)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����(1 ����).
      !
      real(8)               :: w_xv((nm+1)*(nm+1))
      !(out) ���ڥ��ȥ�ǡ���

      real(8), intent(in)   :: xv_data(0:im-1,jc)
      !(in) �ʻ����ǡ���

      integer, intent(in), optional  :: ipow
      !(in) �Ѵ�����Ʊ���˺��Ѥ����� 1/cos�� �μ���. ��ά���� 0.

      integer, intent(in), optional  :: iflag
      ! �Ѵ��μ���
      !     0 : �̾�����Ѵ�
      !     1 : ������ʬ����Ѥ��������Ѵ�
      !    -1 : ������ʬ����Ѥ��������Ѵ�
      !     2 : sin�դ���Ѥ��������Ѵ�
      !   ��ά���� 0.


      integer, parameter  :: ipow_default  = 0    ! �����å��ǥե������
      integer, parameter  :: iflag_default = 0    ! �����å��ǥե������

      integer ipval, ifval

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

      xv_work(1:im,1:jc)=xv_data

      call sntgms(nm,im,id,jc,jd,1,xv_work,w_xv,&
                 it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval,w)

    end function w_xv

end module w_base_mpi_module
