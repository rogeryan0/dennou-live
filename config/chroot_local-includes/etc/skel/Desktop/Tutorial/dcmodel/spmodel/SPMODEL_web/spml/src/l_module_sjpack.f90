!--
!----------------------------------------------------------------------
! Copyright (c) 2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  l_module_sjpack
!
!   spml/l_module_sjpack �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū 
!   1 ����ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
!   ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
!
!   ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
!   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!   �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!   �ؿ�, ���֥롼�����̾���ȵ�ǽ�� l_module �Τ�Τ�Ʊ�����߷פ��Ƥ���. 
!   �������ä� use ʸ�� l_module ���� l_module_sjpack ���ѹ���������� 
!   SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
!
!����  2009/09/22  �ݹ�����  l_module �򸵤� SJPACK �Ѥ˲�¤
!
!++
module l_module_sjpack
  !
  != l_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: l_module_sjpack.f90,v 1.1 2009-09-24 07:12:09 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  !   spml/l_module_sjpack �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū 
  !   1 ����ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
  !   ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
  !
  !   ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
  !   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !   �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !   �ؿ�, ���֥롼�����̾���ȵ�ǽ�� l_module �Τ�Τ�Ʊ�����߷פ��Ƥ���. 
  !   �������ä� use ʸ�� l_module ���� l_module_sjpack ���ѹ���������� 
  !   SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (l_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   l_ :: ���ڥ��ȥ�(�른���ɥ�¿�༰��ʬ)�ǡ���
  !   y_ :: 1 �������ٳʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(GradLat, DivLat, Lapla, LaplaInv)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_l, _y) ��, �����ѿ��η����ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _l :: ���ڥ���(�른���ɥ�¿�༰��ʬ)��ǡ���
  !   _y :: �������� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * p : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm). 
  !   * nm �ϥ른���ɥ�¿�༰�κ��缡���Ǥ���, ���֥롼���� l_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !
  ! * y : �������� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(1:jm).
  !
  ! * l_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * y_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! l_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! y_Lat        ::  �ʻ�����ɸ(����, ���ٺ�ɸ)���Ǽ���� 1 ��������
  ! y_Lat_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  !
  !==== �����Ѵ�
  !
  ! y_l :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! l_y :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! l_Lapla_l       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! l_LaplaInv_l    :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! y_GradLat_l     :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦդ���Ѥ�����
  ! l_DivLat_y      :: �ʻҥǡ�����ȯ����������ʬ
  !                    1 /cos�ա���(g cos��)/�ߦդ���Ѥ�����
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! y_GradMu_l     :: ���ڥ��ȥ�ǡ�����
  !                   ���۷�������ʬ (1-��^2)��/�ߦ̤���Ѥ�����
  ! l_DivMu_y      :: �ʻҥǡ�����ȯ����������ʬ��/�ߦ̤���Ѥ�����
  !
  !==== ���
  !
  ! Interpolate_l  :: ���ڥ��ȥ�ǡ�������Ǥ�դ����Ǥ��ͤ����. 
  !
  !==== ��ʬ��ʿ��
  !
  ! IntLat_y, AvrLat_y :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! ������
  !
  use dc_message, only : MessageNotify

  implicit none

  private

  public l_Initial                            ! �����

  public y_Lat                                ! �ʻҺ�ɸ
  public y_Lat_Weight                         ! �ʻҺ�ɸ�Ť�

  public y_l, l_y                             ! �Ѵ��ؿ�
  public l_Lapla_l, l_LaplaInv_l              ! ��ץ饷����ȵձ黻
  public y_GradLat_l                          ! ���۷���ʬ
  public l_DivLat_y                           ! ȯ������ʬ

  public y_GradMu_l                           ! ���۷���ʬ
  public l_DivMu_y                            ! ȯ������ʬ

  public IntLat_y, AvrLat_y                   ! ����ʿ��

  public Interpolate_l                        ! ��ַ׻�

  integer               :: jm=32            ! �ʻ���������(����)
  integer               :: nm=21            ! �׻������������ȿ�������
  integer               :: nn=22            ! �����ȿ�(���ȿ�)������
  integer               :: mm=1             ! �����ȿ�(�����ȿ�)������

  real(8), allocatable  :: p(:,:), r(:)     ! �Ѵ�������
  real(8), allocatable  :: c(:)             ! ��ʬ������

  real(8), allocatable  :: y_Lat(:)         ! ���ٷ���
  real(8), allocatable  :: y_Lat_Weight(:)  ! ��ɸ�Ť�

  save jm, nm, nn, mm, p, r, c, y_Lat, y_Lat_Weight

contains

  !--------------- ����� -----------------
    subroutine l_initial(n_in,j_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ꤹ��.
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
      ! ���ʤ���Фʤ�ʤ�. 
      !
      integer,intent(in) :: j_in              !(in) �ʻ�����(����)
      integer,intent(in) :: n_in              !(in) �����ȿ�������

      integer :: j

      jm = j_in ; nn = n_in ; nm = n_in+1 ; mm = 1

      allocate(p(jm/2,mm+4))                  ! �Ѵ�������
      allocate(r((mm+1)*(2*nm-mm-1)+1))       ! �Ѵ�������
      allocate(c((nn+1)*(nn+1)))              ! ��ʬ������

      allocate(y_Lat(jm),y_Lat_Weight(jm))      ! ��ɸ�ѿ�����

      call ljinit(mm,nm,jm,p,r)

      call sjinic(nn,c)

      do j=1,jm/2
         y_Lat(jm/2+j)   =  asin(p(j,1))        ! ���ٺ�ɸ
         y_Lat(jm/2-j+1) = -asin(p(j,1))        ! ���ٺ�ɸ
         y_Lat_Weight(jm/2+j)   = 2*p(j,2)      ! ���ٽŤ�(Gauss grid)
         y_Lat_Weight(jm/2-j+1) = 2*p(j,2)      ! ���ٽŤ�(Gauss grid)
      enddo

      call MessageNotify(&
        'M','l_initial','l_module_sjpack (2009/09/22) is initialized')

    end subroutine l_initial

  !--------------- �����Ѵ� -----------------
    function y_l(l_data)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(1 ����).
      !
      real(8)               :: y_l(1:jm)
      !(out) �ʻ����ǡ���

      real(8), intent(in)   :: l_data(0:nn)
      !(in) ���ڥ��ȥ�ǡ���

      real(8)  :: q(jm/2*7)                ! �Ѵ��Ѻ������
      real(8)  :: ws(nn+1)                 ! �Ѵ��Ѻ������

      call ljtszg(nm,nn,jm,l_data,y_l,p,q,r,ws,0)

    end function y_l

    function l_y(y_data)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(1 ����).
      !
      real(8)               :: l_y(0:nn)
      !(in) ���ڥ��ȥ�ǡ���

      real(8), intent(in)   :: y_data(1:jm)
      !(in) �ʻ����ǡ���


      real(8)  :: q(jm/2*7)               ! �Ѵ�a�Ѻ������
      real(8)  :: ws(nn+1)                ! �Ѵ��Ѻ������

      call ljtgzs(nm,nn,jm,l_y,y_data,p,q,r,ws,0)

    end function l_y


  !--------------- ��ʬ�׻� -----------------
    function l_Lapla_l(l_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !    ��^2 = 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)
      !
      ! ����Ѥ���. 
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8)              :: l_Lapla_l(0:nn)
      !(out) ���ϥ��ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), intent(in)  :: l_data(0:nn)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer :: n

      do n=0,nn
         l_Lapla_l(n) = -n*(n+1)*l_data(n)
      enddo

    end function l_Lapla_l

    function l_LaplaInv_l(l_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����
      !
      !    ��^{-2}
      !      =[1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)]^{-1}
      !
      ! ����Ѥ���. 
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8)              :: l_LaplaInv_l(0:nm)
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), intent(in)  :: l_data(0:nm)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer ::  n

      l_LaplaInv_l(0) = 0.0D0
      do n=1,nn
         l_LaplaInv_l(n) = -l_data(n)/(n*(n+1))
      enddo

    end function l_LaplaInv_l

    function y_GradLat_l(l_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: y_GradLat_l(1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: l_data(0:nn)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8)              :: l_ydata(0:nn+1)
      ! ��ȥ��ڥ��ȥ�ǡ���

      real(8)  :: q(jm/2*7)                ! �Ѵ��Ѻ������
      real(8)  :: ws(nn+1)                 ! �Ѵ��Ѻ������

      call ljcszy(nn,l_data,l_ydata,c)
      call ljtszg(nm,nn+1,jm,l_ydata,y_GradLat_l,p,q,r,ws,1)

    end function y_GradLat_l

    function l_DivLat_y(y_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�. 
      !
      real(8)              :: l_DivLat_y(0:nn)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: y_data(1:jm)
      !(in) ���ϳʻ����ǡ���

      real(8)              :: l_ydata(0:nn+1)
      ! ��ȥ��ڥ��ȥ�ǡ���

      real(8)  :: q(jm/2*7)                ! �Ѵ��Ѻ������
      real(8)  :: ws(nn+1)                 ! �Ѵ��Ѻ������

      call ljtgzs(nm,nn+1,jm,l_ydata,y_data,p,q,r,ws,1)
      call ljcyzs(nn,l_ydata,l_DivLat_y,c)

    end function l_DivLat_y

  !--------------- ��ʬ�׻� (�̺�ɸ����) -----------------
    function y_GradMu_l(l_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�. 
      !
      real(8)              :: y_GradMu_l(1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: l_data(0:nm)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      y_GradMu_l = y_GradLat_l(l_data)*cos(y_Lat)

    end function y_GradMu_l

    function l_DivMu_y(y_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: l_DivMu_y(0:nm)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: y_data(1:jm)
      !(in) ���ϳʻ����ǡ���

      l_DivMu_y = l_DivLat_y(y_data/cos(y_Lat))

    end function l_DivMu_y

  !--------------- ��ʬ�׻� -----------------
    function IntLat_y(y_data)
      !
      ! 1 ��������(Y)�ʻ����ǡ����� Y ������ʬ.
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: y_data(1:jm)    !(in)  1 ��������(Y)�ʻ����ǡ���
      real(8)             :: IntLat_y        !(out) ��ʬ��

      IntLat_y = sum(y_data * y_Lat_weight)

    end function IntLat_y

  !--------------- ʿ�ѷ׻� -----------------
    function AvrLat_y(y_data)
      !
      ! 1 ����(Y)�ʻ����ǡ����ΰ���(Y)����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: y_data(1:jm)    !(in)  1 �������ٳʻ����ǡ���
      real(8)             :: AvrLat_y        !(out) ʿ����

      AvrLat_y = IntLat_y(y_data)/sum(y_Lat_weight)

    end function AvrLat_y

  !--------------- ��ַ׻� -----------------    
    function Interpolate_l(l_data,alat)
      !
      ! ���� alat �ˤ�����ؿ��ͤ�
      ! ���Υ른���ɥ��Ѵ����� l_data ������ַ׻�����
      !
      real(8), intent(IN) :: l_data(0:nn)           ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: alat                   ! ��֤������(����)
      real(8)             :: Interpolate_l          ! ��֤�����
      
      real(8) :: mu
      real(8) :: y0, y1, y2
      integer :: k

      mu = sin(alat)
      Interpolate_l = 0.0D0

      !---- ��a_n^0 L_n^0 �η׻�
      y2 = 0 ; y1 = 0
      do k=nn,1,-1
         y0 = alpha(k,mu) * y1 + beta(k+1)*y2 + l_data(k)
         y2 = y1 ; y1 = y0
      enddo
      Interpolate_l = beta(1) * y2 + mu*sqrt(3.0D0) * y1 + l_data(0) 

    end function Interpolate_l

  !--------------- �����롼���� -----------------
    function alpha(n,x)
      !
      !  �������� P_n �η���
      !
      integer, intent(IN) :: n
      real(8), intent(IN) :: x
      real(8)             :: alpha

      alpha = sqrt( (2.0D0*n+3)*(2.0D0*n+1)/((n+1)*(n+1)) ) * x
    end function alpha

    function beta(n)
      !
      !  �������� P_{n-1} �η���
      !
      integer, intent(IN) :: n
      real(8)             :: beta

      beta = - sqrt( (2.0D0*n+3)*n*n/((2*n-1)*(n+1)*(n+1)) )
    end function beta

end module l_module_sjpack
