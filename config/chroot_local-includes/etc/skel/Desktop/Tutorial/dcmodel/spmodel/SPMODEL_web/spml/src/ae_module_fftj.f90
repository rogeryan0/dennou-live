!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ae_module_fftj
!
!      spml/ae_module_fftj �⥸�塼��� 1 ���������������β��Ǥ�ή�α�ư��
!      �¥ա��ꥨ�Ѵ��ˤ�륹�ڥ��ȥ�ˡ�ǿ��ͷ׻����뤿��� Fortran90 �ؿ�
!      ���󶡤���.
!
!      2 �����ǡ����� 1 �����˴ؤ���Ʊ���˥��ڥ��ȥ�׻���¹Ԥ��뤿���
!      �ؿ����󶡤��Ƥ���, 2, 3 �����ΰ�Ǥη׻��Υ١������󶡤���.
!
!      ���Υ⥸�塼��������� fftj �� Fortran77 ���֥롼�����
!      �Ƥ�Ǥ���.
!
!      ���Ѵ����������ӥ��ڥ��ȥ�ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ� fftj ��
!      �ۤʤäƤ���Τǰʲ��Υ����Ȥ���դ��줿��. 
!
!
!����  2009/09/09  �ݹ�����  fftj �� Fortran 90 ��. ae_module.f90 ����¤
!
!++
module ae_module_fftj
  !
  != ae_module_fftj
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: ae_module_fftj.f90,v 1.2 2009-09-12 12:38:33 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  !
  ! spml/ae_module_fftj �⥸�塼��� 1 ���������������β��Ǥ�ή�α�ư��
  ! �¥ա��ꥨ�Ѵ��ˤ�륹�ڥ��ȥ�ˡ�ǿ��ͷ׻����뤿��� Fortran90 �ؿ�
  ! ���󶡤���.
  !
  ! 2 �����ǡ����� 1 �����˴ؤ���Ʊ���˥��ڥ��ȥ�׻���¹Ԥ��뤿���
  ! �ؿ����󶡤��Ƥ���, 2, 3 �����ΰ�Ǥη׻��Υ١������󶡤���.
  !
  ! ���Υ⥸�塼��������� fftj �� Fortran77 ���֥롼�����
  ! �Ƥ�Ǥ���.
  !
  ! ���Ѵ����������ӥ��ڥ��ȥ�ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ� fftj ��
  ! �ۤʤäƤ���Τǰʲ��Υ����Ȥ���դ��줿��. 
  !
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (e_, g_, ae_, ag_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   e_  :: ���ڥ��ȥ�ǡ���,
  !   g_  :: 1 �����ʻ����ǡ���,
  !   ae_ :: 1 �������ڥ��ȥ�ǡ�����ʣ���¤�� 2 �����ǡ���,
  !   ag_ :: 1 �����ʻ����ǡ�����ʣ���¤�� 2 �����ǡ���.
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dx)��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_e, _ae, _g, _ag) ��, �����ѿ��η����ڥ��ȥ�ǡ��������
  !   �ʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _e  :: ���ڥ��ȥ�ǡ���
  !   _g  :: 1 �����ʻ����ǡ���
  !   _ae :: 1 �������ڥ��ȥ�ǡ�����ʣ���¤�� 2 �����ǡ���
  !   _ag :: 1 �����ʻ����ǡ�����ʣ���¤�� 2 �����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * g : 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1).
  !   * im �� X ��ɸ�γʻ������Ǥ���, ���֥롼���� ae_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * e : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km).
  !   * km �� X �����κ����ȿ��Ǥ���, ���֥롼���� ae_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������� 0:km �ޤǤ� cos(kx) �η���, 
  !     -km:-1 �� sin(kx) �η����ȤʤäƤ���.
  !
  ! * ag : 1 ����(X)�ʻ����ǡ������¤�� 2 �����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,0:im-1).
  !     �� 2 ������ X ������ɽ��.
  !
  ! * ae : 1 �������ڥ��ȥ�ǡ������¤�� 2 �����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,-km:km).
  !     �� 2 ���������ڥ��ȥ��ɽ��.
  !
  ! * g_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * e_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * ag_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ������¤��
  !   2 �����ǡ�����Ʊ��.
  !
  ! * ae_ �ǻϤޤ�ؿ����֤��ͤ� 1 �������ڥ��ȥ�ǡ������¤��
  !   2 �����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������ΤΤ��ȤǤ���.
  !
  !== �ѿ�����³����������
  !
  !==== �����
  !
  ! ae_Initial       :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  !
  !==== ��ɸ�ѿ�
  !
  ! g_X              :: �ʻ�����ɸ(X)���Ǽ���� 1 ��������
  ! g_X_Weight       :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  !
  !==== �����Ѵ�
  !
  ! g_e, ag_ae       :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! e_g, ae_ag       :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! e_Dx_e, ae_Dx_ae :: ���ڥ��ȥ�ǡ����� X ��ʬ����Ѥ�����
  !
  !==== ��ʬ��ʿ��
  !
  ! a_Int_ag, a_Avr_ag :: 1 �����ʻ����ǡ������¤�� 2 �����������ʬ�����ʿ��
  ! Int_g, Avr_g       :: 1 �����ʻ����ǡ�������ʬ�����ʿ��
  !
  !
  use dc_message
  implicit none

  private
  public ae_Initial                       ! ������롼����
  public ag_ae, ae_ag, g_e, e_g           ! �����Ѵ�
  public ae_Dx_ae, e_Dx_e                 ! ��ʬ
  public a_Int_ag, Int_g, a_Avr_ag, Avr_g ! ��ʬ��ʿ��
  public g_X, g_X_Weight                  ! ��ɸ�ѿ�

  integer, parameter :: index2max=11      ! Max(im) = 2048 = 2^{11}
  integer            :: im=32             ! �ʻ����ο�
  integer            :: km=10             ! �����ȿ�
  double precision   :: xl=1.0            ! �ΰ���礭��

  real(8), parameter                 :: pi=3.1415926535897932385D0

  real(8), allocatable :: g_x(:)         ! �ʻ�����ɸ(X)���Ǽ���� 1 ��������.
  real(8), allocatable :: g_x_weight(:)  ! �Ťߺ�ɸ���Ǽ���� 1 ��������.
                                         ! X �����γʻ����δֳ֤���Ǽ���Ƥ���.

  save im, km, xl, g_X, g_X_Weight

  contains
  !--------------- ����� -----------------
    subroutine ae_Initial(i,k,xmin,xmax)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭�������ꤹ��.
      !
      ! ¾�δؿ����ѿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
      ! �������򤷤ʤ���Фʤ�ʤ�.
      !
      integer,intent(in) :: i              ! �ʻ����ο�
      integer,intent(in) :: k              ! �����ȿ�
      real(8),intent(in) :: xmin, xmax     ! X ��ɸ�ϰ�

      integer :: ii, index2

      im = i
      km = k
      xl = xmax-xmin

      index2 = 1
      do while ( im /= 2**index2 .AND. index2 <= index2max ) 
         index2=index2+1
      enddo

      if ( index2 > index2max ) then
         call MessageNotify('E','ae_Initial', &
              'Number of grid points should be 2^N and <= 2048')
      else if ( km <= 0 ) then
         call MessageNotify('E','ae_Initial', &
              'Number of waves should be positive')
      elseif ( km >= im/2 ) then
         call MessageNotify('E','ae_Initial', &
              'KM shoud be less than IM/2')
      endif

      allocate(g_x(0:im-1))
      do ii=0,im-1
         g_X(ii) = xmin + xl/im*ii
      enddo

      allocate(g_x_weight(0:im-1))
      g_X_Weight = xl/im

      call MessageNotify(&
        'M','ae_initial','ae_module_fftj (2009/09/12) is initialized')

    end subroutine ae_Initial

  !--------------- �����Ѵ� -----------------

    function ag_ae(ae)
      !
      ! ���ڥ��ȥ�ǡ�������ʻ����ǡ����ص��Ѵ�����(2 �����ǡ�����)
      !
      ! ���ڥ��ȥ���Ѵ�������ϰʲ��ΤȤ���. 
      !
      !   ���ڥ��ȥ�ǡ��� e_k (k=-km,...,km)
      !   �ʻ����ǡ���     g_j (j=-0,...,im-1)
      !
      !  g_j = e_0 + 
      !      + 2\sum_{k=1}^{km}(e_k\cos(2\pi jk/im) - e_{-k}\sin(2\pi jk/im))
      !                                                    (j=0,1,...,im-1).
      !                                                                
      !
      real(8), dimension(:,-km:), intent(in)  :: ae     !(in)  ���ڥ��ȥ�ǡ���
      real(8), dimension(size(ae,1),0:im-1)   :: ag_ae  !(out) �ʻ����ǡ���

      real(8), dimension(size(ae,1),0:im-1)   :: ag_in  ! ���ϥ��ڥ��ȥ�ǡ���
      real(8), dimension(im)                  :: g_work
      real(8), dimension(im*3)                :: xt
      integer, dimension(2)                   :: ip
      integer :: m, k

      if ( im > 2048 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data exceeds 2048.')
      else if ( mod(im,2) /= 0 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data should be 2^N.')
      else if ( size(ae,2) < 2*km+1 ) then
         call MessageNotify('E','ag_ae', &
              'The Fourier dimension of input data too small.')
      elseif ( size(ae,2) > 2*km+1 ) then
         call MessageNotify('W','ag_ae', &
              'The Fourier dimension of input data too large.')
      endif

      ag_in = 0.0D0
      ag_in(:,0)=ae(:,0)
      ag_in(:,1)=0
      do k=1,km
         ag_in(:,2*k)=ae(:,k)
         ag_in(:,2*k+1)=ae(:,-k)
      enddo
      ag_in(:,2*km+2:im-1)=0

      call fjrini(im,2,1,ip,xt)
      do m=1,size(ae,1)
         call fjrrun(ag_in(m,:),ag_ae(m,:),g_work,xt,ip)
      enddo

    end function ag_ae

    function g_e(e)
      !
      ! ���ڥ��ȥ�ǡ�������ʻ����ǡ����ص��Ѵ�����(1 �����ǡ�����)
      !
      ! ���ڥ��ȥ���Ѵ�������ϰʲ��ΤȤ���. 
      !
      !   ���ڥ��ȥ�ǡ��� e_k (k=-km,...,km)
      !   �ʻ����ǡ���     g_j (j=-0,...,im-1)
      !
      !  g_j = e_0 + 
      !      + 2\sum_{k=1}^{km}(e_k\cos(2\pi jk/im) - e_{-k}\sin(2\pi jk/im))
      !                                                    (j=0,1,...,im-1).
      !
      real(8), dimension(0:im-1)             :: g_e  !(out) �ʻ����ǡ���
      real(8), dimension(-km:km), intent(in) :: e    !(in)  ���ڥ��ȥ�ǡ���

      real(8), dimension(1,size(e))  :: ae_work
      real(8), dimension(1,0:im-1)   :: ag_work

      ae_work(1,:) = e
      ag_work = ag_ae(ae_work)
      g_e = ag_work(1,:)

    end function g_e

    function ae_ag(ag)
      !
      ! �ʻ����ǡ������饹�ڥ��ȥ�ǡ��������Ѵ�����(2 �����ǡ�����)
      !
      ! ���ڥ��ȥ����Ѵ�������ϰʲ��ΤȤ���. 
      !
      !   �ʻ����ǡ���     g_j (j=-0,...,im-1)
      !   ���ڥ��ȥ�ǡ��� e_k (k=-km,...,km)
      !
      !   e_0 = (1/im)\sum_{j=0}^{im-1} g_j
      !   e_k = (1/im)\sum_{j=0}^{im-1} g_j \cos(2\pi jk/im)       (k=1,2,...,km)
      !   e_{-k} = - (1/im)\sum_{j=0}^{im-1} g_j \sin(2\pi jk/im)  (k=1,2,...,km)
      !
      real(8), dimension(:,:), intent(in)     :: ag     !(in)  �ʻ����ǡ���
      real(8), dimension(size(ag,1),-km:km)   :: ae_ag  !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(size(ag,1),0:im-1)   :: ag_out ! ���ڥ��ȥ�ǡ���

      real(8), dimension(im)                  :: g_work
      real(8), dimension(im*3)                :: xt
      integer, dimension(2)                   :: ip
      integer :: m, k

      if ( im > 2048 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data exceeds 2048.')
      else if ( mod(im,2) /= 0 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data should be 2^N.')
      else if ( size(ag,2) < im ) then
         call MessageNotify('E','ae_ag', &
              'The Grid points of input data too small.')
      elseif ( size(ag,2) > im ) then
         call MessageNotify('W','ae_ag', &
              'The Grid points of input data too large.')
      endif

      call fjrini(im,2,2,ip,xt)
      do m=1,size(ag,1)
         call fjrrun(ag(m,:),ag_out(m,:),g_work,xt,ip)
      enddo

      do k=1,km
         ae_ag(:,k)  = ag_out(:,2*k)
         ae_ag(:,-k) = ag_out(:,2*k+1)
      enddo
      ae_ag(:,0) = ag_out(:,0)

      ae_ag = ae_ag/im

    end function ae_ag

    function e_g(g)
      !
      ! �ʻ����ǡ������饹�ڥ��ȥ�ǡ��������Ѵ�����(1 �����ǡ�����)
      !
      ! ���ڥ��ȥ����Ѵ�������ϰʲ��ΤȤ���. 
      !
      !   �ʻ����ǡ���     g_j (j=-0,...,im-1)
      !   ���ڥ��ȥ�ǡ��� e_k (k=-km,...,km)
      !
      !   e_0 = (1/im)\sum_{j=0}^{im-1} g_j
      !   e_k = (1/im)\sum_{j=0}^{im-1} g_j \cos(2\pi jk/im)       (k=1,2,...,km)
      !   e_{-k} = - (1/im)\sum_{j=0}^{im-1} g_j \sin(2\pi jk/im)  (k=1,2,...,km)
      !
      real(8), dimension(-km:km)              :: e_g   !(out) ���ڥ��ȥ�ǡ���
      real(8), dimension(0:im-1), intent(in)  :: g     !(in)  �ʻ����ǡ���

      real(8), dimension(1,size(g))        :: ag_work
      real(8), dimension(1,-km:km)         :: ae_work

      ag_work(1,:) = g
      ae_work = ae_ag(ag_work)
      e_g = ae_work(1,:)

    end function e_g

  !--------------- ��ʬ�׻� -----------------
    function ae_Dx_ae(ae)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ����Ѥ���(2 �����ǡ���).
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      !
      real(8), dimension(:,-km:), intent(in)  :: ae
                                         !(in)  ���ϥ��ڥ��ȥ�ǡ���
      real(8), dimension(size(ae,1),-km:km)   :: ae_dx_ae
                                         !(out) ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ

      integer k

      if ( size(ae,2) < 2*km+1 ) then
         call MessageNotify('W','ae_Dx_ae', &
              'The Fourier dimension of input data too small.')
      elseif ( size(ae,2) > 2*km+1 ) then
         call MessageNotify('W','ae_Dx_ae', &
              'The Fourier dimension of input data too large.')
      endif

      do k=-km,km
         ae_Dx_ae(:,k) = -(2*pi*k/xl)*ae(:,-k)
      enddo
    end function ae_dx_ae

    function e_Dx_e(e)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ����Ѥ���(1 �����ǡ���).
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      !
      real(8), dimension(-km:km), intent(in)     :: e
                                         !(in)  ���ϥ��ڥ��ȥ�ǡ���
      real(8), dimension(-km:km)                 :: e_Dx_e
                                         !(out) ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(1,-km:km)               :: ae_work

      ae_work(1,:) = e
      ae_work = ae_Dx_ae(ae_work)
      e_Dx_e = ae_work(1,:)

    end function e_Dx_e

  !--------------- ��ʬ�׻� -----------------
    function a_Int_ag(ag)
      !
      ! 1 �����ʻ����ǡ������¤�� 2 �����������ʬ
      !
      real(8), dimension(:,0:), intent(in)     :: ag        !(in) �ʻ����ǡ���
      real(8), dimension(size(ag,1))           :: a_Int_ag  !(out) ��ʬ���
      integer :: i

      if ( size(ag,2) < im ) then
         call MessageNotify('E','ae_Int_ag', &
              'The Grid points of input data too small.')
      elseif ( size(ag,2) > im ) then
         call MessageNotify('W','ae_Int_ag', &
              'The Grid points of input data too large.')
      endif

      a_Int_ag = 0.0d0
      do i=0,im-1
         a_Int_ag(:) = a_Int_ag(:) + ag(:,i)*g_X_Weight(i)
      enddo
    end function a_Int_ag

    function Int_g(g)
      !
      ! 1 �����ʻ����ǡ�������ʬ
      !
      real(8), dimension(0:im-1), intent(in)   :: g      !(in) �ʻ����ǡ���
      real(8)                                  :: Int_g  !(out) ��ʬ���

      Int_g = sum(g*g_X_Weight)

    end function Int_g

    function a_Avr_ag(ag)
      !
      ! 1 �����ʻ����ǡ������¤�� 2 ���������ʿ��
      !
      real(8), dimension(:,0:), intent(in)     :: ag        !(in) �ʻ����ǡ���
      real(8), dimension(size(ag,1))           :: a_Avr_ag  !(out) ʿ�ѷ��

      a_Avr_ag = a_Int_ag(ag)/sum(g_X_Weight)

    end function a_Avr_ag

    function Avr_g(g)
      !
      ! 1 �����ʻ����ǡ�����ʿ��
      !
      real(8), dimension(0:im-1), intent(in)   :: g
      real(8)                                  :: Avr_g

      Avr_g = Int_g(g)/sum(g_X_Weight)

    end function Avr_g

end module ae_module_fftj
