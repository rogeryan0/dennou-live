!--
!----------------------------------------------------------------------
!     Copyright (c) 2007-2009 SPMODEL Development Group.
!----------------------------------------------------------------------
!ɽ��  w_interpolate_module
!
!  spml/w_interpolate_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� w_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ�ˤ��
!  ��ַ׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ��ַ׻�����ˡ�ˤĤ��Ƥ� doc/w_module.tex �򻲾ȤΤ���. 
!  ���Υ��֥롼����������� ISPACK �� SPPACK �� SNPACK �Υ��֥롼�����
!  �Ƥ�Ǥ��ʤ�. 
!
!����  2007/10/26  �ݹ����� ��������
!      2007/10/28  �ݹ����� �ؿ�̾�ѹ�
!      2007/11/01  �ݹ����� ��������ؿ��ˤ���. 
!      2008/06/25 ��������ʿ �����Ȥ� RDOC �Ѥ˽���
!      2009/01/04  �ݹ����� spml ��ˡ��ȿ���뤿��ʣ��������б��Ǻ��
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/02/09  �ݹ����� P_m^m �η���ȯ���ɻ�
!
!      ����
!         ���Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
!++
module w_interpolate_module
  !
  !=  w_interpolate_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_interpolate_module.f90,v 1.9 2009-02-28 21:33:47 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/w_deriv_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� w_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ�ˤ��
  ! ��ַ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : nm, l_nm
  implicit none
  private

  public Interpolate_w                        ! ��ִؿ�

  interface Interpolate_w
     !
     ! ���� alon, ���� alat �ˤ�����ؿ��ͤ�
     ! ���ε���Ĵ���Ѵ����� w_data ������ַ׻�����
     !
     ! ���Ϥ�����ٷ��ٺ�ɸ��, 
     !       ����, ����1������ʣ����, ����ʣ��������1��, ���ٰ���ʣ����
     ! �� 4 ����
     !
     module procedure Interpolate_array00_w
  end interface

  interface alpha
     module procedure alpha_array0
  end interface

  interface Pmm
     module procedure Pmm_array0
  end interface

  contains

  !--------------- ��ַ׻� -----------------
    function Interpolate_array00_w(w_data,alon,alat)
      !
      ! ���� alat, ���� alon �ˤ�����ؿ��ͤ�
      ! ���ε���Ĵ���Ѵ����� w_data ������ַ׻�����
      !
      real(8), intent(IN) :: w_data((nm+1)*(nm+1))  ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: alon                   ! ��֤������(����)
      real(8), intent(IN) :: alat                   ! ��֤������(����)
      real(8)             :: Interpolate_array00_w  ! ��֤�����
      
      real(8) :: mu
      real(8) :: y0, y1, y2, AnmPnm
      integer :: k,m

      mu = sin(alat)
      Interpolate_array00_w = 0.0D0

      !---- ��a_n^0 P_n^0 �η׻�
      y2 = 0 ; y1 = 0
      do k=nm,1,-1
         y0 = alpha(k,0,mu) * y1 + beta(k+1,0)*y2 + w_data(l_nm(k,0))
         y2 = y1 ; y1 = y0
      enddo
      Interpolate_array00_w = (  beta(1,0) * y2 + mu*sqrt(3.0D0) * y1 &
                       + w_data(l_nm(0,0))  ) * Pmm(0,mu)

      !---- cos m�� ��a_n^m P_n^m �η׻�
      do m=1,nm
         y2 = 0 ; y1 = 0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m) * y2 + w_data(l_nm(k,m))
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(w_data(l_nm(m,m)) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)

         Interpolate_array00_w = Interpolate_array00_w &
                                 + AnmPnm*sqrt(2.0D0)*cos(m*alon)
      end do

      !---- sin �� ��a_n^m P_n^m �η׻�
      do m=1,nm
         y2 = 0 ; y1 = 0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m)*y2 + w_data(l_nm(k,-m))
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(w_data(l_nm(m,-m)) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)


         Interpolate_array00_w = Interpolate_array00_w &
                                 - AnmPnm*sqrt(2.0D0)*sin(m*alon)
      end do
      
    end function Interpolate_array00_w

  !--------------- �����롼���� -----------------
    function alpha_array0(n,m,x)
      !
      !  �������� P_n^m �η���
      !
      integer, intent(IN) :: n,m 
      real(8), intent(IN) :: x
      real(8)             :: alpha_array0

      alpha_array0 = sqrt( (2.0D0*n+3)*(2.0D0*n+1)/((n-m+1)*(n+m+1)) ) * x
    end function alpha_array0

    function beta(n,m)
      !
      !  �������� P_{n-1}^m �η���
      !
      integer, intent(IN) :: n,m 
      real(8)             :: beta

      beta = - sqrt( (2.0D0*n+3)*(n+m)*(n-m)/((2*n-1)*(n+m+1)*(n-m+1)) )
    end function beta

    function Pmm_array0(m,x)
      !
      ! �른���ɥ���ȡ�� P_m^m(x) �η׻�
      !         sqrt( factrl(2*m+1) )/(2.0D0**m * factrl(m)) &
      !          * (1-x**2)**(m/2.0D0)
      !
      ! ������ȯ�����ʤ��褦���п��Ƿ׻�����
      !
      integer, intent(IN) :: m           ! �른���ɥ���ȡ���μ���
      real(8), intent(IN) :: x           ! ������
      real(8)             :: Pmm_array0  ! �른���ɥ���ȡ������
      real(8)             :: gammaln     ! ���ؿ����п�

      if ( m < 0 ) call MessageNotify('E','Pmm in w_Intepolate_module',&
                                'order m should be larger equal to zero')

      Pmm_array0 = 1.0
      if ( m > 0 )then
            Pmm_array0 = &
                 exp(gammaln(2.0D0*m+2.0)/2.0D0 - m*log(2.0D0) - gammaln(m+1.0D0)) &
                 * (1-x**2)**(m/2.0D0)
      endif
    end function Pmm_array0

end module w_interpolate_module
