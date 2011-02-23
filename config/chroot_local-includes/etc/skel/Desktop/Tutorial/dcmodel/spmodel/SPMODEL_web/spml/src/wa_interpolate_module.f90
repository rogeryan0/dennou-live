!--
!----------------------------------------------------------------------
!     Copyright (c) 2007-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_interpolate_module
!
!  spml/wa_interpolate_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ�ˤ��
!  ��ַ׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_interpolate_module �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ������Ѥ���
!  ��ַ׻����Ԥ���.
!
!  ��ַ׻�����ˡ�ˤĤ��Ƥ� doc/w_module.tex �򻲾ȤΤ���. 
!  ���Υ��֥롼����������� ISPACK �� SPPACK �� SNPACK �Υ��֥롼�����
!  �Ƥ�Ǥ��ʤ�. �������ä�Ʊ���ˤ��Ĥ����륹�ڥ��ȥ�ǡ����ο���
!  wa_Initial �����ꤷ�� km ����礭���Ƥ��ɤ�. 
!
!����  2007/10/31  �ݹ����� ��������
!      2008/06/28  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/01/04  �ݹ����� spml ��ˡ��ȿ���뤿��ʣ��������б��Ǻ��
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/07/30  �ݹ�����   DO �ѿ����������ѹ�(for OpenMP)
!
!      ����
!      * �Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
!++
module wa_interpolate_module
  !
  != wa_interpolate_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_interpolate_module.f90,v 1.8 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wa_interpolate_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ�ˤ��
  ! ��ַ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ���̾�� 1 �إ�ǥ��� w_interpolate_module �⥸�塼���¿�إ�ǥ��Ѥ�
  ! ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ������Ѥ���
  ! ��ַ׻����Ԥ���.
  !
  ! ��ַ׻�����ˡ�ˤĤ��Ƥ� doc/w_module.tex �򻲾ȤΤ���. 
  ! ���Υ��֥롼����������� ISPACK �� SPPACK �� SNPACK �Υ��֥롼�����
  ! �Ƥ�Ǥ��ʤ�. �������ä�Ʊ���ˤ��Ĥ����륹�ڥ��ȥ�ǡ����ο���
  ! wa_Initial �����ꤷ�� km ����礭���Ƥ��ɤ�. 
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : nm, l_nm
  implicit none
  private

  public a_Interpolate_wa                        ! ��ִؿ�

  interface a_Interpolate_wa
     !
     ! ���� alon, ���� alat �ˤ�����ؿ��ͤ�
     ! ����Ĵ���Ѵ����� wa_data ������ַ׻�����
     !
     ! ���Ϥ�����ٷ��ٺ�ɸ�ϣ����� 1 ����
     !
     module procedure a_Interpolate_array00_wa
  end interface

  interface alpha
     module procedure alpha_array0
  end interface

  interface Pmm
     module procedure Pmm_array0
  end interface

  contains

  !--------------- ��ַ׻� -----------------
    function a_Interpolate_array00_wa(wa_data,alon,alat)
      !
      ! ���� alon, ���� alat �ˤ�����ؿ��ͤ�
      ! ���ε���Ĵ���Ѵ����� wa_data ������ַ׻�����
      !
      real(8), intent(IN) :: wa_data(:,:)             ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: alon                     ! ��֤������(����)
      real(8), intent(IN) :: alat                     ! ��֤������(����)
      real(8) :: a_Interpolate_array00_wa(size(wa_data,2))   ! ��֤�����
      
      real(8) :: mu
      real(8), dimension(size(wa_data,2)) :: y0, y1, y2, AnmPnm
      integer :: k,m

      mu = sin(alat)
      a_Interpolate_array00_wa = 0.0D0

      !---- ��a_n^0 P_n^0 �η׻�
      y2 = 0.0D0 ; y1 = 0.0D0
      do k=nm,1,-1
         y0 = alpha(k,0,mu) * y1 + beta(k+1,0)*y2 + wa_data(l_nm(k,0),:)
         y2 = y1 ; y1 = y0
      enddo
      a_Interpolate_array00_wa = (  beta(1,0) * y2 + mu*sqrt(3.0D0) * y1 &
                       + wa_data(l_nm(0,0),:)  ) * Pmm(0,mu)

      !---- cos m�� ��a_n^m P_n^m �η׻�
      do m=1,nm
         y2 = 0.0D0 ; y1 = 0.0D0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m) * y2 + wa_data(l_nm(k,m),:)
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(wa_data(l_nm(m,m),:) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)

         a_Interpolate_array00_wa = a_Interpolate_array00_wa &
                                 + AnmPnm*sqrt(2.0D0)*cos(m*alon)
      end do

      !---- sin �� ��a_n^m P_n^m �η׻�
      do m=1,nm
         y2 = 0.0D0 ; y1 = 0.0D0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m)*y2 + wa_data(l_nm(k,-m),:)
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(wa_data(l_nm(m,-m),:) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)


         a_Interpolate_array00_wa = a_Interpolate_array00_wa &
                                 - AnmPnm*sqrt(2.0D0)*sin(m*alon)
      end do
      
    end function a_Interpolate_array00_wa

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
      !
      integer, intent(IN) :: m           ! �른���ɥ���ȡ���μ���
      real(8), intent(IN) :: x           ! ������
      real(8)             :: Pmm_array0  ! �른���ɥ���ȡ������
      real(8)             :: factrl      ! ����(�����ؿ�)


      if ( m < 0 ) call MessageNotify('E','Pmm in wa_Intepolate_module',&
                                'order m should be larger equal to zero')

      Pmm_array0 = 1.0D0
      if ( m > 0 )then
            Pmm_array0 = &
                 sqrt( factrl(2*m+1) )/(2.0D0**m * factrl(m)) &
                 * (1-x**2)**(m/2.0D0)
      endif
    end function Pmm_array0

end module wa_interpolate_module
