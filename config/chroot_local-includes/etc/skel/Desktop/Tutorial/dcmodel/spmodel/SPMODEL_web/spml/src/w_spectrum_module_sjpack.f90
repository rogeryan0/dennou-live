!--
!----------------------------------------------------------------------
! Copyright(C) 2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_spectrum_module_sjpack
!
!  spml/w_spectrum_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� w_module_sjpack �β����⥸�塼��Ǥ���, 
!  ���ڥ��ȥ���Ϸ׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ���ڥ��ȥ�׻�����ˡ�ˤĤ��Ƥ� doc/w_module_sjpack.tex �򻲾ȤΤ���. 
!  ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2009/09/04  �ݹ����� w_spectrum_module ���¤, SJPACK �б�
!      2009/09/23  �ݹ����� nm_* �η׻�����
!
!++
module w_spectrum_module_sjpack
  !
  !=  w_spectrum_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_spectrum_module_sjpack.f90,v 1.2 2009-09-23 06:35:59 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/w_spectrum_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� w_module_sjpack �β����⥸�塼��Ǥ���, 
  ! ���ڥ��ȥ���Ϸ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ���ڥ��ȥ�׻�����ˡ�ˤĤ��Ƥ� doc/w_module.tex �򻲾ȤΤ���. 
  ! ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use w_base_module_sjpack, only : nm=>nn, l_nm

  implicit none

  private
 
  public nm_EnergyFromStreamfunc_w          ! ���ͥ륮�����ڥ��ȥ�           
                                            ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnergyFromStreamfunc_w           ! ���ͥ륮�����ڥ��ȥ�
                                            ! (��ʿ���ȿ� n ����) 
  public nm_EnstrophyFromStreamfunc_w       ! ���󥹥ȥ�ե������ڥ��ȥ�     
                                            ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnstrophyFromStreamfunc_w        ! ���󥹥ȥ�ե������ڥ��ȥ�  
                                            !  (��ʿ���ȿ� n ����)
  public w_spectrum_VMiss                   ! ��»��

  real(8) :: w_spectrum_VMiss = -999.000    ! ��»�ͽ����

  contains

  !--------------- ���ͥ륮�����ڥ��ȥ�׻� -----------------
    function nm_EnergyFromStreamfunc_w(w_Strfunc)
      ! 
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�ͥ륮���ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(1 ����).
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�� (1/2)n(n+1)|��(n,m)|^2 �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�4�Ф򤫤�����Τ����̾�Ǥ�
      !    �����ͥ륮����������.
      !
      !  * ���ڥ��ȥ�� (n,m) ��ʬ�����ȿ� n, �����ȿ� m �μ�����ʬ, 
      !    (n,-m) ��ʬ�����ȿ� n, �����ȿ� m �ε�����ʬ����Ǽ����Ƥ���.
      !
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    ��»�ͤ��ͤϥ⥸�塼���ѿ� w_spectrum_VMiss �ˤ�ä�����Ǥ���
      !    (����ͤ� -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnergyFromStreamfunc_w
      !(out) ���ͥ륮�����ڥ��ȥ�(��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer n,m                               ! DO �ѿ�

      nm_EnergyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         nm_EnergyFromStreamfunc_w(n,0) &
              = 0.5 * n*(n+1) * w_Strfunc(l_nm(n,0))**2
         do m=1,n
            nm_EnergyFromStreamfunc_w(n,m) &
                 = 0.5 * n*(n+1) &
                 * (w_Strfunc(l_nm(n,m))**2+w_Strfunc(l_nm(n,-m))**2)
            nm_EnergyFromStreamfunc_w(n,-m) &
                 = nm_EnergyFromStreamfunc_w(n,m)
         enddo
      enddo
    end function nm_EnergyFromStreamfunc_w

    function n_EnergyFromStreamfunc_w(w_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(1 ����).
      !
      !  * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n(n+1)|��(n,m)|^2 
      !    �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4�Ф򤫤�����Τ�
      !    ���̾�Ǥ������ͥ륮����������.
      !

      real(8), intent(in)      :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm) :: n_EnergyFromStreamfunc_w
      !(out) ���ͥ륮�����ڥ��ȥ� (��ʿ���ȿ� n ����) 

      integer n,m                                 ! DO �ѿ�

      n_EnergyFromStreamfunc_w = 0.0D0
      do n=0,nm
         n_EnergyFromStreamfunc_w(n) = &
                0.5 * n*(n+1) * w_StrFunc(l_nm(n,0))**2
         do m=1,n
            n_EnergyFromStreamfunc_w(n) = n_EnergyFromStreamfunc_w(n)+ &
                2 * 0.5 * n*(n+1) &
                * (w_StrFunc(l_nm(n,m))**2+w_StrFunc(l_nm(n,-m))**2)
         enddo
      enddo

    end function n_EnergyFromStreamfunc_w

  !--------------- ���󥹥ȥ�ե������ڥ��ȥ�׻� -----------------
    function nm_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�󥹥ȥ�ե����ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(1 ����). 
      !
      ! * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���󥹥ȥ�ե������ڥ��ȥ�� (1/2)n^2(n+1)^2|��(n,m)|^2 �ȷ׻������.
      !
      ! * ���ƤΥ��󥹥ȥ�ե������ڥ��ȥ���ʬ���¤�4��/R^2�򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥ�ե�����������. ������ R �ϵ��̤�Ⱦ�¤Ǥ���.
      !
      ! * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !   ��»�ͤ��ͤϥ⥸�塼���ѿ� w_spectrum_VMiss �ˤ�ä�����Ǥ���
      !   (����ͤ� -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnstrophyFromStreamfunc_w
      ! ���󥹥ȥ�ե������ڥ��ȥ� (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer n,m                               ! DO �ѿ�

      nm_EnstrophyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         nm_EnstrophyFromStreamfunc_w(n,0) &
                 = 0.5 * n**2 * (n+1)**2 * w_Strfunc(l_nm(n,0))**2 
         do m=1,n
            nm_EnstrophyFromStreamfunc_w(n,m) &
                 = 0.5 * n**2 * (n+1)**2 * &
                 (w_Strfunc(l_nm(n,m))**2 +w_Strfunc(l_nm(n,-m))**2 )
            nm_EnstrophyFromStreamfunc_w(n,-m) &
                 = nm_EnstrophyFromStreamfunc_w(n,m)
         enddo
      enddo
    end function nm_EnstrophyFromStreamfunc_w

    function n_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(1 ����)
      !
      ! * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ���饨�󥹥ȥ�ե���
      !   ���ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n^2(n+1)^2|��(n,m)|^2 �ȷ׻������.
      !    
      ! * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4��/R^2 �򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥե�����������.
      !
      real(8), intent(in)      :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm) :: n_EnstrophyFromStreamfunc_w  
      !(out) ���󥹥ȥ�ե������ڥ��ȥ�(��ʿ���ȿ� n ����)

      integer n,m                                ! DO �ѿ�

      do n=0,nm
         n_EnstrophyFromStreamfunc_w(n)  &
              = 0.5 * n**2 * (n+1)**2 * w_StrFunc(l_nm(n,0))**2
         do m=1,n
            n_EnstrophyFromStreamfunc_w(n) &
               =  n_EnstrophyFromStreamfunc_w(n) &
               + 2* 0.5 * n**2 * (n+1)**2 &
                * (w_StrFunc(l_nm(n,m))**2+w_StrFunc(l_nm(n,-m))**2)
         enddo
      enddo
    end function n_EnstrophyFromStreamfunc_w

end module w_spectrum_module_sjpack
