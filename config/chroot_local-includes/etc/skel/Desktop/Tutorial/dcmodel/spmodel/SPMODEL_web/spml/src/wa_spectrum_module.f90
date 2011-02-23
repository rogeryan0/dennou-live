!--
!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_spectrum_module
!
!  spml/wa_spectrum_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, 
!  ���ڥ��ȥ���Ϸ׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_spectrum_module �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!  �Ф����Ѵ����Ԥ���.
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2005/04/23  �ݹ����� 
!      2005/05/16  �ݹ����� �ؿ�̾��û��
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2008/06/28  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!
!++
module wa_spectrum_module
  !
  != wa_spectrum_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_spectrum_module.f90,v 1.7 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wa_spectrum_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, 
  ! ���ڥ��ȥ���Ϸ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ���̾�� 1 �إ�ǥ��� w_spectrum_module �⥸�塼���¿�إ�ǥ��Ѥ�
  ! ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  ! �Ф����Ѵ����Ԥ���.
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use w_base_module, only : nm, l_nm

  implicit none

  private
 
  public nma_EnergyFromStreamfunc_wa      ! ���ͥ륮�����ڥ��ȥ�
                                          ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public na_EnergyFromStreamfunc_wa       ! ���ͥ륮�����ڥ��ȥ�
                                          ! (��ʿ���ȿ� n ����)
  public nma_EnstrophyFromStreamfunc_wa   ! ���󥹥ȥ�ե������ڥ��ȥ�
                                          ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public na_EnstrophyFromStreamfunc_wa    ! ���󥹥ȥ�ե������ڥ��ȥ�
                                          !  (��ʿ���ȿ� n ����)
  public wa_spectrum_VMiss                ! ��»��

  real(8) :: wa_spectrum_VMiss = -999.000 ! ��»�ͽ����

  contains

  !--------------- ���ͥ륮�����ڥ��ȥ�׻� -----------------
    function nma_EnergyFromStreamfunc_wa(wa_Strfunc)
      ! 
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�ͥ륮���ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(¿����).
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�� (1/2)n(n+1)��(n,m)^2 �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�4�Ф򤫤�����Τ����̾�Ǥ�
      !    �����ͥ륮����������.
      !
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    ��»�ͤ��ͤϥ⥸�塼���ѿ� w_spectrum_VMiss �ˤ�ä�����Ǥ���
      !    (����ͤ� -999.0)
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnergyFromStreamfunc_wa
      !(out) ���ͥ륮�����ڥ��ȥ�(��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer n,m                             ! DO �ѿ�

      nma_EnergyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         do m=-n,n
            nma_EnergyFromStreamfunc_wa(n,m,:) &
                 = 0.5 * n*(n+1) * wa_Strfunc(l_nm(n,m),:)**2
         enddo
      enddo
    end function nma_EnergyFromStreamfunc_wa

    function na_EnergyFromStreamfunc_wa(wa_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(¿����).
      !
      !  * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n(n+1)��(n,m)^2 
      !    �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4�Ф򤫤�����Τ�
      !    ���̾�Ǥ������ͥ륮����������.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnergyFromStreamfunc_wa
      !(out) ���ͥ륮�����ڥ��ȥ� (��ʿ���ȿ� n ����) 

      integer n,m                            ! DO �ѿ�
  
      do n=0,nm
         na_EnergyFromStreamfunc_wa(n,:)  &
              = 0.5 * n*(n+1) &
                * sum(wa_StrFunc(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)
      enddo

    end function na_EnergyFromStreamfunc_wa

  !--------------- ���󥹥ȥ�ե������ڥ��ȥ�׻� -----------------

    function nma_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�󥹥ȥ�ե����ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(¿����). 
      !
      ! * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���󥹥ȥ�ե������ڥ��ȥ�� (1/2)n^2(n+1)^2��(n,m)^2 �ȷ׻������.
      !
      ! * ���ƤΥ��󥹥ȥ�ե������ڥ��ȥ���ʬ���¤�4��/R^2�򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥ�ե�����������. ������ R �ϵ��̤�Ⱦ�¤Ǥ���.
      !
      ! * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !   ��»�ͤ��ͤϥ⥸�塼���ѿ� w_spectrum_VMiss �ˤ�ä�����Ǥ���
      !   (����ͤ� -999.0)
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnstrophyFromStreamfunc_wa
      ! ���󥹥ȥ�ե������ڥ��ȥ� (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer n,m                             ! DO �ѿ�

      nma_EnstrophyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         do m=-n,n
            nma_EnstrophyFromStreamfunc_wa(n,m,:) &
                 = 0.5 * n**2 * (n+1)**2 &
                    * wa_Strfunc(l_nm(n,m),:)**2

         enddo
      enddo
    end function nma_EnstrophyFromStreamfunc_wa

    function na_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(¿����)
      !
      ! * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ���饨�󥹥ȥ�ե���
      !   ���ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n^2(n+1)^2��(n,m)^2 �ȷ׻������.
      !    
      ! * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4��/R^2 �򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥե�����������.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnstrophyFromStreamfunc_wa      ! ���󥹥ȥ�ե������ڥ��ȥ�
      !(out) ���󥹥ȥ�ե������ڥ��ȥ�(��ʿ���ȿ� n ����)

      integer n,m                                ! DO �ѿ�

      do n=0,nm
         na_EnstrophyFromStreamfunc_wa(n,:)  &
              = 0.5 * n**2 * (n+1)**2 &
                * sum(wa_StrFunc(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)
      enddo

    end function na_EnstrophyFromStreamfunc_wa

end module wa_spectrum_module
