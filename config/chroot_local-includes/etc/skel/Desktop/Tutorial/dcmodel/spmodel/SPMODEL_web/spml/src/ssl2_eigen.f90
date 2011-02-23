!--
!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ��ͭ�����ꥵ�֥롼���� (SSL2)
!
!����  2005/01/25  �ݹ�����
!      2006/03/08  �ݹ����� �����Ȥ� RDoc �Ѥ��ѹ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ��ѹ�
!
!++
module ssl2_eigen
  !
  != ssl2_eigen
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: ssl2_eigen.f90,v 1.4 2009-02-28 21:33:47 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  ! 
  !== ����
  !
  ! spml/ssl2_eigen �� eigmatrix �⥸�塼��θ������֥롼���� eigen
  ! �Ѥθ�ͭ�ͷ׻��Ѷ��̥��󥿡��ե�������Ϳ����.
  !
  !   * ���� AMAT �� i ���ܸ�ͭ�ͤ� eigen_r(i), eigen_i(i) �˳�Ǽ
  !   * �б������ͭ�٥��ȥ�� eigvec_r(:,i), eigvec_i(:,i) �˳�Ǽ
  !   * ��Ǽ�����ͭ�ͤο��ϰ��� eigen_r ���礭���Ƿ�ޤ�
  !   * ��ͭ�ͤν��֤� sort �� order ��������. 
  !   * sort �ˤ�äƽ��֤����뤿����Ѥ����̤���ꤹ��. 
  !     ����(R), ������������(RA), ����(I), ������������(IA)
  !   * reverse �ˤ�äƾ�������(.false.), �礭����(.true.)�����Ǥ���.
  !   * �ǥե���Ȥ� sort='R', reverse=.false.
  !
  ! �����Ǥ� DEIG1/SSL2 �롼����ˤ��¹���θ�ͭ��/��ͭ�٥��ȥ�׻���
  ! �ԤäƤ���. ��, �桼�������Ѥ��Ƥ���饤�֥��ȥ��֥롼�����ռ�
  ! ���뤳�Ȥʤ��Ȥ����Ȥ��Ǥ���. 
  use dc_message, only : MessageNotify

  implicit none
  private
  public deig1_ssl2

contains
  subroutine deig1_ssl2(amat,eigen_r,eigen_i,eigvec_r,eigvec_i,&
                           info,sort,reverse )
    !
    ! ���Υ��֥롼����ϸ�ͭ�ͷ׻��Ѷ��̥��󥿡��ե�������Ϳ����
    ! eigmatrix �⥸�塼��θ������֥롼���� eigen �Ȥ����Ѥ�����. 
    !
    !   * ���� AMAT �� i ���ܸ�ͭ�ͤ� eigen_r(i), eigen_i(i) �˳�Ǽ
    !   * �б������ͭ�٥��ȥ�� eigvec_r(:,i), eigvec_i(:,i) �˳�Ǽ
    !   * ��Ǽ�����ͭ�ͤο��ϰ��� eigen_r ���礭���Ƿ�ޤ�
    !
    !   * ��ͭ�ͤν��֤� sort �� order ��������. 
    !   * sort �ˤ�äƽ��֤����뤿����Ѥ����̤���ꤹ��. 
    !     ����(R), ������������(RA), ����(I), ������������(IA)
    !   * reverse �ˤ�äƾ�������(.false.), �礭����(.true.)�����Ǥ���.
    !   * �ǥե���Ȥ� sort='R', reverse=.false.
    !
    ! �����Ǥ� DEIG1/SSL2 �롼����ˤ��¹���θ�ͭ��/��ͭ�٥��ȥ�׻���
    ! �ԤäƤ���. ��, �桼�������Ѥ��Ƥ���饤�֥��ȥ��֥롼�����ռ�
    ! ���뤳�Ȥʤ��Ȥ����Ȥ��Ǥ���. 
    !
    interface 
       function indexx(arrin)
         implicit none
         real(8), dimension(:), intent(in)  :: arrin
         integer, dimension(size(arrin))    :: indexx
       end function indexx
    end interface

   !------------ ���� ------------
    real(8), dimension(:,:)                   :: amat      ! ������������
    real(8), intent(out), dimension(:)        :: eigen_r   ! ��ͭ�ͼ¿���
    real(8), intent(out), dimension(:)        :: eigen_i   ! ��ͭ�͵�����
    real(8), intent(out), &
      dimension(size(amat,1),size(eigen_r))   :: eigvec_r  ! ��ͭ�٥��ȥ����
    real(8), intent(out), &
      dimension(size(amat,1),size(eigen_i))   :: eigvec_i  ! ��ͭ�٥��ȥ����
    integer, intent(out)                      :: info      ! ���ơ�����
    character(len=2), intent(in), optional    :: sort      ! �¤��Ѥ�����
    logical, intent(in), optional             :: reverse   ! �¤��Ѥ������å�

   !------------ ����ѿ� ------------
    real(8), dimension(size(amat,1))              :: wr    ! ��ͭ�ͼ¿���
    real(8), dimension(size(amat,1))              :: wi    ! ��ͭ�͵�����
    real(8), dimension(size(amat,1),size(amat,1)) :: vec   ! ��ͭ�٥��ȥ�
    real(8), dimension(size(amat,1))              :: work  ! ����ѿ�
    integer, dimension(size(amat,1))              :: index ! �¤��Ѥ���
    integer, parameter :: mode=0! DEIG1���Ϥ������å�

    integer :: nm, i, j

    !------- ���������å� ------
    if (size(amat,1) /= size(amat,2))then
       call MessageNotify('E','DEIG1_SSL2','Input matrix not square')
    else
       nm = size(amat,1)
    endif

    !------- DEIG1/SSL2 �ˤ��׻� ------
    call deig1(amat,nm,nm,mode,wr,wi,vec,work,info)

    !------- ���֥롼���󥨥顼���� -------
    if ( info /= 0 ) then
       call MessageNotify('W','DEIG1_SSL2',&
            'Error in calculating eigenvalues/vectors...',i=(/info/) )
       return
    endif

    !------- ��ͭ�٥��ȥ����촹�� -------
    if ( present(sort) ) then
       if ( sort == 'RA' ) then          ! ��ͭ�ͼ�����������
          index=indexx(abs(wr))
       elseif ( trim(sort) == 'I' ) then ! ��ͭ�͵���
          index=indexx(wi)
       elseif ( sort == 'IA' ) then      ! ��ͭ�͵�����������
          index=indexx(abs(wi))
       else
          index=indexx(wr)               ! default�ϸ�ͭ�ͼ���
       endif
    else
       index=indexx(wr)                  ! default�ϸ�ͭ�ͼ���
    endif

    if ( present(reverse) )then
       if ( reverse ) then               ! �礭����
          index=index(size(index):1:-1)
       endif
    endif

    do i=1,size(eigen_r)
       j = index(i)
       eigen_r(i) = wr(j)
       eigen_i(i) = wi(j)

       if ( wi(j) == 0 ) then
          eigvec_r(:,i) = vec(:,j)
          eigvec_i(:,i) = 0.0
       else if ( wi(j) == -wi(j+1) ) then
          eigvec_r(:,i) = vec(:,j)
          eigvec_i(:,i) = vec(:,j+1)
       else
          eigvec_r(:,i) = vec(:,j-1)
          eigvec_i(:,i) = -vec(:,j)
       endif
    enddo

  end subroutine deig1_ssl2

end module ssl2_eigen

