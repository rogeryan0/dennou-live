!--
!----------------------------------------------------------------------
!     Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ��ͭ�����ꥵ�֥롼���� (LAPACK)
!
!����  2002/07/06  �ݹ�����
!      2005/01/25  �ݹ�����  MessageNotify/gt4f90io ���Ѥ��� dcl �����ڤ�Υ��.
!      2006/03/08  �ݹ����� �����Ȥ� RDoc �Ѥ��ѹ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ��ѹ�
!
!      Debian/GNU Linux + Fujitsu frt �ʤ��
!      lapack, lapack-deb �ѥå������򥤥󥹥ȡ��뤷��, 
!         -llapack -lblas -L/usr/lib/gcc-lib/i386-linux/2.95.4 -lg2c
!      �Ȥ��ä����ץ�����Ĥ���٤�. 
!++
module lapack_eigen
  !
  != lapack_eigen
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: lapack_eigen.f90,v 1.4 2009-02-28 21:33:47 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  ! 
  !== ����
  !
  ! spml/lapack_eigen �� eigmatrix �⥸�塼��θ������֥롼���� eigen
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
  ! �����Ǥ� DGEEV/LAPACK �롼����ˤ��¹���θ�ͭ��/��ͭ�٥��ȥ�׻���
  ! �ԤäƤ���. ��, �桼�������Ѥ��Ƥ���饤�֥��ȥ��֥롼�����ռ�
  ! ���뤳�Ȥʤ��Ȥ����Ȥ��Ǥ���. 
  !
  use dc_message, only : MessageNotify

  implicit none
  private
  public deigen_lapack

contains
  subroutine deigen_lapack(amat,eigen_r,eigen_i,eigvec_r,eigvec_i,&
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
    ! �����Ǥ� DGEEV/LAPACK �롼����ˤ��¹���θ�ͭ��/��ͭ�٥��ȥ�׻���
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
    real(8), dimension(size(amat,1),size(amat,1)) :: vl    ! ����ͭ�٥��ȥ�
    real(8), dimension(size(amat,1),size(amat,1)) :: vr    ! ����ͭ�٥��ȥ�
    real(8), dimension(size(amat,1)*4)            :: work  ! ����ѿ�
    integer, dimension(size(amat,1))              :: index ! �¤��Ѥ���
    character(len=1), parameter :: jobvl='N', jobvr='V'    ! DEGGV ���Ϥ������å�

    integer :: nm, i, j

    !------- ���������å� ------
    if (size(amat,1) /= size(amat,2))then
       call MessageNotify('E','DEIGEN_LAPACK','Input matrix not square')
    else
       nm = size(amat,1)
    endif

    !------- DGEEV/LAPACK �ˤ��׻� ------
    call DGEEV( jobvl, jobvr, nm, amat, nm, wr, wi, &
                vl, nm, vr, nm, work, nm*4, info )

    !------- ���֥롼���󥨥顼���� -------
    if ( info /= 0 ) then
       call MessageNotify('W','DEIGEN_LAPACK',&
            'Error in calculating eigenvalues/vectors...',i=(/info/) )
       return
    endif

    !------- ��ͭ�٥��ȥ����촹�� -------
    if ( present(sort) ) then
       if ( sort == 'RA' ) then          ! ��ͭ�ͼ�����������
          index=indexx(abs(wr))
       elseif ( trim(sort) == 'I ' ) then ! ��ͭ�͵���
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
          eigvec_r(:,i) = vr(:,j)
          eigvec_i(:,i) = 0.0
       elseif ( wi(j) > 0 ) then
          eigvec_r(:,i) = vr(:,j)
          eigvec_i(:,i) = vr(:,j+1)
       else
          eigvec_r(:,i) = vr(:,j-1)
          eigvec_i(:,i) = -vr(:,j)
       endif
    enddo

  end subroutine deigen_lapack

end module lapack_eigen
