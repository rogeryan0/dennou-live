!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  lumatrix : ����� LU ʬ��ˤ������ϢΩ�������β�
!
!      spml/lumatrix �⥸�塼���, LU ʬ��ˡ�ˤ��ϢΩ 1 ����������򤯤����
!      Fortran90 �ؿ����󶡤���. 
!
!      ¾�Υ��ڥ��ȥ�׻��ѥ⥸�塼�������о줹�붭���������򤯤����
!      �Ѥ����Ƥ���. 
!
!      �٥��ȥ�׻�����ռ�����, Ʊ��������ʣ���Ĥ�ϢΩ1 �������� 
!
!          A[ij]^(n) X [j]^(n) = B[i]^(n) 
!
!      �β��Ʊ����ʣ���Ĥα��ե٥��ȥ� B[i]^(n)b ���Ф��Ƶ��뤳�Ȥ�
!      �Ǥ���褦�ˤʤäƤ���.
!
!
!����  2002/01/20  �ݹ�����
!      2002/06/10  �ݹ�����  �٥��ȥ�Ĺ�������Τ��� lusol2 �����
!      2005/01/10  �ݹ�����  msgdmp -> MessageNotify ���ѹ�
!      2006/03/04  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2009/01/29  ��������ʿ  �����Ȥ� RDoc �Ѥ˽���
!      2009/08/06  �ݹ�����    ludecomp21 �ѥ롼���� OMP ���ѹ�
!
! * �ɥ�����Ȥϸ�Ⱦ���� module ������˵���
!++
subroutine ludecomp21(alu,kp)
  !
  ! ALU(NDIM,NDIM), KP(NDIM)
  ! NDIM x NDIM �ι���� LU ʬ��.
  ! �̣չ���� ���Ϲ���˾�񤭤����.
  !
  use dc_message
  
  real(8), intent(inout) :: alu(:,:)                  ! ���ϡ��̣չ���
  integer, intent(out)   :: kp(size(alu,1))           ! �ԥܥå�
  
  if ( size(alu,1) > size(alu,2) ) then
    call MessageNotify('E','ludecomp',&
      'The third dimension is less than the second')
  elseif( size(alu,1) < size(alu,2) ) then
    call MessageNotify('W','ludecomp',&
      'The third dimension is grater than the second')
  endif
  
  !" ����Σ̣�ʬ�����ʬ�ԥܥå������
  call LUMAK1( alu, kp, size(alu,1) )
  
end subroutine ludecomp21

subroutine ludecomp32(alu,kp)
  !
  ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM)
  ! NDIM x NDIM �ι��� JDIM �Ĥ���٤� LU ʬ��.
  ! �̣չ���� ���Ϲ���˾�񤭤����.
  !
  use dc_message
  
  real(8), intent(inout) :: alu(:,:,:)                  ! ���ϡ��̣չ���
  integer, intent(out)   :: kp(size(alu,1),size(alu,2)) ! �ԥܥå�
  
  if ( size(alu,2) > size(alu,3) ) then
    call MessageNotify('E','ludecomp',&
      'The third dimension is less than the second')
  elseif( size(alu,2) < size(alu,3) ) then
    call MessageNotify('W','ludecomp',&
      'The third dimension is grater than the second')
  endif
  
  !" ����Σ̣�ʬ�����ʬ�ԥܥå������
  call LUMAKE( alu, kp, size(alu,1), size(alu,2) )
  
end subroutine ludecomp32

function lusolve211(alu,kp,b)
  !
  ! ALU(NDIM,NDIM), KP(NDIM), B(NDIM)
  ! NDIM x NDIM �������ϢΩ������
  ! A X = B �� 1 �Ĥ� B ���Ф��Ʒ׻�����. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:)              ! ���ϡ��̣չ���
  integer, intent(in)  :: kp(:)                 ! �ԥܥå�
  real(8), intent(in)  :: b(:)                  ! ���ե٥��ȥ�
  
  real(8) :: lusolve211(size(b))                   ! ��
  
  lusolve211 = b
  call LUSOL2( lusolve211, alu , kp, &             !" �̣�ʬ��ˤ���η׻�
    1, size(b) )
  
end function lusolve211

function lusolve212(alu,kp,b)
  !
  ! ALU(NDIM,NDIM), KP(NDIM), B(JDIM,NDIM)
  ! NDIM x NDIM �������ϢΩ������
  ! A X = B �� JDIM �Ĥ� B ���Ф��Ʒ׻�����. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:)              ! ���ϡ��̣չ���
  integer, intent(in)  :: kp(:)                 ! �ԥܥå�
  real(8), intent(in)  :: b(:,:)                ! ���ե٥��ȥ�
  
  real(8) :: lusolve212(size(b,1),size(b,2))       ! ��
  
  lusolve212 = b
  call LUSOL2( lusolve212, alu , kp, &             !" �̣�ʬ��ˤ���η׻�
    size(b,1), size(b,2) )
  
end function lusolve212

function lusolve322(alu,kp,b)
  !
  ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(JDIM,NDIM)
  ! NDIM x NDIM ������� JDIM ���¤٤�ϢΩ������
  ! A X = B ��ҤȤĤ� B ���¤Ӥ��Ф��Ʒ׻�����. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:,:)                   ! ���ϡ��̣չ���
  integer, intent(in)  :: kp(:,:)                      ! �ԥܥå�
  real(8), intent(in)  :: b(:,:)                       ! ���ե٥��ȥ�
  
  real(8) :: lusolve322(size(b,1),size(b,2))             ! ��
  
  lusolve322 = b
  call LUSOLV( lusolve322, alu , kp, &           !" �̣�ʬ��ˤ���η׻�
    1, size(b,1), size(b,2) )
  
end function lusolve322

function lusolve323(alu,kp,b)
  !
  ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(IDIM,JDIM,NDIM)
  ! NDIM x NDIM ������� JDIM ���¤٤�ϢΩ������
  ! A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:,:)                   ! ���ϡ��̣չ���
  integer, intent(in)  :: kp(:,:)                      ! �ԥܥå�
  real(8), intent(in)  :: b(:,:,:)                     ! ���ե٥��ȥ�
  
  real(8) :: lusolve323(size(b,1),size(b,2),size(b,3)) ! ��
  
  lusolve323 = b
  call LUSOLV( lusolve323, alu , kp, &           !" �̣�ʬ��ˤ���η׻�
    size(b,1), size(b,2), size(b,3) )
  
end function lusolve323

module lumatrix
  !
  != lumatrix
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: lumatrix.f90,v 1.9 2009-08-06 12:41:58 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/lumatrix �⥸�塼���, LU ʬ��ˡ�ˤ��ϢΩ 1 ����������򤯤����
  ! Fortran90 �ؿ����󶡤���. 
  !
  ! ¾�Υ��ڥ��ȥ�׻��ѥ⥸�塼�������о줹�붭���������򤯤����
  ! �Ѥ����Ƥ���. 
  !
  ! �٥��ȥ�׻�����ռ�����, Ʊ��������ʣ���Ĥ�ϢΩ1 �������� 
  !
  !     A[ij]^(n) X [j]^(n) = B[i]^(n) 
  !
  ! �β��Ʊ����ʣ���Ĥα��ե٥��ȥ� B[i]^(n)b ���Ф��Ƶ��뤳�Ȥ�
  ! �Ǥ���褦�ˤʤäƤ���.
  !
  !== �ѿ�����³����������
  !
  ! LUDecomp    :: ����� LU ʬ���Ԥ�
  ! LUSolve     :: ϢΩ 1 ���������β�����
  !
  private 
  public LUDecomp, LUSolve

  interface LUDecomp
     !
     !=== Ϳ����줿����� LU ʬ���Ԥ�, �ԥܥåȤ��Ǽ����.
     !
     ! * LU ʬ�򤵤줿��̤� Alu �˾�񤭤����. 
     !   ���ΥԥܥåȾ���� kp �˳�Ǽ�����.
     !
     ! * LUSolve ���Ѥ������ˤ��Υ��֥롼�����Ƥ�� Alu �� kp ��
     !   �׻����Ƥ���.
     !
     ! * ���Ϥ������ȥԥܥå�������μ����ˤ�ä������ǥ��֥롼�����
     !   �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !   �����롼����Ǥ��� ludecomp21, ludecomp32 ��Ƥ�ɬ�פϤʤ�.
     !
     !=== �����ȷ�̤η�
     !
     ! * Alu �� 2 ��������(Ϳ���뷸������ 1 ��)�ξ��
     !
     !     ! ALU(NDIM,NDIM), KP(NDIM)
     !     ! NDIM x NDIM �ι���� LU ʬ��.
     !     ! �̣չ���� ���Ϲ���˾�񤭤����.
     !
     !     real(8), intent(inout) :: alu(:,: )         ! ���ϡ��̣չ���
     !     integer, intent(out)   :: kp(size(alu,1))   ! �ԥܥå�
     !
     !
     ! * Alu �� 3 ��������(Ϳ���뷸������ʣ��)�ξ��
     !
     !     ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM)
     !     ! NDIM x NDIM �ι��� JDIM �Ĥ���٤� LU ʬ��.
     !     ! �̣չ���� ���Ϲ���˾�񤭤����.
     !
     !     real(8), intent(inout) :: alu(:,:,:)      ! ���ϡ��̣չ�
     !     integer, intent(out)   :: kp(size(alu,1),size(alu,2)) ! �ԥܥå�
     !
     !
     subroutine ludecomp21(alu,kp)
       !
       ! ALU(NDIM,NDIM), KP(NDIM)
       ! NDIM x NDIM �ι���� LU ʬ��.
       ! �̣չ���� ���Ϲ���˾�񤭤����.
       !
       real(8), intent(inout) :: alu(:,: )                 ! ���ϡ��̣չ���
       integer, intent(out)   :: kp(size(alu,1))           ! �ԥܥå�
     end subroutine ludecomp21

     subroutine ludecomp32(alu,kp)
       !
       ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM)
       ! NDIM x NDIM �ι��� JDIM �Ĥ���٤� LU ʬ��.
       ! �̣չ���� ���Ϲ���˾�񤭤����.
       !
       real(8), intent(inout) :: alu(:,:,:)                  ! ���ϡ��̣չ���
       integer, intent(out)   :: kp(size(alu,1),size(alu,2)) ! �ԥܥå�
     end subroutine ludecomp32
  end interface

  interface LUSolve
     !
     ! ϢΩ 1 ���������β�����
     !
     !  * LUSolve ���Ѥ������� LUDecomp��Ƥ�� Alu �� LU ʬ��, 
     !    �ԥܥåȾ��� kp ��׻����Ƥ����ͤФʤ�ʤ�.
     !
     !  * ���Ϥ������ȥԥܥå�������μ����ˤ�ä������ǥ��֥롼�����
     !    �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !    �����롼����Ǥ��� lusolve??? ��Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     !  *  Alu �� 2 ��������(Ϳ���뷸������ 1 ��), 
     !     b �� 1 ��������(Ϳ���뱦�ե٥��ȥ뤬 1 ��)�ξ��
     !
     !     ! ALU(NDIM,NDIM), KP(NDIM), B(NDIM)
     !     ! NDIM x NDIM �������ϢΩ������
     !     ! A X = B �� 1 �Ĥ� B ���Ф��Ʒ׻�����.
     !
     !     real(8), intent(in)  :: alu(:,:)              ! ���ϡ��̣չ���
     !     integer, intent(in)  :: kp(:)                 ! �ԥܥå�
     !     real(8), intent(in)  :: b(:)                  ! ���ե٥��ȥ�
     !
     !     real(8) :: lusolve(size(b))                   ! ��
     !
     !  * Alu �� 2 ��������(Ϳ���뷸������ 1 ��), 
     !    b �� 2 ��������(Ϳ���뱦�ե٥��ȥ뤬ʣ��)�ξ��
     !
     !     ! ALU(NDIM,NDIM), KP(NDIM), B(JDIM,NDIM)
     !     ! NDIM x NDIM �������ϢΩ������
     !     ! A X = B �� JDIM �Ĥ� B ���Ф��Ʒ׻�����.
     !
     !     real(8), intent(in)  :: alu(:,:)              ! ���ϡ��̣չ���
     !     integer, intent(in)  :: kp(:)                 ! �ԥܥå�
     !     real(8), intent(in)  :: b(:,:)                ! ���ե٥��ȥ�
     !
     !     real(8) :: lusolve(size(b,1),size(b,2))       ! ��
     !
     !
     !  * Alu �� 3 ��������(Ϳ���뷸������ʣ��), 
     !    b �� 2 ��������(Ϳ���뱦�ե٥��ȥ뤬 1 ��)�ξ��
     !
     !     ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(JDIM,NDIM)
     !     ! NDIM x NDIM ������� JDIM ���¤٤�ϢΩ������
     !     ! A X = B ��ҤȤĤ� B ���¤Ӥ��Ф��Ʒ׻�����.
     !
     !     real(8), intent(in)  :: alu(:,:,:)            ! ���ϡ��̣չ���
     !     integer, intent(in)  :: kp(:,:)               ! �ԥܥå�
     !     real(8), intent(in)  :: b(:,:)                ! ���ե٥��ȥ�
     !
     !     real(8) :: lusolve(size(b,1),size(b,2))             ! ��
     !
     !
     !  * Alu �� 3 ��������(Ϳ���뷸������ʣ��), 
     !    b �� 3 ��������(Ϳ���뱦�ե٥��ȥ뤬ʣ��)�ξ��
     !
     !     ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(IDIM,JDIM,NDIM)
     !     ! NDIM x NDIM ������� JDIM ���¤٤�ϢΩ������
     !     ! A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����.
     !
     !     real(8), intent(in)  :: alu(:,:,:)                ! ���ϡ��̣չ���
     !     integer, intent(in)  :: kp(:,:)                   ! �ԥܥå�
     !     real(8), intent(in)  :: b(:,:,:)                  ! ���ե٥��ȥ�
     !
     !     real(8) :: lusolve(size(b,1),size(b,2),size(b,3)) ! ��
     !
     !
     function lusolve211(alu,kp,b)
       !
       ! ALU(NDIM,NDIM), KP(NDIM), B(NDIM)
       ! NDIM x NDIM �������ϢΩ������
       ! A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����. 
       ! ��ϱ��դ����ϥ٥��ȥ�˾�񤭤����
       !
       real(8), intent(in)  :: alu(:,:)              ! ���ϡ��̣չ���
       integer, intent(in)  :: kp(:)                 ! �ԥܥå�
       real(8), intent(in)  :: b(:)                  ! ���ե٥��ȥ�
       real(8) :: lusolve211(size(b))                 ! ��
     end function lusolve211

     function lusolve212(alu,kp,b)
       !
       ! ALU(NDIM,NDIM), KP(NDIM), B(JDIM,NDIM)
       ! NDIM x NDIM �������ϢΩ������
       ! A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����. 
       !
       real(8), intent(in)  :: alu(:,:)              ! ���ϡ��̣չ���
       integer, intent(in)  :: kp(:)                 ! �ԥܥå�
       real(8), intent(in)  :: b(:,:)                ! ���ե٥��ȥ�

       real(8) :: lusolve212(size(b,1),size(b,2))       ! ��

     end function lusolve212

     function lusolve322(alu,kp,b)
       !
       ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(JDIM,NDIM)
       ! NDIM x NDIM ������� JDIM ���¤٤�ϢΩ������
       ! A X = B �� B ���Ф��Ʒ׻�����. 
       !
       real(8), intent(in)  :: alu(:,:,:)                   ! ���ϡ��̣չ���
       integer, intent(in)  :: kp(:,:)                      ! �ԥܥå�
       real(8), intent(in)  :: b(:,:)                       ! ���ե٥��ȥ�

       real(8) :: lusolve322(size(b,1),size(b,2))             ! ��
     end function lusolve322

     function lusolve323(alu,kp,b)
       !
       ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(IDIM,JDIM,NDIM)
       ! NDIM x NDIM ������� JDIM ���¤٤�ϢΩ������
       ! A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����. 
       !
       real(8), intent(in)  :: alu(:,:,:)                   ! ���ϡ��̣չ���
       integer, intent(in)  :: kp(:,:)                      ! �ԥܥå�
       real(8), intent(in)  :: b(:,:,:)                     ! ���ե٥��ȥ�

       real(8) :: lusolve323(size(b,1),size(b,2),size(b,3)) ! ��

     end function lusolve323
  end interface

end module lumatrix


