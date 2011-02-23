!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  INDEXX :" ���������ֹ��դ�
!
!����  2009/02/06   �ݹ����� dsort/Netlib ��
!      
!
!����    �礭�����¤��Ѥ���ˤϼ��Τ褦�ˤ���Ф褤
!
!        index = indexx(array)
!        array = array(index)                      ! ��������
!        array = array(index(size(index):1:-1)     ! �礭����
!
!++
!
!= indexx
!
! Authors:: Shin-ichi Takehiro, Youhei SASAKI
! Version:: $Id: indexx.f90,v 1.4 2009-02-27 05:51:09 uwabami Exp $
!
!== ����
!
! ���Υե�����Ǥ�Ϳ����줿������ͤξ�������� sort ���뤿��δؿ���
! ���� indexx ���󶡤���.
!
!== �ؿ�
!
! indexx :: Ϳ����줿������ͤξ�������� sort ����ؿ�.
!           �ͤ��礭����� sort �������, �㤨�аʲ����ͤˤ�����ɤ�����
!
!        index = indexx(array)
!        array = array(index)                      ! ��������(default)
!        array = array(index(size(index):1:-1)     ! �礭����
!
!
function indexx(arrin)
  implicit none
  real(8), dimension(:), intent(in)  :: arrin
  integer, dimension(size(arrin))    :: indexx
  
  integer :: ir, l, i, j, indxt
  real(8) :: Q
  
  integer :: n
  
  n = size(arrin)
  
  indexx = (/(j, j=1,n)/)
  
  l  = n/2 + 1
  ir = n
  
10 continue
  
  if( l .gt. 1 )then
    l     = l - 1
    indxt = indexx(l)
    q     = arrin(indxt)
  else
    indxt = indexx(ir)
    q     = arrin(indxt)
    indexx(ir) = indexx(1)
    ir = ir - 1
    if( ir .eq. 1 )then
      indexx(1) = indxt
      return
    endif
  endif
  i = l
  j = l + l
  
20 if( j .le. ir )then
    if( j .lt. ir )then
      if( arrin( indexx(j) ) .lt. arrin( indexx(j+1) )  ) j = j + 1
    endif
    if(  q .lt. arrin( indexx(j) )  )then
      indexx( i ) = indexx( j )
      i = J
      j = j + j
    else
      j = ir + 1
    endif
    go to 20
  endif
  indexx( i ) = indxt
  go to 10
  
end function indexx
