!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  INDEXX :" 小さい順番号付け テストプログラム
!
!履歴  2009/02/06   竹広真一 
!
program indexx_test

  interface 
     function indexx(arrin)
       implicit none
       real(8), dimension(:), intent(in)  :: arrin
       integer, dimension(size(arrin))    :: indexx
     end function indexx
  end interface

  integer, parameter         :: narray = 5
  real(8), dimension(narray) :: array
  integer, dimension(narray) :: indary
  integer, dimension(narray) :: indary_sol

!!$  array = (/1.0D0, 2.0D0, -3.0D0, -1.0D0, 0.0D0/)
!!$  indary_sol = (/3,4,5,1,2/)

  array = (/-1.0D0, 1.0D0, -3.0D0, 2.0D0, 0.0D0/)
  indary_sol = (/3,1,5,2,4/)

  indary = indexx(array)

  write(6,*) 'Index check (calc, exact)'
  do i=1,narray
     write(6,*) indary(i), indary_sol(i)
  enddo

  write(6,*)
  write(6,*) 'icreasing order'
  write(6,*) array(indary)
  write(6,*)
  write(6,*) 'dereasing order'
  write(6,*) array(indary(narray:1:-1))

end program indexx_test
