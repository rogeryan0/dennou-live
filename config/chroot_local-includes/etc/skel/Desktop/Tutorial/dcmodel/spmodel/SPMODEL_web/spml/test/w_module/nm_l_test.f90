program nmltest

  use w_base_module, only : w_base_initial, nm_l

  integer, parameter :: nm=10, im=32, jm=16
  integer, parameter :: lm=5
  integer, parameter :: lfix=5

  integer            :: larray(lm)
  integer            :: nmarray(lm,2)

  integer            :: l

  call w_base_initial(nm,im,jm)

  larray=(/(l,l=1,lm)/)

  write(6,*) 'test normal'
  write(6,*) 'l=',lfix, 'n, m = ', nm_l(lfix)

  write(6,*) 
  write(6,*) 'test array'
  nmarray = nm_l(larray)

  do l=1,lm
     write(6,*) 'l= ', larray(l), '    n,m = ', nmarray(l,:)
  enddo

end program nmltest

