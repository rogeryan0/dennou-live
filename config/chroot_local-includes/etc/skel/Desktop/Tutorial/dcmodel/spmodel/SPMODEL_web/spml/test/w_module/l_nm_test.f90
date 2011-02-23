program lnmtest

  use w_base_module, only : w_base_initial, l_nm

  integer, parameter :: nm=10, im=32, jm=16
  integer, parameter :: nfix=nm
  integer, parameter :: mfix=0
  integer            :: narray(nm+1), marray(nm+1)
  integer            :: n, m

  call w_base_initial(nm,im,jm)

  narray=(/(n,n=0,nm)/)
  marray=(/(m,m=0,nm)/)

  write(6,*) 'test 00'
  write(6,*) 'n=1, m=1, l=', l_nm(1,1)

  write(6,*) 
  write(6,*) 'test 01'
  write(6,*) 'n = ', nfix
  write(6,*) 'marray = ', narray
  write(6,*) 'l= ', l_nm(nfix,marray)

  write(6,*) 
  write(6,*) 'test 10'
  write(6,*) 'narray = ', narray
  write(6,*) 'm = ', mfix
  write(6,*) 'l= ', l_nm(narray,mfix)

  write(6,*) 
  write(6,*) 'test 11'
  write(6,*) 'narray = ', narray
  write(6,*) 'marray = ', marray
  write(6,*) 'l= ', l_nm(narray,marray)

end program lnmtest

