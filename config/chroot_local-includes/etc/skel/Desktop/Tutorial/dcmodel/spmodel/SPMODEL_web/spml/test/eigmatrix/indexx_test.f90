program indexx_test

  implicit none
    interface 
       function indexx(arrin)
         implicit none
         real(8), dimension(:), intent(in)  :: arrin
         integer, dimension(size(arrin))    :: indexx
       end function indexx
    end interface

  integer, parameter    :: n=10
  real(8), dimension(n) :: array
  integer, dimension(n) :: index

  array  = (/7,8,1,3,9,0,2,5,4,6/)

  index = indexx(array)

  write(6,*) array
  write(6,*) array(index)
  write(6,*) array(index(size(index):1:-1))

end program indexx_test

