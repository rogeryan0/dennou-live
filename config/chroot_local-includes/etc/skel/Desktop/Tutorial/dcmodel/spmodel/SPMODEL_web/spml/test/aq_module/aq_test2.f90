program aq_basetest2
  use aq_module

  integer, parameter :: im=16, km=31         ! 格子点数, 切断波数
  real(8), parameter :: ra=2               ! 領域の大きさ

  real(8) :: alpha=1.0D0        ! 展開多項式パラメター  0 < α <= 1
  real(8) :: beta= 2.0D0        ! 展開多項式パラメター  0 < β
  integer :: md(2)=(/2,3/)   

  real(8) :: ag_data(2,im)
  real(8) :: ag_data_sol(2,im)
  real(8) :: aq_data(2,0:km)

  integer :: ir

  call aq_Initial(im,km,ra,alpha,beta,md)

  ag_data(1,:) = g_R**6
  ag_data(2,:) = g_R**5

  aq_data     = aq_ag(ag_data)
  aq_data     = aq_rDr_aq(aq_data)
  ag_data_sol = ag_aq(aq_data)

  ag_data(1,:) = 6*g_R**6
  ag_data(2,:) = 5*g_R**5

  aq_data     = aq_ag(ag_data)

  do ir=1,im
     write(6,*) ir,ag_data(1,ir),ag_data_sol(1,ir), ag_data(1,ir)-ag_data_sol(1,ir)
  enddo
  write(6,*)
  do ir=1,im
     write(6,*) ir,ag_data(2,ir),ag_data_sol(2,ir), ag_data(2,ir)-ag_data_sol(2,ir)
  enddo

end program aq_basetest2
