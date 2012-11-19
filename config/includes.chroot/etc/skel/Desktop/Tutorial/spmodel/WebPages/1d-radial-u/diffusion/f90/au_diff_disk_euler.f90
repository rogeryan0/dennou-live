!----------------------------------------------------------------------
!   Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!$BI=Bj(B  at_module $B%5%s%W%k%W%m%0%i%`(B
!
!      $B3H;6J}Dx<0(B d temp/dt = kappa 1/r d/dr (r d temp/dr)
!
!$BMzNr(B  2007/12/28  $BC]9-??0l(B
!
program au_diff_disk

  use lumatrix
  use au_module
  use gt4_history
  implicit none

 !---- $B6u4V2rA|EY@_Dj(B ----
  integer, parameter :: im=16                ! $B3J;RE@$N@_Dj(B
  integer, parameter :: km=16               ! $B@ZCGGH?t$N@_Dj(B

 !---- $B:BI8JQ?t$J$I(B ----
  real(8), parameter :: ra=2.0D0            ! $BH>7B(B

 !---- $BJQ?t(B ----
  real(8), dimension(0:im)  :: g_temp
  real(8), dimension(0:km)  :: u_temp

 !---- $B;~4V@QJ,%Q%i%a%?!<(B ----
  real(8), parameter :: dt=1e-5                   ! $B;~4V%9%F%C%W4V3V(B
  integer, parameter :: nt=100000, ndisp=10000    ! $B;~4V@QJ,?t(B, $BI=<(%9%F%C%W(B

 !---- $BJ*M}%Q%i%a%?!<(B ----
  real(8), parameter :: tempbndry=0.0           ! $B6-3&CM(B
  character(len=2), parameter :: tempbc='D'     ! $B6-3&>r7o(B(D/N)

!!$  real(8), parameter :: sigma=0.1               ! $B=i4|CM$NI}(B
  real(8), parameter :: kappa=1.0               ! $B3H;678?t(B
 
  integer :: it=0

 !---------------- $B:BI8CM$N@_Dj(B ---------------------
  call au_initial(im,km,ra,(/0/))

 !------------------- $B=i4|CM@_Dj(B ----------------------
  g_temp = 1.0D0
  u_temp = u_g(g_temp)

  call output_gtool4_init                            ! $B%R%9%H%j!<=i4|2=(B
  call output_gtool4

 !------------------- $B;~4V@QJ,(B ----------------------
  do it=1,nt
     u_temp = u_temp &
          + dt * kappa * u_g(1.0D0/g_R * g_Dr_u(u_g(g_R*g_Dr_u(u_temp))))

     if ( tempbc == 'D' ) then
        call au_Boundary_D(u_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call au_Boundary_N(u_temp,tempbndry)
     else
        write(6,*) 'B.C. not supported'
     endif

     if(mod(it,ndisp) .eq. 0)then                    ! $B=PNO(B
        call output_gtool4
     endif

  enddo
  call output_gtool4_close                           ! $B%R%9%H%j!<8e=hM}(B

  stop

  contains

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! $B%R%9%H%j!<:n@.(B
           file='au_diff_disk.nc', &
           title='Diffusion equation in a sphere', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'r','t'/), dimsizes=(/im+1,0/),    &
           longnames=(/'radial coordinate',&
                       'time             '/),&
           units=(/'1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('r',g_R)                            ! $BJQ?t=PNO(B

      call HistoryAddVariable( &                          ! $BJQ?tDj5A(B
           varname='temp', dims=(/'r','t'/), & 
           longname='voticity', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      g_temp = g_u(u_temp)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',g_temp)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program au_diff_disk
