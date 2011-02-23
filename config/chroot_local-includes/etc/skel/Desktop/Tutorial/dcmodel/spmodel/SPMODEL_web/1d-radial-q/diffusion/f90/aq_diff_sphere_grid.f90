!----------------------------------------------------------------------
!   Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!$BI=Bj(B  aq_module $B%5%s%W%k%W%m%0%i%`(B
!
!      $B3H;6J}Dx<0(B 
!   d temp/dt = kappa [1/r^2 d/dr (r^2 d temp/dr) - n(n+1)temp/r^2]
!             = kappa [1/r^2 rd/dr (r d temp/dr)+ 1/r^2(r d temp/dr)
!                      - n(n+1)temp/r^2]
!
!$BMzNr(B  2008/04/02  $BC]9-??0l(B
!
program aq_diff_sphere_grid

  use lumatrix
  use aq_module
  use gt4_history
  implicit none

 !---- $B6u4V2rA|EY@_Dj(B ----
  integer, parameter :: im=32, km=63       ! $B3J;RE@?t(B, $B@ZCGGH?t(B

 !---- $B:BI8JQ?t$J$I(B ----
  real(8), parameter :: ra=2.0D0            ! $BH>7B(B
  real(8), parameter :: alpha=1.0D0         ! $BE83+B?9`<0%Q%i%a%?!<(B  0 < $B&A(B <= 1
  real(8), parameter :: beta= 2.0D0         ! $BE83+B?9`<0%Q%i%a%?!<(B  0 < $B&B(B
  integer, parameter :: mind=3             ! $BE83+%9%Z%/%H%k>eIU<!?t(B

 !---- $BJQ?t(B ----
  real(8), dimension(im)    :: g_temp

 !---- $B;~4V@QJ,%Q%i%a%?!<(B ----
  real(8), parameter :: dt=1e-5                 ! $B;~4V%9%F%C%W4V3V(B
  integer, parameter :: nt=100000, ndisp=10000    ! $B;~4V@QJ,?t(B, $BI=<(%9%F%C%W(B
 
 !---- $BJ*M}%Q%i%a%?!<(B ----
  real(8), parameter :: tempbndry=0.0           ! $B6-3&CM(B
  character(len=2), parameter :: tempbc='N'     ! $B6-3&>r7o(B(D/N)

!!$  real(8), parameter :: sigma=0.1               ! $B=i4|CM$NI}(B
  real(8), parameter :: kappa=1.0               ! $B3H;678?t(B
 
  integer :: it=0

 !---------------- $B:BI8CM$N@_Dj(B ---------------------
  call aq_Initial(im,km,ra,alpha,beta,(/mind/))

 !------------------- $B=i4|CM@_Dj(B ----------------------
  g_temp = g_R**mind * (ra-g_R)

  call output_gtool4_init                            ! $B%R%9%H%j!<=i4|2=(B
  call output_gtool4

 !------------------- $B;~4V@QJ,(B ----------------------
  do it=1,nt
     g_temp = g_temp &
          + dt * kappa * &
                1.0D0/g_R**2*(g_q(q_rDr_q(q_rDr_q(q_g(g_temp))) &
                                +q_rDr_q(q_g(g_temp)))-mind*(mind+1)*g_temp )

     if ( tempbc == 'D' ) then
        call aq_BoundaryGrid_D(g_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call aq_BoundaryGrid_N(g_temp,tempbndry)
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
           file='aq_diff_sphere_grid.nc', &
           title='Diffusion equation in a sphere', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'r','t'/), dimsizes=(/im,0/),    &
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

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',g_temp)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program aq_diff_sphere_grid
