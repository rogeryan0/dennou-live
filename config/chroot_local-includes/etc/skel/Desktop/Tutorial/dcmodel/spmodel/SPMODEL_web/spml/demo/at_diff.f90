!----------------------------------------------------------------------
!   Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!$BI=Bj(B  at_module $B%5%s%W%k%W%m%0%i%`(B
!
!      $B3H;6J}Dx<0(B d zeta/dt = kappa d^2 zeta/dx^2
!
!$BMzNr(B  2002/07/06  $BC]9-??0l(B
!      2005/03/15  $BC]9-??0l(B
!      2005/06/03  $B:4!9LZ(B $BMNJ?(B it=0 $B$rL@<(E*$K@k8@(B
!
program at_diff

  use lumatrix
  use at_module
  use gt4_history
  implicit none

 !---- $B6u4V2rA|EY@_Dj(B ----
  integer, parameter :: im=8                ! $B3J;RE@$N@_Dj(B
  integer, parameter :: km=8                ! $B@ZCGGH?t$N@_Dj(B

 !---- $B:BI8JQ?t$J$I(B ----
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0   ! $BHO0O(B

 !---- $BJQ?t(B ----
  real(8), dimension(0:im)  :: g_zeta
  real(8), dimension(0:km)  :: t_zeta

 !---- $B;~4V@QJ,%Q%i%a%?!<(B ----
  real(8), parameter :: dt=1e-3                 ! $B;~4V%9%F%C%W4V3V(B
  integer, parameter :: nt=1000, ndisp=100      ! $B;~4V@QJ,?t(B, $BI=<(%9%F%C%W(B

 !---- $BJ*M}%Q%i%a%?!<(B ----
  real(8), parameter, dimension(2) :: zetabndry=(/0.0,0.0/)  ! $B6-3&CM(B
  character(len=2), parameter :: tempbc='DN'    ! $B6-3&>r7o(B(DD/DN/ND/NN)

  real(8), parameter :: sigma=0.1               ! $B=i4|CM$NI}(B
  real(8), parameter :: kappa=1.0               ! $B3H;678?t(B
 
  integer :: it=0

 !---------------- $B:BI8CM$N@_Dj(B ---------------------
  call at_initial(im,km,xmin,xmax)

 !------------------- $B=i4|CM@_Dj(B ----------------------
  g_zeta = exp( - ( (g_x-g_x(im/2))/sigma)**2 )
  t_zeta = t_g(g_zeta)
  call boundaries

  call output_gtool4_init                            ! $B%R%9%H%j!<=i4|2=(B
  call output_gtool4

 !------------------- $B;~4V@QJ,(B ----------------------
  do it=1,nt
     t_zeta = t_zeta + dt * ( kappa * t_dx_t(t_dx_t(t_zeta) ) )
     call boundaries

     if(mod(it,ndisp) .eq. 0)then                    ! $B=PNO(B
        g_zeta = g_t(t_zeta)
        call output_gtool4
     endif

  enddo
  call output_gtool4_close                           ! $B%R%9%H%j!<8e=hM}(B

  stop

  contains

    subroutine boundaries
      if ( tempbc == 'DD' ) then
         call at_boundaries_DD(t_zeta,zetabndry)
      elseif ( tempbc == 'DN' ) then
         call at_boundaries_DN(t_zeta,zetabndry)
      elseif ( tempbc == 'ND' ) then
         call at_boundaries_ND(t_zeta,zetabndry)
      elseif ( tempbc == 'NN' ) then
         call at_boundaries_NN(t_zeta,zetabndry)
      else
         write(6,*) 'B.C. not supported'
      endif
    end subroutine boundaries

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! $B%R%9%H%j!<:n@.(B
           file='at_diff.nc', title='Diffusion equation', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','t'/), dimsizes=(/im+1,0/),    &
           longnames=(/'X-coordinate','time        '/),&
           units=(/'1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',g_x)                            ! $BJQ?t=PNO(B

      call HistoryAddVariable( &                          ! $BJQ?tDj5A(B
           varname='zeta', dims=(/'x','t'/), & 
           longname='voticity', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      g_zeta = g_t(t_zeta)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('zeta',g_zeta)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program at_diff
