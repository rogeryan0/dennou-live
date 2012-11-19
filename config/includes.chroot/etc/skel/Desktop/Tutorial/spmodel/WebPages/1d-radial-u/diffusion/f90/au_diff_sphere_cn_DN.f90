!----------------------------------------------------------------------
!   Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!$BI=Bj(B  at_module $B%5%s%W%k%W%m%0%i%`(B
!
!      $B3H;6J}Dx<0(B d temp/dt = kappa 1/r^2 d/dr (r^2 d temp/dr)
!
!      Crank-Nicolson $B%9%-!<%`(B
!
!$BMzNr(B  2007/12/28  $BC]9-??0l(B
!      2007/12/31  $BC]9-??0l(B  $B2r@O2r$H$NHf3S(B
!      2008/01/01  $BC]9-??0l(B  $B%N%$%^%s>r7o2rDI2C(B
!
program au_diff_sphere_cn_DN

  use lumatrix
  use dc_message
  use au_module
  use gt4_history
  implicit none

 !---- $B6u4V2rA|EY@_Dj(B ----
  integer, parameter :: im=32                ! $B3J;RE@$N@_Dj(B
  integer, parameter :: km=32               ! $B@ZCGGH?t$N@_Dj(B

 !---- $B:BI8JQ?t$J$I(B ----
  real(8), parameter :: ra=2.0D0            ! $BH>7B(B

 !---- $BJQ?t(B ----
  real(8), dimension(0:im)  :: g_temp
  real(8), dimension(0:km)  :: u_temp

  real(8), dimension(0:im)  :: g_temp_analytic    ! $B2r@O2r(B

 ! Crank Nicholson $B1"E*7W;;MQ3H;69TNs(B($B29EY(B)
  real(8)             :: DifLUMT_Temp(0:km,0:km)
  integer             :: kpivot_Temp(0:km)        ! $B%T%\%C%H(B

 !---- $B;~4V@QJ,%Q%i%a%?!<(B ----
  real(8), parameter :: dt=1e-3                   ! $B;~4V%9%F%C%W4V3V(B
  integer, parameter :: nt=1000, ndisp=100    ! $B;~4V@QJ,?t(B, $BI=<(%9%F%C%W(B

 !---- $BJ*M}%Q%i%a%?!<(B ----
  real(8)            :: tempbndry=0.0           ! $B6-3&CM(B
  character(len=1)   :: tempbc='D'              ! $B6-3&>r7o(B(D/N)

  real(8), parameter :: kappa=1.0               ! $B3H;678?t(B
  integer :: norder=2                           ! $B8GM-4X?t$N<!?t(B
 
  real(8) :: time, pi, alpha
  integer :: it=0

  pi = 4.0D0 * ATAN(1.0D0)

 !---------------- $B:BI8CM$N@_Dj(B ---------------------
  call au_initial(im,km,ra,(/0/))

 !------------------- $B=i4|CM@_Dj(B ----------------------
  write(6,*)'BC? (D)richlet or (N)euman'
  read(5,*) tempbc

  write(6,*)'BC value?'
  read(5,*) tempbndry

  write(6,*)'norder?'
  read(5,*)norder

  call CNDiffusionMatrix( kappa, dt, DifLUMT_Temp, kpivot_Temp )
 
  if ( tempbc == 'D' ) then
     g_temp = sin(norder*pi/ra*g_R)/(norder*pi/ra*g_R) + tempbndry ! $B8GM-%b!<%I(B
  else if ( tempbc == 'N' ) then
     alpha=CalAlpha(norder)
     g_temp = sin(alpha/ra*g_R)/(alpha/ra*g_R)  ! $B8GM-%b!<%I(B
  else
     call MessageNotify('E','main', 'B.C. not supported')
  endif
  u_temp = u_g(g_temp)

  call output_gtool4_init                            ! $B%R%9%H%j!<=i4|2=(B
  call output_gtool4

 !------------------- $B;~4V@QJ,(B ----------------------
  do it=1,nt
     time = it * dt
     u_temp = u_temp  + dt/2 * kappa &
                      * u_g(1.0D0/g_R** 2* g_Dr_u(u_g(g_R**2*g_Dr_u(u_temp))))

     if ( tempbc == 'D' ) then
        call au_Boundary_D(u_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call au_Boundary_N(u_temp,tempbndry)
     else
        call MessageNotify('E','main', 'B.C. not supported')
     endif

     u_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,u_Temp)

     if(mod(it,ndisp) .eq. 0)then                    ! $B=PNO(B
        call output_gtool4
     endif

  enddo
  call output_gtool4_close                           ! $B%R%9%H%j!<8e=hM}(B

  stop

  contains

 !------------------- $B3H;69`(B ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! $B3H;678?t(B
    real(8), intent(IN)          :: dt       ! $B;~4V9o(B

    ! Crank Nicholson $B3H;61"E*7W;;MQ9TNs(B(1-D dt/2$B"&(B^2, LU $BJ,2r(B)
    real(8), dimension(0:km,0:km), intent(OUT)  :: DiffLUMatrix
    ! $B%T%\%C%H>pJs(B
    integer, dimension(0:km), intent(OUT)       :: kpivot

    ! $B:n6HMQJQ?t(B
    real(8), dimension(0:km)           :: u_I
    real(8), dimension(0:km)           :: u_DI

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,km
       u_I = 0.0 ; u_I(l) = 1.0             ! $B3FGH?t@.J,FHN)(B
       u_DI =  - Diffc * dt/2.0 &
                 * u_g(1.0D0/g_R** 2* g_Dr_u(u_g(g_R**2*g_Dr_u(u_I))))


       if ( tempbc == 'D' ) then
          call au_Boundary_D(u_DI)          ! $BJQ2=ItJ,$O6-3&CM$rJQ$($J$$(B
       else if ( tempbc == 'N' ) then
          call au_Boundary_N(u_DI)          ! $BJQ2=ItJ,$O6-3&CM$rJQ$($J$$(B
       else
          call MessageNotify('E','main', 'B.C. not supported')
       endif

       DiffLUMatrix(:,l) = u_I + u_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !-----------------------------------------
 ! x sin(x)- cos(x) $B$N2r$r5a$a$k(B
 !
  function CalAlpha(n)
    integer, intent(IN) :: n                 ! $B5a$a$k2r$NHV9f(B
    real(8)             :: CalAlpha
    real(8), parameter  :: eps = 1.0D-14     ! $B2r$N@:EY(B

    real(8) :: PI
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    PI = atan(1.0D0)*4.0D0

    xs=PI/2.0D0  + n*PI
    xl=PI/2.0D0  + (n+1)*PI

    ValS = xs*sin(xs) - cos(xs)
    ValL = xl*sin(xl) - cos(xl)
    if ( ValS * ValL .GT. 0.0D0 ) &
         call MessageNotify('E','InvXtanX',&
         'Initial values of ValS and ValL are the same sign.')
!!$    write(6,*) 'vals, vall',vals, vall
10  xm = (xs + xl)/2.0
    ValM = xm*sin(xm) - cos(xm)
    
    if ( ValS * ValM .GT. 0.0D0 ) then
       xs = xm ; ValS= xs*sin(xs) - cos(xs)
    else
       xl = xm ; ValL=xl*sin(xl) - cos(xl)
    endif

    if ( abs(xl-xs) .lt. eps ) then
       CalAlpha = xm
       goto 9
    endif

    goto 10

9   continue
  end function CalAlpha

 !------------------- $B=PNO%k!<%A%s(B ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                               ! $B%R%9%H%j!<:n@.(B
         file='au_diff_sphere_cn_DN.nc', &
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
         longname='temperature', units='1', xtype='double')

    call HistoryAddVariable( &                          ! $BJQ?tDj5A(B
         varname='temp_analytic', dims=(/'r','t'/), & 
         longname='temperature(analytic)', units='1', xtype='double')

    call HistoryAddVariable( &                          ! $BJQ?tDj5A(B
         varname='temp_error', dims=(/'r','t'/), & 
         longname='temperature(error)', units='1', xtype='double')

  end subroutine output_gtool4_init

  subroutine output_gtool4
    g_temp = g_u(u_temp)
    if ( tempbc == 'D' ) then
       g_temp_analytic = sin(norder*pi/ra*g_R)/(norder*pi/ra*g_R) &
                         *exp(-(norder*pi/ra)**2*time) + tempbndry
    else if ( tempbc == 'N' ) then
       g_temp_analytic = sin(alpha/ra*g_R)/(alpha/ra*g_R) &
                         *exp(-(alpha/ra)**2*time) 
    else
          call MessageNotify('E','main', 'B.C. not supported')
    endif

    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',g_temp)
    call HistoryPut('temp_analytic',g_temp_analytic)
    call HistoryPut('temp_error',g_temp_analytic-g_temp)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program au_diff_sphere_cn_DN
