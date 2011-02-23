!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wq_module_sjpack �ƥ��ȥץ����
!
!      �ݥ�����ݥƥ󥷥��ζ���������
!
!����  2009/12/11  �ݹ�����   wq_test_polvelbc.f90 ��� SJPACK �Ѥ˲�¤
!
program wq_module_sjpack_polvelbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module_sjpack

  implicit none

  integer,parameter  :: im=32, jm=16, km=8   ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=15         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ra=0.5D0             ! ��Ⱦ��

  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Poloidal
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_LaplaPol
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_True
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Zero=0.0D0
  real(8), dimension((nm+1)**2,0:lm)     :: wq_Poloidal
  real(8), dimension((nm+1)**2,0:lm)     :: wq_LaplaPol
  real(8), dimension((nm+1)**2,0:lm)     :: wq_LaplaPol0
  character(len=1), dimension(2), parameter :: BCond=(/'F','R'/)

  ! Ƚ�������
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  integer :: l

  call MessageNotify('M','wq_module_sjpack_polvelbc_test', &
       'wq_module_sjpack wq_LaplaPol2Pol_wq subroutine test')

  call wq_initial(im,jm,km,nm,lm,ra)

  do l=1,2

     ! P_10
     xyr_Poloidal = sin(xyr_Lat) * xyr_Rad * ((xyr_Rad-ra)*(xyr_Rad+ra))**3
!!$     xyr_Poloidal = sin(xyr_Lat) * xyr_Rad * ((xyr_Rad-ra)*(xyr_Rad+ra))**2
     wq_LaplaPol0 = wq_Lapla_wq(wq_xyr(xyr_Poloidal))
     !xyr_LaplaPol = sin(xyr_Lat) * sin( pi*(xyr_Rad-ri)/(ro-ri) )
     ! P_1_1
     !xyr_LaplaPol = cos(xyr_Lat)*cos(xyr_Lon)* xyr_Rad * (xyr_Rad-ra) 
     !xyr_LaplaPol = 2*sin(xyr_Lat)**2 * xyr_Rad * (xyr_Rad-ra) 

     !xyr_Poloidal = xyr_wz(wr_LaplaPol2pol_wr(wr_xyr(xyr_LaplaPol),BCond(l)))
     !xyr_Poloidal = xyr_wq(wq_LaplaPol2PolTau_wq(wq_xyr(xyr_LaplaPol),BCond(l)))

     wq_Poloidal  = wq_LaplaPol2Pol_wq(wq_LaplaPol0,BCond(l),new=.true.)
     wq_LaplaPol  = wq_Lapla_wq(wq_Poloidal)
     xyr_Poloidal = xyr_wq(wq_Poloidal)

     !---------------- ���������å� -----------------------
     call AssertEqual(&
          message='wq_Laplapol2Pol_wq (internal value)',                &
          answer = wq_LaplaPol(:,0:lm-4),                               &
          check = wq_LaplaPol0(:,0:lm-4),                               &
          significant_digits = check_digits, ignore_digits = ignore     &
       )

     !--------- ��ü���������å� ----------

     !----- ��=0 at the top ---------
     call AssertEqual(&
          message='wq_LaplaPol2Pol_wq (��=0 Top Booundary)',            &
          answer = xyr_Poloidal(:,:,km),                                &
          check = xyr_Zero(:,:,km),                                     &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     !----- d��/dr=0, d^2��/dr^2 at the top ---------
     if( BCond(l)(1:1) == 'F' ) then
        xyr_True = xyr_wq(wq_RadDRad_wq(wq_RadDRad_wq(wq_Poloidal))&
                                       -wq_RadDRad_wq(wq_Poloidal))/xyr_Rad**2
     else
        xyr_True = xyr_wq(wq_RadDRad_wq(wq_Poloidal))/xyr_Rad
     endif

     call AssertEqual(&
          message='wq_LaplaPol2Pol_wq (Top Booundary)',                 &
          answer = xyr_True(:,:,km),                                    &
          check = xyr_Zero(:,:,km),                                     &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     call MessageNotify('M','wq_module_sjpack_polvelbc_test', &
          'wq_LaplaPol2Pol_wq: '//BCond(l)//'-Top B.C. test succeeded!')
          
  end do

  call MessageNotify('M','wq_module_sjpack_polvelbc_test', &
       'wq_module_sjpack wq_LaplaPol2Pol_wq subroutine test succeeded!')

end program wq_module_sjpack_polvelbc_test
