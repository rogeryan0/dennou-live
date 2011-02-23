!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wq_module �ƥ��ȥץ����
!
!      �ȥ�����ݥƥ󥷥��ζ���������
!
!����  2009/12/11  �ݹ�����   wq_test_torvelbc.f90 ��� SJPACK �Ѥ˲�¤
!
program wq_module_torvelbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=8   ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=15         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ra=2.0               ! ��Ⱦ��

  real(8), dimension(0:im-1,1:jm,km)       :: xyr_TorVel0
  real(8), dimension(0:im-1,1:jm,km)       :: xyr_TorVel
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wq_TorVel
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wq_TorVel0
  real(8), dimension((nm+1)*(nm+1),km)     :: wr_TorVel
  character(len=1), dimension(2),parameter :: BCond=(/'F','R'/)

  real(8), dimension(0:im-1,1:jm,km)       :: xyr_True
  real(8), dimension(0:im-1,1:jm,km)       :: xyr_Zero=0.0D0

  ! Ƚ�������
  integer, parameter :: check_digits = 13
  integer, parameter :: ignore = -14

  integer :: ll

  call MessageNotify('M','wq_module_torvelbc_test', &
       'wq_module wq_TorvelBoundary subroutine test')

  call wq_initial(im,jm,km,nm,lm,ra)

  do ll=1,2

     ! P_10
     !xyr_TorVel = sin(xyr_lat) * cos( pi*(xyr_rad-ri)/(ro-ri) )
     ! P_1_1
     !xyr_TorVel = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/(ro-ri) )
     xyr_TorVel = 2*sin(xyr_lat)**2 * (xyr_rad-ra)**2 * xyr_Rad**2

     xyr_TorVel0 = xyr_TorVel
     wq_TorVel = wq_xyr(xyr_TorVel)
     wq_TorVel0 = wq_TorVel  
     wr_TorVel = wr_xyr(xyr_TorVel)


     !---- wq_TorBoundary test
     call wq_TorBoundary(wq_TorVel,cond=BCond(ll),new=.true.)
     xyr_TorVel = xyr_wq(wq_TorVel)

     call AssertEqual(&
          message='wq_TorvelgBoundary (internal value)',                &
          answer = wq_Torvel(:,0:lm-2),                                 &
          check = wq_Torvel0(:,0:lm-2),                                 &
          significant_digits = check_digits, ignore_digits = ignore     &
       )

     ! ��ü���������å�
     if( BCond(ll) == 'F' ) then
        xyr_True = xyr_wq(wq_RadDRad_wq(wq_Torvel)- wq_Torvel)/xyr_Rad
     else
        xyr_True = xyr_TorVel
     endif

     call AssertEqual(&
          message='wq_TorlBoundary (Top boundary)',                     &
          answer = xyr_True(:,:,km),                                    &
          check = xyr_Zero(:,:,km),                                     &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     call MessageNotify('M','wq_module_torvelbc_test', &
          'wq_TorBoundarry: '//BCond(ll)//'-Top B.C. test succeeded!')


     !---- wr_TorBoundaryGrid test
     call wr_TorBoundaryGrid(wr_TorVel,cond=BCond(ll),new=.true.)
     xyr_TorVel = xyr_wr(wr_TorVel)
     wq_TorVel = wq_wr(wr_TorVel)

     ! ���������å�
     xyr_True = xyr_TorVel - xyr_TorVel0

     call AssertEqual(&
          message='wq_TorBoundary (internal value)',                    &
          answer = xyr_Torvel(:,:,1:km-1),                              &
          check = xyr_Torvel0(:,:,1:km-1),                              &
          significant_digits = check_digits, ignore_digits = ignore     &
       )

     ! ��ü���������å�
     if( BCond(ll) == 'F' ) then
        xyr_True = xyr_wq(wq_RadDRad_wq(wq_Torvel)- wq_Torvel)/xyr_Rad
     else
        xyr_True = xyr_TorVel
     endif

     call AssertEqual(&
          message='wq_TorlBoundaryGrid (Top Booundary)',                &
          answer = xyr_True(:,:,km),                                    &
          check = xyr_Zero(:,:,km),                                     &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     call MessageNotify('M','wq_module_torvelbc_test', &
          'wq_TorBoundaryGrid: '//BCond(ll)//'-Top B.C. test succeeded!')
          
  enddo

  call MessageNotify('M','wq_module_torvelbc_test', &
       'wq_module wq_TorvelBoundary subroutine test succeeded!')

end program wq_module_torvelbc_test

