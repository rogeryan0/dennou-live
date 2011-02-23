!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wtu_module テストプログラム
!
!      トロイダルポテンシャルの境界値問題
!
!履歴  2008/01/13  竹広真一  wu_test_torvelbc.f90 より改変
!
program wu_test_torvelbci

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8        ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmi)           :: xyr_TorVel0
  real(8), dimension(im,jm,0:kmi)           :: xyr_TorVel
  real(8), dimension((nm+1)*(nm+1),0:lmi)   :: wu_TorVel
  character(len=1), dimension(2),parameter :: BCond=(/'F','R'/)

  real(8), dimension(im,jm,0:kmi)           :: xyr_True

  real(8), parameter  :: eps=1D-15

  integer :: i,j,k,l

  call MessageNotify('M','wu_test_torvelbc', &
       'wu_module  wu_TorBoundaryGrid subroutine test')

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  do l=1,2

     ! P_10
     !xyr_TorVel = sin(xyr_lat) * cos( pi*(xyr_rad-ri)/(ro-ri) )
     ! P_1_1
     !xyr_TorVel = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/(ro-ri) )
     xyr_TorVel = 2*sin(xyr_lat)**2 * (xyr_rad-ri)**2 * xyr_Rad**2

     xyr_TorVel0 = xyr_TorVel
     wu_TorVel = wu_xyr(xyr_TorVel)
     call wu_TorBoundaryGrid(wu_TorVel,cond=BCond(l),new=.true.)
!!$     call wu_TorBoundary(wu_TorVel,cond=BCond(l),new=.true.)
     xyr_TorVel = xyr_wu(wu_TorVel)


     ! 内部チェック
     xyr_True = xyr_TorVel - xyr_TorVel0

     do k=1,kmi
        do j=1,jm
           do i=1,im
              if ( abs(xyr_True(i,j,k)) > eps ) then
                 write(6,*) 'internal value. : ', i,j,xyr_True(i,j,k)
                 call MessageNotify('E','wu_test_torvelbc',&
                              'internal value error too large')
              endif
           enddo
        enddo
     enddo

     ! 上端境界チェック
     if( BCond(l) == 'F' ) then
        xyr_True = xyr_wr(wr_DRad_wu(wu_Torvel)- (wr_wu(wu_Torvel)/wr_Rad))
     else
        xyr_True = xyr_TorVel
     endif

     do j=1,jm
        do i=1,im
           if ( abs(xyr_True(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyr_True(i,j,0)
              call MessageNotify('E','wu_test_torvelbc',&
                              'Top B.C. error too large')
           endif
        enddo
     enddo
     call MessageNotify('M','wu_test_torvelbc', &
                        BCond(l)//'-Top B.C. test succeeded!')

  enddo

  call MessageNotify('M','wu_test_torvelbc', &
       'wu_module  wu_TorgBoundaryGrid subroutine test succeeded!')

end program wu_test_torvelbci

