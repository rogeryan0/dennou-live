!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wu_module テストプログラム
!
!      ポロイダルポテンシャルの境界値問題
!
!履歴  2008/01/01  竹広真一
!      2008/01/01  佐々木洋平  配列宣言の修正
!
program wu_test_polvelbc

  use dc_message, only : MessageNotify
  use wu_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=0.5               ! 球半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Poloidal
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_LaplaPol
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_LaplaPol1
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_True
  real(8), dimension((nm+1)**2,0:lm) :: wu_Poloidal
  character(len=1), dimension(2), parameter :: BCond=(/'F','R'/)

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-14

  integer :: k, l, i, j

  call MessageNotify('M','wu_test_polvelbc', &
       'wu_module  wu_LaplaPol2polGrid_wu function tests')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wu_initial(im,jm,km,nm,lm,ra)

  do l=1,2

     ! P_10
     xyr_Poloidal = sin(xyr_Lat) * xyr_Rad * ((xyr_Rad-ra)*(xyr_Rad+ra))**2
     xyr_LaplaPol = xyr_wr(wr_Lapla_wu(wu_xyr(xyr_Poloidal)))
     !xyr_LaplaPol = sin(xyr_Lat) * sin( pi*(xyr_Rad-ri)/(ro-ri) )
     ! P_1_1
     !xyr_LaplaPol = cos(xyr_Lat)*cos(xyr_Lon)* xyr_Rad * (xyr_Rad-ra) 
     !xyr_LaplaPol = 2*sin(xyr_Lat)**2 * xyr_Rad * (xyr_Rad-ra) 

     !xyr_Poloidal = xyr_wz(wr_LaplaPol2pol_wr(wr_xyr(xyr_LaplaPol),BCond(l)))
     !xyr_Poloidal = xyr_wu(wu_LaplaPol2PolTau_wu(wu_xyr(xyr_LaplaPol),BCond(l)))

     wu_Poloidal = wu_LaplaPol2PolGrid_wu(wu_xyr(xyr_LaplaPol),BCond(l),new=.true.)

     xyr_LaplaPol1 = xyr_wr(wr_Lapla_wu(wu_Poloidal))
     xyr_Poloidal = xyr_wu(wu_Poloidal)

     !---------------- 内部チェック -----------------------
     xyr_True = xyr_LaplaPol1 - xyr_LaplaPol

     do k=2,km
        do j=1,jm
           do i=0,im-1
              if ( abs(xyr_True(i,j,k)) > eps ) then
                 write(6,*) 'internal value. : ', i,j,xyr_True(i,j,k)
                 call MessageNotify('E','wu_test_polvelbc',&
                              'internal value error too large')
              endif
           enddo
        enddo
     enddo

     call MessageNotify('M','wu_test_polvelbc', &
                        'internal value test succeeded!')

     !--------- 上端境界チェック ----------

     !----- Φ=0 at the top ---------
     do j=1,jm
        do i=0,im-1
           if ( abs(xyr_Poloidal(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyr_Poloidal(i,j,0)
              call MessageNotify('E','wu_test_polvelbc',&
                              'Φ=0 Top B.C. error too large')
           endif
        enddo
     enddo

     !----- dΦ/dr=0, d^2Φ/dr^2 at the top ---------
     if( BCond(l)(1:1) == 'F' ) then
        xyr_True = xyr_wr(wr_DRad2_wu(wu_Poloidal))
     else
        xyr_True = xyr_wr(wr_DRad_wu(wu_Poloidal))
     endif
     do j=1,jm
        do i=0,im-1
           if ( abs(xyr_True(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyr_True(i,j,0)
              call MessageNotify('E','wu_test_polvelbc',&
                              BCond(l)//'-Top B.C. error too large')
           endif
        enddo
     enddo

     call MessageNotify('M','wu_test_polvelbc', &
                        BCond(l)//'-Top B.C. test succeeded!')
  end do

  write( 6,* ) 
  call MessageNotify('M','wu_test_polvelbc', &
       'wu_module  wu_LaplaPol2polGrid_wu function tests succeeded!')


end program wu_test_polvelbc
