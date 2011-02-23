!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wtu_module テストプログラム
!
!      ポロイダルポテンシャルの境界値問題
!
!履歴  2008/01/13  竹広真一  wt_test_polvelbc.f90 より改変
!
program wtu_test_polvelbc

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8       ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmo)     :: xyz_Poloidal
  real(8), dimension(im,jm,0:kmo)     :: xyz_LaplaPol
  real(8), dimension(im,jm,0:kmo)     :: xyz_LaplaPol1
  real(8), dimension(im,jm,0:kmo)     :: xyz_True
  character(len=2), dimension(4), parameter :: BCond=(/'FF','FR','RF','RR'/)

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: k, l, i, j

  call MessageNotify('M','wtu_test_polvelbc', &
       'wt_module  wtu_LaplaPol2polGrid_wt function tests')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  do l=1,4

     ! P_10
     xyz_Poloidal = sin(xyz_Lat) * sin( pi*(xyz_Rad-ri)/(ro-ri) )
     xyz_LaplaPol = xyz_wt(wt_Lapla_wt(wt_xyz(xyz_Poloidal)))
     !xyz_LaplaPol = sin(xyz_Lat) * sin( pi*(xyz_Rad-ri)/(ro-ri) )
     ! P_1_1
     !xyz_LaplaPol = cos(xyz_Lat)*cos(xyz_Lon)* sin( pi*(xyz_Rad-ri)/(ro-ri) )
     !xyz_LaplaPol = 2*sin(xyz_Lat)**2 * sin( pi*(xyz_Rad-ri)/(ro-ri) )

     !xyz_Poloidal = xyz_wz(wz_LaplaPol2pol_wz(wz_xyz(xyz_LaplaPol),BCond(l)))
     !xyz_Poloidal = xyz_wt(wt_LaplaPol2PolTau_wt(wt_xyz(xyz_LaplaPol),BCond(l)))

     xyz_Poloidal = xyz_wt(wt_LaplaPol2PolGrid_wt(wt_xyz(xyz_LaplaPol),BCond(l),new=.true.))

     xyz_LaplaPol1 = xyz_wt(wt_Lapla_wt(wt_xyz(xyz_Poloidal)))

     !---------------- 内部チェック -----------------------
     xyz_True = xyz_LaplaPol1 - xyz_LaplaPol

     do k=2,kmo-2
        do j=1,jm
           do i=1,im
              if ( abs(xyz_True(i,j,k)) > eps ) then
                 write(6,*) 'internal value. : ', i,j,xyz_True(i,j,k)
                 call MessageNotify('E','wtu_test_polvelbc',&
                              'internal value error too large')
              endif
           enddo
        enddo
     enddo

     call MessageNotify('M','wtu_test_polvelbc', &
                        'internal value test succeeded!')

     !--------- 上端境界チェック ----------

     !----- Φ=0 at the top ---------
     do j=1,jm
        do i=1,im
           if ( abs(xyz_Poloidal(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyz_Poloidal(i,j,0)
              call MessageNotify('E','wtu_test_polvelbc',&
                              'Φ=0 Top B.C. error too large')
           endif
        enddo
     enddo

     !----- dΦ/dr=0, d^2Φ/dr^2 at the top ---------
     if( BCond(l)(1:1) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal))))
     else
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal)))
     endif
     do j=1,jm
        do i=1,im
           if ( abs(xyz_True(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyz_True(i,j,0)
              call MessageNotify('E','wtu_test_polvelbc',&
                              BCond(l)//'-Top B.C. error too large')
           endif
        enddo
     enddo

     call MessageNotify('M','wtu_test_polvelbc', &
                        BCond(l)//'-Top B.C. test succeeded!')

     !--------- 下端境界チェック ----------

     !----- Φ=0 at the bottom ---------
     do j=1,jm
        do i=1,im
           if ( abs(xyz_Poloidal(i,j,kmo)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyz_Poloidal(i,j,kmo)
              call MessageNotify('E','wtu_test_polvelbc',&
                              'Φ=0 Bottom B.C. error too large')
           endif
        enddo
     enddo

     !----- dΦ/dr=0, d^2Φ/dr^2 at the bottom ---------
     if( BCond(l)(2:2) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal))))
     else
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal)))
     endif
     do j=1,jm
        do i=1,im
           if ( abs(xyz_True(i,j,kmo)) > eps ) then
              write(6,*) 'Bottom B.C. : ', i,j,xyz_True(i,j,kmo)
              call MessageNotify('E','wtu_test_polvelbc',&
                              BCond(l)//'-Bottom B.C. error too large')
           endif
        enddo
     enddo

     call MessageNotify('M','wtu_test_polvelbc', &
                        BCond(l)//'-Bottom B.C. test succeeded!')

  end do

  write( 6,* ) 
  call MessageNotify('M','wtu_test_polvelbc', &
       'wt_module  wt_LaplaPol2polGrid_wt function tests succeeded!')



end program wtu_test_polvelbc
