!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module サンプルプログラム : 線形解析(チェビシェフ空間)
!
!      拡散方程式 d zeta/dt = kappa d^2 zeta/dx^2
!
!履歴  2002/07/06  竹広真一
!
!備考  lapack, blas ライブラリが必要. Debian/GNU Linux + Fujitsu frt ならば
!      lapack, lapack-deb パッケージをインストールして, 
!         -llapack -lblas -L/usr/lib/gcc-lib/i386-linux/2.95.4 -lg2c
!      といったオプションをつけるべし. 
!
!
program lapack_eigen_test

  use lapack_eigen
  implicit none

  real(8), dimension(:,:), allocatable  :: amatrix
  real(8), dimension(:),   allocatable  :: eigval_r, eigval_i
  real(8), dimension(:,:), allocatable  :: eigvec_r, eigvec_i
  integer                     :: info
  integer                     :: i, j

!--------------- 3x3 行列 ------------------
!  VALUE = 1, 2, 3
!  VECTOR = (-15,12,4), (-16,13,4), (-4,3,1)

  allocate(amatrix(3,3))
  allocate(eigval_r(3), eigval_i(3))
  allocate(eigvec_r(3,3), eigvec_i(3,3))

  amatrix(:,1) = (/ 33, -24,  -8 /)
  amatrix(:,2) = (/ 16, -10,  -4 /)
  amatrix(:,3) = (/ 72, -57, -17 /)

  call deigen_lapack(amatrix,eigval_r,eigval_i,eigvec_r,eigvec_i,info,sort=' R')

  do i=1,3
     write(6,*) 'EIGENVALUE  : ', eigval_r(i),eigval_i(i)
     write(6,*) 'EIGENVECTOR : ', (eigvec_r(j,i),eigvec_i(j,i),j=1,3)
  enddo

  deallocate(amatrix)
  deallocate(eigval_r, eigval_i)
  deallocate(eigvec_r, eigvec_i)

!--------------- 4x4 行列 ------------------
!  VALUE = 12, 1+5I, 1-5i, 2
!  VECTOR = (1,-1,1,1), (1,-i,-i,-1), (1,i,i,-1), (1,1,-1,1)

  allocate(amatrix(4,4))
  allocate(eigval_r(4), eigval_i(4))
  allocate(eigvec_r(4,4), eigvec_i(4,4))

  amatrix(:,1) = (/  4,  0,  5,  3 /)
  amatrix(:,2) = (/ -5,  4, -3,  0 /)
  amatrix(:,3) = (/  0, -3,  4,  5 /)
  amatrix(:,4) = (/  3, -5,  0,  4 /)

  call deigen_lapack(amatrix,eigval_r,eigval_i,eigvec_r,eigvec_i,info)

  do i=1,4
     write(6,*) 'EIGENVALUE  : ', eigval_r(i),eigval_i(i)
     write(6,*) 'EIGENVECTOR : '
     do j=1,4
        write(6,*) eigvec_r(j,i),eigvec_i(j,i)
     enddo
  enddo

  deallocate(amatrix)
  deallocate(eigval_r, eigval_i)
  deallocate(eigvec_r, eigvec_i)

end program lapack_eigen_test
