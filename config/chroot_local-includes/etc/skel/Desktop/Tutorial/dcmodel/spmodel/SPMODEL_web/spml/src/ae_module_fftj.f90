!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ae_module_fftj
!
!      spml/ae_module_fftj モジュールは 1 次元周期境界条件の下での流体運動を
!      実フーリエ変換によるスペクトル法で数値計算するための Fortran90 関数
!      を提供する.
!
!      2 次元データの 1 次元に関して同時にスペクトル計算を実行するための
!      関数も提供しており, 2, 3 次元領域での計算のベースも提供する.
!
!      このモジュールは内部で fftj の Fortran77 サブルーチンを
!      呼んでいる.
!
!      正変換の定義およびスペクトルデータの格納方法については fftj と
!      異なっているので以下のコメントに注意されたい. 
!
!
!履歴  2009/09/09  竹広真一  fftj の Fortran 90 版. ae_module.f90 より改造
!
!++
module ae_module_fftj
  !
  != ae_module_fftj
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: ae_module_fftj.f90,v 1.2 2009-09-12 12:38:33 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !
  ! spml/ae_module_fftj モジュールは 1 次元周期境界条件の下での流体運動を
  ! 実フーリエ変換によるスペクトル法で数値計算するための Fortran90 関数
  ! を提供する.
  !
  ! 2 次元データの 1 次元に関して同時にスペクトル計算を実行するための
  ! 関数も提供しており, 2, 3 次元領域での計算のベースも提供する.
  !
  ! このモジュールは内部で fftj の Fortran77 サブルーチンを
  ! 呼んでいる.
  !
  ! 正変換の定義およびスペクトルデータの格納方法については fftj と
  ! 異なっているので以下のコメントに注意されたい. 
  !
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (e_, g_, ae_, ag_) は, 返す値の形を示している.
  !   e_  :: スペクトルデータ,
  !   g_  :: 1 次元格子点データ,
  !   ae_ :: 1 次元スペクトルデータが複数並んだ 2 次元データ,
  !   ag_ :: 1 次元格子点データが複数並んだ 2 次元データ.
  !
  ! * 関数名の間の文字列(Dx)は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_e, _ae, _g, _ag) は, 入力変数の形スペクトルデータおよび
  !   格子点データであることを示している.
  !   _e  :: スペクトルデータ
  !   _g  :: 1 次元格子点データ
  !   _ae :: 1 次元スペクトルデータが複数並んだ 2 次元データ
  !   _ag :: 1 次元格子点データが複数並んだ 2 次元データ
  !
  !=== 各データの種類の説明
  !
  ! * g : 1 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:im-1).
  !   * im は X 座標の格子点数であり, サブルーチン ae_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * e : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km).
  !   * km は X 方向の最大波数であり, サブルーチン ae_Initial にて
  !     あらかじめ設定しておく. 
  !   * スペクトルデータの格納のされ方は 0:km までが cos(kx) の係数, 
  !     -km:-1 が sin(kx) の係数となっている.
  !
  ! * ag : 1 次元(X)格子点データの並んだ 2 次元データ.
  !   * 変数の種類と次元は real(8), dimension(:,0:im-1).
  !     第 2 次元が X 方向を表す.
  !
  ! * ae : 1 次元スペクトルデータの並んだ 2 次元データ.
  !   * 変数の種類と次元は real(8), dimension(:,-km:km).
  !     第 2 次元がスペクトルを表す.
  !
  ! * g_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * e_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * ag_ で始まる関数が返す値は 1 次元格子点データの並んだ
  !   2 次元データに同じ.
  !
  ! * ae_ で始まる関数が返す値は 1 次元スペクトルデータの並んだ
  !   2 次元データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したもののことである.
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化
  !
  ! ae_Initial       :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  !
  !==== 座標変数
  !
  ! g_X              :: 格子点座標(X)を格納した 1 次元配列
  ! g_X_Weight       :: 重み座標を格納した 1 次元配列
  !
  !==== 基本変換
  !
  ! g_e, ag_ae       :: スペクトルデータから格子データへの変換
  ! e_g, ae_ag       :: 格子データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! e_Dx_e, ae_Dx_ae :: スペクトルデータに X 微分を作用させる
  !
  !==== 積分・平均
  !
  ! a_Int_ag, a_Avr_ag :: 1 次元格子点データの並んだ 2 次元配列の積分および平均
  ! Int_g, Avr_g       :: 1 次元格子点データの積分および平均
  !
  !
  use dc_message
  implicit none

  private
  public ae_Initial                       ! 初期化ルーチン
  public ag_ae, ae_ag, g_e, e_g           ! 基本変換
  public ae_Dx_ae, e_Dx_e                 ! 微分
  public a_Int_ag, Int_g, a_Avr_ag, Avr_g ! 積分・平均
  public g_X, g_X_Weight                  ! 座標変数

  integer, parameter :: index2max=11      ! Max(im) = 2048 = 2^{11}
  integer            :: im=32             ! 格子点の数
  integer            :: km=10             ! 切断波数
  double precision   :: xl=1.0            ! 領域の大きさ

  real(8), parameter                 :: pi=3.1415926535897932385D0

  real(8), allocatable :: g_x(:)         ! 格子点座標(X)を格納した 1 次元配列.
  real(8), allocatable :: g_x_weight(:)  ! 重み座標を格納した 1 次元配列.
                                         ! X 方向の格子点の間隔が格納してある.

  save im, km, xl, g_X, g_X_Weight

  contains
  !--------------- 初期化 -----------------
    subroutine ae_Initial(i,k,xmin,xmax)
      !
      ! スペクトル変換の格子点数, 波数, 領域の大きさを設定する.
      !
      ! 他の関数や変数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない.
      !
      integer,intent(in) :: i              ! 格子点の数
      integer,intent(in) :: k              ! 切断波数
      real(8),intent(in) :: xmin, xmax     ! X 座標範囲

      integer :: ii, index2

      im = i
      km = k
      xl = xmax-xmin

      index2 = 1
      do while ( im /= 2**index2 .AND. index2 <= index2max ) 
         index2=index2+1
      enddo

      if ( index2 > index2max ) then
         call MessageNotify('E','ae_Initial', &
              'Number of grid points should be 2^N and <= 2048')
      else if ( km <= 0 ) then
         call MessageNotify('E','ae_Initial', &
              'Number of waves should be positive')
      elseif ( km >= im/2 ) then
         call MessageNotify('E','ae_Initial', &
              'KM shoud be less than IM/2')
      endif

      allocate(g_x(0:im-1))
      do ii=0,im-1
         g_X(ii) = xmin + xl/im*ii
      enddo

      allocate(g_x_weight(0:im-1))
      g_X_Weight = xl/im

      call MessageNotify(&
        'M','ae_initial','ae_module_fftj (2009/09/12) is initialized')

    end subroutine ae_Initial

  !--------------- 基本変換 -----------------

    function ag_ae(ae)
      !
      ! スペクトルデータから格子点データへ逆変換する(2 次元データ用)
      !
      ! スペクトル逆変換の定義は以下のとおり. 
      !
      !   スペクトルデータ e_k (k=-km,...,km)
      !   格子点データ     g_j (j=-0,...,im-1)
      !
      !  g_j = e_0 + 
      !      + 2\sum_{k=1}^{km}(e_k\cos(2\pi jk/im) - e_{-k}\sin(2\pi jk/im))
      !                                                    (j=0,1,...,im-1).
      !                                                                
      !
      real(8), dimension(:,-km:), intent(in)  :: ae     !(in)  スペクトルデータ
      real(8), dimension(size(ae,1),0:im-1)   :: ag_ae  !(out) 格子点データ

      real(8), dimension(size(ae,1),0:im-1)   :: ag_in  ! 入力スペクトルデータ
      real(8), dimension(im)                  :: g_work
      real(8), dimension(im*3)                :: xt
      integer, dimension(2)                   :: ip
      integer :: m, k

      if ( im > 2048 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data exceeds 2048.')
      else if ( mod(im,2) /= 0 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data should be 2^N.')
      else if ( size(ae,2) < 2*km+1 ) then
         call MessageNotify('E','ag_ae', &
              'The Fourier dimension of input data too small.')
      elseif ( size(ae,2) > 2*km+1 ) then
         call MessageNotify('W','ag_ae', &
              'The Fourier dimension of input data too large.')
      endif

      ag_in = 0.0D0
      ag_in(:,0)=ae(:,0)
      ag_in(:,1)=0
      do k=1,km
         ag_in(:,2*k)=ae(:,k)
         ag_in(:,2*k+1)=ae(:,-k)
      enddo
      ag_in(:,2*km+2:im-1)=0

      call fjrini(im,2,1,ip,xt)
      do m=1,size(ae,1)
         call fjrrun(ag_in(m,:),ag_ae(m,:),g_work,xt,ip)
      enddo

    end function ag_ae

    function g_e(e)
      !
      ! スペクトルデータから格子点データへ逆変換する(1 次元データ用)
      !
      ! スペクトル逆変換の定義は以下のとおり. 
      !
      !   スペクトルデータ e_k (k=-km,...,km)
      !   格子点データ     g_j (j=-0,...,im-1)
      !
      !  g_j = e_0 + 
      !      + 2\sum_{k=1}^{km}(e_k\cos(2\pi jk/im) - e_{-k}\sin(2\pi jk/im))
      !                                                    (j=0,1,...,im-1).
      !
      real(8), dimension(0:im-1)             :: g_e  !(out) 格子点データ
      real(8), dimension(-km:km), intent(in) :: e    !(in)  スペクトルデータ

      real(8), dimension(1,size(e))  :: ae_work
      real(8), dimension(1,0:im-1)   :: ag_work

      ae_work(1,:) = e
      ag_work = ag_ae(ae_work)
      g_e = ag_work(1,:)

    end function g_e

    function ae_ag(ag)
      !
      ! 格子点データからスペクトルデータへ正変換する(2 次元データ用)
      !
      ! スペクトル正変換の定義は以下のとおり. 
      !
      !   格子点データ     g_j (j=-0,...,im-1)
      !   スペクトルデータ e_k (k=-km,...,km)
      !
      !   e_0 = (1/im)\sum_{j=0}^{im-1} g_j
      !   e_k = (1/im)\sum_{j=0}^{im-1} g_j \cos(2\pi jk/im)       (k=1,2,...,km)
      !   e_{-k} = - (1/im)\sum_{j=0}^{im-1} g_j \sin(2\pi jk/im)  (k=1,2,...,km)
      !
      real(8), dimension(:,:), intent(in)     :: ag     !(in)  格子点データ
      real(8), dimension(size(ag,1),-km:km)   :: ae_ag  !(out) スペクトルデータ

      real(8), dimension(size(ag,1),0:im-1)   :: ag_out ! スペクトルデータ

      real(8), dimension(im)                  :: g_work
      real(8), dimension(im*3)                :: xt
      integer, dimension(2)                   :: ip
      integer :: m, k

      if ( im > 2048 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data exceeds 2048.')
      else if ( mod(im,2) /= 0 ) then
         call MessageNotify('E','ag_ae', &
              'The dimension of output data should be 2^N.')
      else if ( size(ag,2) < im ) then
         call MessageNotify('E','ae_ag', &
              'The Grid points of input data too small.')
      elseif ( size(ag,2) > im ) then
         call MessageNotify('W','ae_ag', &
              'The Grid points of input data too large.')
      endif

      call fjrini(im,2,2,ip,xt)
      do m=1,size(ag,1)
         call fjrrun(ag(m,:),ag_out(m,:),g_work,xt,ip)
      enddo

      do k=1,km
         ae_ag(:,k)  = ag_out(:,2*k)
         ae_ag(:,-k) = ag_out(:,2*k+1)
      enddo
      ae_ag(:,0) = ag_out(:,0)

      ae_ag = ae_ag/im

    end function ae_ag

    function e_g(g)
      !
      ! 格子点データからスペクトルデータへ正変換する(1 次元データ用)
      !
      ! スペクトル正変換の定義は以下のとおり. 
      !
      !   格子点データ     g_j (j=-0,...,im-1)
      !   スペクトルデータ e_k (k=-km,...,km)
      !
      !   e_0 = (1/im)\sum_{j=0}^{im-1} g_j
      !   e_k = (1/im)\sum_{j=0}^{im-1} g_j \cos(2\pi jk/im)       (k=1,2,...,km)
      !   e_{-k} = - (1/im)\sum_{j=0}^{im-1} g_j \sin(2\pi jk/im)  (k=1,2,...,km)
      !
      real(8), dimension(-km:km)              :: e_g   !(out) スペクトルデータ
      real(8), dimension(0:im-1), intent(in)  :: g     !(in)  格子点データ

      real(8), dimension(1,size(g))        :: ag_work
      real(8), dimension(1,-km:km)         :: ae_work

      ag_work(1,:) = g
      ae_work = ae_ag(ag_work)
      e_g = ae_work(1,:)

    end function e_g

  !--------------- 微分計算 -----------------
    function ae_Dx_ae(ae)
      !
      ! 入力スペクトルデータに X 微分を作用する(2 次元データ).
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      !
      real(8), dimension(:,-km:), intent(in)  :: ae
                                         !(in)  入力スペクトルデータ
      real(8), dimension(size(ae,1),-km:km)   :: ae_dx_ae
                                         !(out) 入力スペクトルデータの X 微分

      integer k

      if ( size(ae,2) < 2*km+1 ) then
         call MessageNotify('W','ae_Dx_ae', &
              'The Fourier dimension of input data too small.')
      elseif ( size(ae,2) > 2*km+1 ) then
         call MessageNotify('W','ae_Dx_ae', &
              'The Fourier dimension of input data too large.')
      endif

      do k=-km,km
         ae_Dx_ae(:,k) = -(2*pi*k/xl)*ae(:,-k)
      enddo
    end function ae_dx_ae

    function e_Dx_e(e)
      !
      ! 入力スペクトルデータに X 微分を作用する(1 次元データ).
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      !
      real(8), dimension(-km:km), intent(in)     :: e
                                         !(in)  入力スペクトルデータ
      real(8), dimension(-km:km)                 :: e_Dx_e
                                         !(out) 入力スペクトルデータの X 微分

      real(8), dimension(1,-km:km)               :: ae_work

      ae_work(1,:) = e
      ae_work = ae_Dx_ae(ae_work)
      e_Dx_e = ae_work(1,:)

    end function e_Dx_e

  !--------------- 積分計算 -----------------
    function a_Int_ag(ag)
      !
      ! 1 次元格子点データが並んだ 2 次元配列の積分
      !
      real(8), dimension(:,0:), intent(in)     :: ag        !(in) 格子点データ
      real(8), dimension(size(ag,1))           :: a_Int_ag  !(out) 積分結果
      integer :: i

      if ( size(ag,2) < im ) then
         call MessageNotify('E','ae_Int_ag', &
              'The Grid points of input data too small.')
      elseif ( size(ag,2) > im ) then
         call MessageNotify('W','ae_Int_ag', &
              'The Grid points of input data too large.')
      endif

      a_Int_ag = 0.0d0
      do i=0,im-1
         a_Int_ag(:) = a_Int_ag(:) + ag(:,i)*g_X_Weight(i)
      enddo
    end function a_Int_ag

    function Int_g(g)
      !
      ! 1 次元格子点データの積分
      !
      real(8), dimension(0:im-1), intent(in)   :: g      !(in) 格子点データ
      real(8)                                  :: Int_g  !(out) 積分結果

      Int_g = sum(g*g_X_Weight)

    end function Int_g

    function a_Avr_ag(ag)
      !
      ! 1 次元格子点データが並んだ 2 次元配列の平均
      !
      real(8), dimension(:,0:), intent(in)     :: ag        !(in) 格子点データ
      real(8), dimension(size(ag,1))           :: a_Avr_ag  !(out) 平均結果

      a_Avr_ag = a_Int_ag(ag)/sum(g_X_Weight)

    end function a_Avr_ag

    function Avr_g(g)
      !
      ! 1 次元格子点データの平均
      !
      real(8), dimension(0:im-1), intent(in)   :: g
      real(8)                                  :: Avr_g

      Avr_g = Int_g(g)/sum(g_X_Weight)

    end function Avr_g

end module ae_module_fftj
