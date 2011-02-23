!--
!----------------------------------------------------------------------
! Copyright (c) 2008--2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  la_module
!
!   spml/la_module モジュールは球面上での経度方向に一様な帯状的子午面 2 次元
!   流体運動をルジャンドル多項式を用いたスペクトル法によって数値計算する
!   ための Fortran90 関数を提供する. 
!
!   内部で ISPACK の LTPACK の Fortran77 サブルーチンを呼んでいる.
!   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!   ついては ISPACK/LTPACK のマニュアルを参照されたい.
!
!履歴  2008/12/24  竹広真一  多層用
!
!++
module la_module
  !
  != la_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: la_module.f90,v 1.5 2009-02-28 21:33:47 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/l_module モジュールは球面上での経度方向に一様な帯状的子午面 2 次元
  ! 流体運動をルジャンドル多項式を用いたスペクトル法によって数値計算する
  ! ための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK の LTPACK の Fortran77 サブルーチンを呼んでいる.
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/LTPACK のマニュアルを参照されたい.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (la_, ya_, l_, y_) は, 返す値の形を示している.
  !   la_ :: スペクトル(ルジャンドル多項式成分)データ(鉛直多層)
  !   ya_ :: 2 次元緯度鉛直格子点データ
  !   l_  :: スペクトル(ルジャンドル多項式成分)データ
  !   y_  :: 1 次元緯度格子点データ
  !
  ! * 関数名の間の文字列(GradLat, DivLat, Lapla, LaplaInv)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_la, _ya, _l, _y) は, 入力変数の形スペクトルデータ
  !   および格子点データであることを示している.
  !   _la :: スペクト(ルジャンドル多項式成分)ルデータ(鉛直多層)
  !   _ya :: 緯度鉛直 2 次元格子点データ
  !   _l  :: スペクト(ルジャンドル多項式成分)ルデータ
  !   _y  :: 緯度方向 1 次元格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * la : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(0:nm,:). 
  !   * nm はルジャンドル多項式の最大次数であり, 
  !     サブルーチン la_Initial にてあらかじめ設定しておく. 
  !
  ! * ya : 緯度方向 1 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(1:jm,:).
  !
  ! * l : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(0:nm). 
  !   * nm はルジャンドル多項式の最大次数であり, サブルーチン la_Initial にて
  !     あらかじめ設定しておく. 
  !
  ! * y : 緯度方向 1 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(1:jm).
  !
  ! * la_, l_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * ya_, y_ で始まる関数が返す値は 2, 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! la_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! y_Lat        ::  格子点座標(緯度, 経度座標)を格納した 1 次元配列
  ! y_Lat_Weight ::  重み座標を格納した 1 次元配列
  !
  !==== 基本変換
  !
  ! ya_la :: スペクトルデータから格子データへの変換
  ! la_ya :: 格子データからスペクトルデータへの変換
  ! y_l   :: スペクトルデータから格子データへの変換
  ! l_y   :: 格子データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! la_Lapla_la    :: スペクトルデータにラプラシアンを作用させる
  ! la_LaplaInv_la :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! ya_GradLat_la  :: スペクトルデータに勾配型緯度微分∂/∂φを作用させる
  ! la_DivLat_ya   :: 格子データに
  !                   発散型緯度微分 1/cosφ・∂(g cosφ)/∂φを作用させる
  !
  ! l_Lapla_l      :: スペクトルデータにラプラシアンを作用させる
  ! l_LaplaInv_l   :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! y_GradLat_l    :: スペクトルデータに勾配型緯度微分∂/∂φを作用させる
  ! l_DivLat_y     :: 格子データに
  !                   発散型緯度微分 1/cosφ・∂(g cosφ)/∂φを作用させる
  !
  !==== 微分(λ,μ=sinφ 座標)
  !
  ! ya_GradMu_la   :: スペクトルデータに
  !                   勾配型緯度微分 (1-μ^2)∂/∂μを作用させる
  ! la_DivMu_ya    :: 格子データに発散型緯度微分∂/∂μを作用させる
  ! y_GradMu_l     :: スペクトルデータに
  !                   勾配型緯度微分 (1-μ^2)∂/∂μを作用させる
  ! l_DivMu_y      :: 格子データに発散型緯度微分∂/∂μを作用させる
  !
  !==== 補間
  !
  ! Interpolate_la :: スペクトルデータから任意の点での値を求める. 
  ! Interpolate_l  :: スペクトルデータから任意の点での値を求める. 
  !
  !==== 積分・平均
  !
  ! a_IntLat_ya, a_AvrLat_ya   :: 2 次元格子点データの緯度方向積分および平均
  ! IntLat_y, AvrLat_y         :: 1 次元格子点データの緯度方向積分および平均
  !
  !==== スペクトル解析
  !
  ! 作成中
  !
  use dc_message, only : MessageNotify
  use l_module

  implicit none

  private

  public la_Initial                           ! 初期化

  public y_Lat                                ! 格子座標
  public y_Lat_Weight                         ! 格子座標重み

  public ya_la, la_ya                         ! 変換関数
  public la_Lapla_la, la_LaplaInv_la          ! ラプラシアンと逆演算
  public ya_GradLat_la                        ! 勾配型微分
  public la_DivLat_ya                         ! 発散型微分

  public y_l, l_y                             ! 変換関数
  public l_Lapla_l, l_LaplaInv_l              ! ラプラシアンと逆演算
  public y_GradLat_l                          ! 勾配型微分
  public l_DivLat_y                           ! 発散型微分

  public ya_GradMu_la                         ! 勾配型微分
  public la_DivMu_ya                          ! 発散型微分

  public y_GradMu_l                           ! 勾配型微分
  public l_DivMu_y                            ! 発散型微分

  public a_IntLat_ya, a_AvrLat_ya             ! 緯度平均
  public IntLat_y, AvrLat_y                   ! 緯度平均

  public a_Interpolate_la                     ! 補間計算
  public Interpolate_l                        ! 補間計算

  integer               :: jm=32              ! 格子点の設定(南北)
  integer               :: nm=21              ! 切断波数の設定

  save jm, nm

contains

  !--------------- 初期化 -----------------
    subroutine la_initial(n_in,j_in,l_init)
      !
      ! スペクトル変換の格子点数, 波数を設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
      ! しなければならない. 
      !
      integer,intent(in) :: j_in              !(in) 格子点数(南北)
      integer,intent(in) :: n_in              !(in) 切断波数の設定
      logical,intent(in),optional :: l_init   !(in) l_initial の call スイッチ

      if ( .not. present(l_init) ) then
         call l_Initial(n_in,j_in)
      else if ( l_init ) then
         call l_Initial(n_in,j_in)
      endif

      jm = j_in ; nm = n_in

      call MessageNotify(&
        'M','la_initial','la_module (2009/01/09) is initialized')

    end subroutine la_initial

  !--------------- 基本変換 -----------------
    function ya_la(la_data)
      !
      ! スペクトルデータから格子データへ変換する(多層用).
      !
      real(8), intent(in)   :: la_data(:,:)
      !(in) スペクトルデータ(0:nm,:)

      real(8)               :: ya_la(1:jm,size(la_data,2))
      !(out) 格子点データ

      integer               :: k

      do k=1,size(la_data,2)
         ya_la(:,k) = y_l(la_data(:,k))
      end do

    end function ya_la

    function la_ya(ya_data)
      !
      ! 格子データからスペクトルデータへ変換する(多層用).
      !
      real(8), intent(in)   :: ya_data(:,:)
      !(in) スペクトルデータ(1:jm,:)

      real(8)               :: la_ya(0:nm,size(ya_data,2))
      !(out) 格子点データ

      integer               :: k

      do k=1,size(ya_data,2)
         la_ya(:,k) = l_y(ya_data(:,k))
      end do

    end function la_ya

  !--------------- 微分計算 -----------------
    function la_Lapla_la(la_data)
      !
      ! 入力スペクトルデータにラプラシアン
      !
      !    ▽^2 = 1/cosφ・∂/∂φ(cosφ∂/∂φ)
      !
      ! を作用する(多層用). 
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) 入力スペクトルデータ(0:nm,:)

      real(8)              :: la_Lapla_la(0:nm,size(la_data,2))
      !(out) 入力スペクトルデータのラプラシアン

      integer               :: k

      do k=1,size(la_data,2)
         la_Lapla_la(:,k) = l_Lapla_l(la_data(:,k))
      end do
      
    end function la_Lapla_la

    function la_LaplaInv_la(la_data)
      !
      ! 入力スペクトルデータに逆ラプラシアン
      !
      !    ▽^{-2}
      !      =[1/cosφ・∂/∂φ(cosφ∂/∂φ)]^{-1}
      !
      ! を作用する(多層用). 
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) 入力スペクトルデータ(0:nm,:)

      real(8)              :: la_LaplaInv_la(0:nm,size(la_data,2))
      !(out) スペクトルデータの逆ラプラシアン

      integer               :: k

      do k=1,size(la_data,2)
         la_LaplaInv_la(:,k) = l_LaplaInv_l(la_data(:,k))
      end do
    end function la_LaplaInv_la

    function ya_GradLat_la(la_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(多層用).
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) 入力スペクトルデータ(0:nm,:)

      real(8)              :: ya_GradLat_la(1:jm,size(la_data,2))
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      integer               :: k

      do k=1,size(la_data,2)
         ya_GradLat_la(:,k) = y_GradLat_l(la_data(:,k))
      end do

    end function ya_GradLat_la

    function la_DivLat_ya(ya_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す(多層用).. 
      !
      real(8), intent(in)  :: ya_data(:,:)
      !(in) 入力格子点データ(1:jm,:)

      real(8)              :: la_DivLat_ya(0:nm,size(ya_data,2))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      integer              :: k

      do k=1,size(ya_data,2)
         la_DivLat_ya(:,k) = l_DivLat_y(ya_data(:,k))
      end do

    end function la_DivLat_ya

  !--------------- 微分計算 (μ座標系用) -----------------
    function ya_GradMu_la(la_data)
      !
      ! スペクトルデータに勾配型緯度微分 (1-μ^2)∂/∂μ  (μ=sinφ)
      ! を作用させて格子点データに変換して返す(多層用).. 
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) 入力スペクトルデータ(0:nm,:)

      real(8)              :: ya_GradMu_la(1:jm,size(la_data,2))
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      integer              :: k

      do k=1,size(la_data,2)
         ya_GradMu_la(:,k) = y_GradMu_l(la_data(:,k))
      end do

    end function ya_GradMu_la

    function la_DivMu_ya(ya_data)
      !
      ! 格子点データに発散型緯度微分 ∂/∂μ (μ=sinφ)を作用させて
      ! スペクトルデータに変換して返す(多層用).
      !
      real(8), intent(in)  :: ya_data(:,:)
      !(in) 入力格子点データ(1:jm,:)

      real(8)              :: la_DivMu_ya(0:nm,size(ya_data,2))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      integer              :: k

      do k=1,size(ya_data,2)
         la_DivMu_ya(:,k) = l_DivMu_y(ya_data(:,k))
      end do

    end function la_DivMu_ya

  !--------------- 積分計算 -----------------
    function a_IntLat_ya(ya_data)
      !
      ! 2 次元緯度鉛直格子点データの緯度方向積分.
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  緯度鉛直格子点データ(1:jm,:)

      real(8)             :: a_IntLat_ya(size(ya_data,2))
      !(out) 積分値
      
      integer :: k

      do k=1,size(ya_data,2)
         a_IntLat_ya(k) = sum(ya_data(:,k) * y_Lat_weight)
      end do

    end function a_IntLat_ya

  !--------------- 平均計算 -----------------
    function a_AvrLat_ya(ya_data)
      !
      ! 2 次元緯度鉛直格子点データの緯度(Y)方向平均.
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  緯度鉛直格子点データ(1:jm,:)

      real(8)             :: a_AvrLat_ya(size(ya_data,2))
      !(out) 平均値

      a_AvrLat_ya = a_IntLat_ya(ya_data)/sum(y_Lat_weight)

    end function a_AvrLat_ya

  !--------------- 補間計算 -----------------    
    function a_Interpolate_la(la_data,alat)
      !
      ! 緯度 alat における関数値を
      ! そのルジャンドル変換係数 la_data から補間計算する
      !
      real(8), intent(IN) :: la_data(:,:)
      ! スペクトルデータ(0:nm,:)

      real(8), intent(IN) :: alat
      ! 補間する位置(緯度)

      real(8)             :: a_Interpolate_la(size(la_data,2))
      ! 補間した値
      
      integer               :: k

      do k=1,size(la_data,2)
         a_Interpolate_la(k) = Interpolate_l(la_data(:,k),alat)
      end do

    end function a_Interpolate_la

end module la_module
