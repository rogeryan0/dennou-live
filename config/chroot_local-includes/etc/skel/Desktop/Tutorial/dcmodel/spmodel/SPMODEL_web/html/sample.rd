=begin JA
= プログラム集
=end JA
=begin EN
= SPMODEL programs
=end EN

=begin

# * 2009/01/20 (竹広真一) サブディレクトリ追加
# * 2009/01/12 (竹広真一) サンプルディレクトリ追加
# * 2009/01/12 (竹広真一) spmodel 内リンクを相対パスに変更
# * 2008/01/14 (竹広真一) wu_module, wtu_module のリンク追加
# * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に合わせた修正
# * 2005/12/14 (林祥介)
# * 2005/07/25 (小高正嗣)
# * 2005/07/21 (小高正嗣)
# * 2005/06/30 (小高正嗣)
# * 2005/04/01 (小高正嗣)
# * 2005/04/01 (小高正嗣)
# * 2005/03/30 (小高正嗣)
# * 2004/03/18 (小高正嗣)
# * 2004/01/26 (小高正嗣)
# * 2002/09/12 (竹広真一, 林祥介)
# * 2002/02/15 (竹広真一) 新規作成

=end

=begin JA

=== 1 次元問題

*((<1 次元周期境界領域モデル : フーリエ展開|URL:../1d-cyclic-e/SIGEN.htm>))
  *((<移流拡散方程式モデル|URL:../1d-cyclic-e/advection-diffusion/SIGEN.htm>))
  *((<Korteveg de Vries 方程式モデル|URL:../1d-cyclic-e/kdv/SIGEN.htm>))
*((<動径 1 次元境界領域モデル : チェビシェフ展開|URL:../1d-radial-u/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../1d-radial-u/diffusion/SIGEN.htm>))
*((<動径 1 次元境界領域モデル : Matsushima and Marcus 多項式展開|URL:../1d-radial-q/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../1d-radial-q/diffusion/SIGEN.htm>))

=end
=begin EN

=== 1 dimensional model

*((<Cyclic boundary condition models: Fourier expansion|URL:../1d-cyclic-e/SIGEN.htm>))
  *((<advection diffusion model|URL:../1d-cyclic-e/advection-diffusion/SIGEN.htm>))
  *((<Korteveg de Vries model|URL:../1d-cyclic-e/kdv/SIGEN.htm>))
*((<1-dim radial coordinate models : Chebyshev expansion|URL:../1d-radial-u/SIGEN.htm>))
  *((<diffusion model|URL:../1d-radial-u/diffusion/SIGEN.htm>))
*((<1-dim radial coordinate models : Matsushima and Marcus Polynomial expansion by |URL:../1d-radial-q/SIGEN.htm>))
  *((<diffusion model|URL:../1d-radial-q/diffusion/SIGEN.htm>))

=end 

=begin JA
=== 2 次元問題

*((<2 次元 2 重周期境界モデル : 2 重フーリエ展開|URL:../2d-cyclic-ee/SIGEN.htm>))
  *((<順圧モデル|URL:../2d-cyclic-ee/baro/SIGEN.htm>))
*((<2 次元水路領域モデル : フーリエ展開 + sin cos 展開|URL:../2d-channel-esc/SIGEN.htm>))
  *((<ブシネスク流体モデル|URL:../2d-channel-esc/boussinesq/SIGEN.htm>))
  *((<2 成分拡散ブシネスク流体モデル|URL:../2d-channel-esc/boussinesq-dbldif/SIGEN.htm>))
  *((<赤道β平面浅水モデル|URL:../2d-channel-esc/shallow-equator/SIGEN.htm>))
  *((<赤道β平面多層浅水モデル|URL:../2d-channel-esc/multilayer-equator/SIGEN.htm>))
*((<2 次元水路領域モデル : フーリエ展開 + チェビシェフ展開|URL:../2d-channel-et/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../2d-channel-et/diffusion/SIGEN.htm>))
  *((<ブシネスク流体モデル|URL:../2d-channel-et/boussinesq/SIGEN.htm>))
  *((<赤道β平面浅水モデル|URL:../2d-channel-et/shallow-equator/SIGEN.htm>))
*((<2 次元円盤領域モデル : フーリエ展開 + Matsushima Marcus 多項式展開|URL:../2d-disk-eq/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../2d-disk-eq/diffusion/SIGEN.htm>))
*((<2 次元球面モデル : 球面調和関数展開|URL:../2d-sphere-w/SIGEN.htm>))
  *((<順圧モデル|URL:../2d-sphere-w/baro/SIGEN.htm>))
    *((<さらなる応用例|URL:/library/numexp/>))
  *((<浅水モデル|URL:../2d-sphere-w/shallow/SIGEN.htm>))
  *((<浅水モデル(渦度発散型)|URL:../2d-sphere-w/shallow-zd/SIGEN.htm>))
    *((<さらなる応用例|URL:/library/numexp/>))

=end
=begin EN

=== 2 dimensional model

*((<Cyclic boundary condition models : Double Fourier expansion|URL:../2d-cyclic-ee/SIGEN.htm>))
  *((<barotropic model|URL:../2d-cyclic-ee/baro/SIGEN.htm>))
*((<Channel models : Fourier and sin cos expansion|URL:../2d-channel-esc/SIGEN.htm>))
  *((<Boussinesq model|URL:../2d-channel-esc/boussinesq/SIGEN.htm>))
  *((<double diffusive Boussinesq model|URL:../2d-channel-esc/boussinesq-dbldif/SIGEN.htm>))
  *((<shallow water model on the equatorial-beta plane|URL:../2d-channel-esc/shallow-equator/SIGEN.htm>))
  *((<multilayer shallow water model on the equatorial-beta plane|URL:../2d-channel-esc/multilayer-equator/SIGEN.htm>))
*((<Channel models : Fourier and Chebyshev expansion|URL:../2d-channel-et/SIGEN.htm>))
  *((<diffusion model|URL:../2d-channel-et/diffusion/SIGEN.htm>))
  *((<Boussinesq model|URL:../2d-channel-et/boussinesq/SIGEN.htm>))
  *((<shallow water model on the equatorial-beta plane|URL:../2d-channel-et/shallow-equator/SIGEN.htm>))
*((<Disk models :Fourier and Matsushima Marcus polynomial expansion|URL:../2d-disk-eq/SIGEN.htm>))
  *((<diffusion model|URL:../2d-disk-eq/diffusion/SIGEN.htm>))
*((<Spherical models : Spherical harmonic expansion|URL:../2d-sphere-w/SIGEN.htm>))
  *((<barotropic model|URL:../2d-sphere-w/baro/SIGEN.htm>))
    *((<Further applications|URL:/library/numexp/>))
  *((<shallow water model|URL:../2d-sphere-w/shallow/SIGEN.htm>))
  *((<shallow water model(vorticiy-divergence formulation)|URL:../2d-sphere-w/shallow-zd/SIGEN.htm>))
    *((<Further applications|URL:/library/numexp/>))
=end 


=begin JA
=== 3 次元問題

# *((<3 次元球面モデル : 球面調和函数展開 + 鉛直格子|URL:../3d-sphere-wa/SIGEN.htm>))
*((<3 次元球殻領域モデル : 球面調和関数 + チェビシェフ展開|URL:../3d-shell-wt/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../3d-shell-wt/diffusion/SIGEN.htm>))
  *((<ナビエ-ストークスモデル|URL:../3d-shell-wt/navier-stokes/SIGEN.htm>))
  *((<ブシネスク流体モデル|URL:../3d-shell-wt/boussinesq/SIGEN.htm>))
  *((<ブシネスク磁気流体モデル|URL:../3d-shell-wt/mhd-boussinesq/SIGEN.htm>))
    *((<さらなる応用例|URL:/library/dynamo/>))
*((<3 次元球領域モデル : 球面調和関数 + チェビシェフ展開|URL:../3d-sphere-wu/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../3d-sphere-wu/diffusion/SIGEN.htm>))
*((<3 次元球領域モデル : 球面調和関数 + Matsushima and Marcus 多項式展開|URL:../3d-sphere-wq/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../3d-sphere-wq/diffusion/SIGEN.htm>))
*((<3 次元球および殻領域モデル : 球面調和関数 + チェビシェフ展開|URL:../3d-sphereshell-wtu/SIGEN.htm>))
  *((<拡散方程式モデル|URL:../3d-sphereshell-wtu/diffusion/SIGEN.htm>))

=end
=begin EN

=== 3 dimensional model

# *((<Spherical models : Spherical harmonic expansion with vertical deference|URL:../3d-sphere-wa/SIGEN.htm>))
*((<Spherical shell models : Spherical harmonic and Chebyshev expansion|URL:../3d-shell-wt/SIGEN.htm>))
  *((<duffision model|URL:../3d-shell-wt/diffusion/SIGEN.htm>))
  *((<Navier-Stokes model|URL:../3d-shell-wt/navier-stokes/SIGEN.htm>))
  *((<Boussinesq model|URL:../3d-shell-wt/boussinesq/SIGEN.htm>))
  *((<MHD Boussinesq model|URL:../3d-shell-wt/mhd-boussinesq/SIGEN.htm>))
*((<Sphere models : Spherical harmonic and Chebyshev expansion|URL:../3d-sphere-wu/SIGEN.htm>))
  *((<duffision model|URL:../3d-sphere-wu/diffusion/SIGEN.htm>))
*((<Sphere models : Spherical harmonic and Matsushima Marcus polynomial expansion|URL:../3d-sphere-wq/SIGEN.htm>))
  *((<duffision model|URL:../3d-sphere-wq/diffusion/SIGEN.htm>))
*((<Sphere and spherical shell models : Spherical harmonic and Chebyshev expansion|URL:../3d-sphereshell-wtu/SIGEN.htm>))
  *((<duffision model|URL:../3d-sphereshell-wtu/diffusion/SIGEN.htm>))

=end EN



