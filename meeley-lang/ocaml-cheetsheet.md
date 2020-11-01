# OCaml チートシート

```sh
# ライブラリのプロジェクトの初期化 (種類、名前、パス)
dune init library meeley_syntax ./meeley_syntax

# 実行形式のプロジェクトの初期化 (種類、名前、パス)
dune init executable meeley ./meeley_lang

# プロジェクトのビルド
dune build

# 実行 (ビルドしなくても実行される)
dune exec ./meeley.exe
```

## 開発環境の構築

- [OCaml の環境構築 - Qiita](https://qiita.com/zenwerk/items/7bc6177adcbeb6990e60)
- [2019年度「プログラミング言語」配布資料 (6)](https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/pl/setup.html)
- [IoPLMaterials | Materials for the class “Implementation of Programming Languages” in Kyoto University.](https://kuis-isle3sw.github.io/IoPLMaterials/textbook/setting-up-ocaml.html)
- [install | Real World OCaml](https://dev.realworldocaml.org/install.html)
- [OCaml Platform - Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)

## 参考

- [Quickstart — dune documentation](https://dune.readthedocs.io/en/stable/quick-start.html)
