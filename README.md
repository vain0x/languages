# Altery Language

*WIP*: 開発途中

アルテリ言語は、自作の動的言語とインタプリタです。

## 構造

- projects
    - al_aux: 共通コード
    - al_compiler: コンパイラ
    - al_runtime: ランタイム
    - al_test: テストランナー
- tests:
    - \*/\*.altery: テスト用に書かれたアルテリ言語のコード
    - \*/\*.altery_il: 自動生成された中間言語のコード

## 開発: テスト

`./test` を実行して `Congratulations!` が表示されて、さらに自動生成された中間言語コードに問題がなければOK。
