個人的なメモ

# Roxygen2

## 特殊文字

| レイヤー | 特殊文字                  |
|----------|---------------------------|
| roxygen  | `#'`, `@`                 |
| Rd       | `\`, `{}`, `%`            |
| Markdown | `*`, `_`, `` ` ``, `[]()` |

[]は使うときに気をつけた方がいいかも。

## Tags

よく使うタグ

| タグ名   | 説明                                        |
|----------|---------------------------------------------|
| \@export | NAMESPACE に export(foo) を書くための指示　 |
| \@param  | 引数                                        |
| \@return | 戻り値                                      |

\@usage \@note \@seealso \@examples \@references

------------------------------------------------------------------------

# 命名規則とか

-   変数名は基本的にスネークケースで
    -   ドット(.) はS3クラス関係の特別な意味を持つので、基本NG
-   自分ルールだけど、機能別にプレフィックスをつけることにする
    -   p - plot
    -   v - vector
    -   ...
-   定数はどう扱うのがいいんだろ？

------------------------------------------------------------------------

# 0. 事前準備

R と RStudio をインストールした上で、以下のパッケージ(とそれらが依存しているもの)を入れていく。

-   **usethis**
    -   プロジェクトの設定を変更するためのコマンド群を提供
    -   具体的には、DESCPRIPTIONの修正、 gitの設定、ユニットテスト用のフォルダやファイルの準備などをしてくれる
-   **devtools**
    -   作業自動化ツール的な機能を提供
    -   RStudio内のビルドは、実際にはこのモジュールの機能を叩いているらしい
-   **roxygen2**
    -   R Package 作成で必須となる NAMESPACE と man/\*Rd を、コメントから自動生成するツール
    -   devtools::document() 経由で呼び出される
-   **testthat**
    -   Unit test 用のツール
    -   devtools::test() 経由で呼び出される
-   **knitr/rmarkdown**
    -   必須なのかよく分かっていないが、devtools::document() でエラーが出るようなら入れる
    -   Texツール (tinytexとか) も必要かも

注意点としては、**RStudioのPackage管理機能からインストールとすると最新版が入らないことがある**。最新版を要求される場合は、手動でCRANから tar.gz をダウンロードし、ダウンロードしたファイルをコマンドでインストールする。

``` r
install.packages("package_[version].tar.gz")
```

# 1. プロジェクトの作成

パッケージを作成する際には、正しい場所に必要なファイルが置かれている必要がある。

手動でもできない訳ではないが、基本的には usethis を使うのが一般的らしい。

``` r
# プロジェクトの作成
# devtools::create()を使っているサンプルもよく見かけるが、現代的にはこっちらしい
usethis::create_package("~/path/to/mypkg")

# - - - - - - - - - - - - - - - - - - - - - - - - 
# RStudio を使っていれば自動でプロジェクトが開く
# - - - - - - - - - - - - - - - - - - - - - - - - 

# 1) 以前パッケージとライセンス情報の設定 (DESCRIPTIONに設定が追加される)
usethis::use_package("dplyr")              # Imports に追加
usethis::use_package("ggplot2", "Suggests")
usethis::use_mit_license("Your Name")

# 2) Git の設定
usethis::use_git()  # github使うなら usethis::use_github()

# 3) roxygen2 の設定
usethis::use_roxygen_md()

# 4) testthat の設定
usethis::use_testthat()

# - - - - - - - - - - - - - - - - - - - - - - - - 

# vignette の設定 (optional)
usethis::use_vignette("introduction")
```

プロジェクト作成に関しては、RStudioを使っているなら、新規プロジェクト作成で R Package を選択すれば、同じことをしてくれる。

作成されたプロジェクトは、こんな感じになっているはず。

-   DESCRIPTION
    -   パッケージの情報
    -   ただのテキストファイルに見えるが、タグや形式はかなり厳密に決まっているので注意
-   NAMESPACE
    -   Export する関数の一覧
    -   roxygen2 が自動的に作成してくれる
-   R (フォルダ)
    -   パッケージのソースコードは全てこのフォルダに置く必要がある

# 2. コードを書く

パッケージに含む関数を実装する。ソースコードのファイルは、作成された 'R' フォルダ内に置かれていなければならない。

Roxygen2 形式のコメントを書く (最低でも、Exportする関数には全てコメントを書く必要がある)。
