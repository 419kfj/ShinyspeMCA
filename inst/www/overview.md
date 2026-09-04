- このversionは、RのPackageとして開発されているために、Rmdないでdfを処理しているなかで呼び出すことが可能になりました。

- Packageの取得、インストールは、以下のようにしてください。

```
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
devtools::install_github("419kfj/ShinyspeMCA", upgrade="never")
```

- 使うときは、
```
library(ShinyspeMCA)

ShinyspeMCA::run_app(df = <使用したいdfのオブジェクト名>)
ShinyspeMCA::run_app() # これは、Web版と同じrdaファイルのuploadで始めます。
```

- speMCAのリザルトを統合オブジェクトとしてダウンロードすることができます（Rdsファイル）。
   + そのリザルトを、Rmdなどでresultの分析をおこなったり、explorパッケージ等で別途分析することも可能です。
   + また、そのリザルトともとのデータフレームを指定して起動すれば、Active変数、junkカテゴリの設定など、MCAの最初のステップを省略して、分析をすすめることが可能です。

