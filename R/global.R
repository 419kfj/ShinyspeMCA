#' @import shiny
#' @import GDAtools
#' @import ggplot2
#' @import dplyr
NULL

# パッケージロード時に inst/app/www を /ShinyspeMCA として登録
.onLoad <- function(libname, pkgname) {
  www_path <- system.file("app/www", package = pkgname)
  if (www_path != "") {
    shiny::addResourcePath("ShinyspeMCA", www_path)
  }
}

# Linuxコンテナ環境向けの日本語フォント設定
if (requireNamespace("showtext", quietly = TRUE)) {
  showtext::showtext_auto(TRUE)
}

#' アプリ起動関数
#' @param df オプション。解析対象のデータフレーム（未指定の場合はファイルアップロードUIを使用）
#' @param res オプション。計算済みの speMCA 結果オブジェクト（GDAtools::speMCA の出力）
#' @param ... shinyAppへ渡すその他の引数
#' @export
run_app <- function(df = NULL, res = NULL, ...) {
  app_version <- tryCatch({
    desc_path <- system.file("DESCRIPTION", package = "ShinyspeMCA")
    if (nchar(desc_path) > 0 && file.exists(desc_path)) {
      read.dcf(desc_path)[1, "Version"]
    } else {
      "3.0.1"
    }
  }, error = function(e) "3.0.1")

  ui <- app_ui(app_version = app_version)

  # 引数 df および res を server 関数側へ渡す
  server <- function(input, output, session) {
    app_server(input, output, session, external_df = df, external_res = res)
  }

  shinyApp(
    ui = ui,
    server = server,
    ...
  )
}
