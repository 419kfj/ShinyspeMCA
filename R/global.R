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
#' @param ... shinyAppへ渡すその他の引数
#' @export
run_app <- function(df = NULL, ...) {
  # app_version <- tryCatch({
  #   desc <- read.dcf(system.file("DESCRIPTION", package = "ShinyspeMCA"))
  #   desc[1, "Version"]
  # }, error = function(e) "0.95")

  # 変更後（インストール済みパッケージの DESCRIPTION を安全に取得）
  app_version <- tryCatch({
    desc_path <- system.file("DESCRIPTION", package = "ShinyspeMCA")
    if (nchar(desc_path) > 0 && file.exists(desc_path)) {
      read.dcf(desc_path)[1, "Version"]
    } else {
      "3.0.1" # フォールバック値
    }
  }, error = function(e) "3.0.1")


  ui <- app_ui(app_version = app_version)

  # 引数 df を server 関数側で参照できるようにセット
  server <- function(input, output, session) {
    app_server(input, output, session, external_df = df)
  }

  shinyApp(
    ui = ui,
    server = server,
    ...
  )
}
