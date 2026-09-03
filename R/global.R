#' @import shiny
#' @import GDAtools
#' @import ggplot2
#' @import dplyr
NULL

# Linuxコンテナ環境向けの日本語フォント設定
if (requireNamespace("showtext", quietly = TRUE)) {
  showtext::showtext_auto(TRUE)
}

#' アプリ起動関数
#' @param ... shinyAppへ渡す引数
#' @export
run_app <- function(...) {
  # DESCRIPTIONからバージョン情報の取得
  app_version <- tryCatch({
    desc <- read.dcf("DESCRIPTION")
    desc[1, "Version"]
  }, error = function(e) "0.95")

  # パッケージ内の inst/app/www を /www として登録
  www_path <- system.file("app/www", package = "ShinyspeMCA")
  if (www_path != "") {
    shiny::addResourcePath("www", www_path)
  }

  shinyApp(
    ui = app_ui(app_version = app_version),
    server = app_server,
    ...
  )
}
