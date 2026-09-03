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
#' @param ... shinyAppへ渡す引数
#' @export
run_app <- function(...) {
  app_version <- tryCatch({
    desc <- read.dcf(system.file("DESCRIPTION", package = "ShinyspeMCA"))
    desc[1, "Version"]
  }, error = function(e) "0.95")

  shinyApp(
    ui = app_ui(app_version = app_version),
    server = app_server,
    ...
  )
}
