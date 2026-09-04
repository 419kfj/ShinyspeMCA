#' アプリ UI 定義
#' @param app_version アプリのバージョン文字列
#' @export
app_ui <- function(app_version = "0.95") {
  navbarPage(
    "speMCA分析サポート",
    tabPanel(
      "概要",
      h2("多重対応分析MCA分析サポート", paste("Version:", app_version)),
      # パッケージ名が ShinyspeMCA の場合
#      includeMarkdown(system.file("www/overview.md", package = "ShinyspeMCA")),
      HTML(markdown::mark_html(system.file("www/overview.md", package = "ShinyspeMCA"))),
      hr(),
      h3("Powered by & Supported by"),
      div(
        style = "display: flex; gap: 20px; align-items: center; flex-wrap: wrap; margin-bottom: 20px;",
        img(src = "ShinyspeMCA/GDAtools.png", width = "100px"),
        img(src = "ShinyspeMCA/shiny.webp", width = "100px"),
        img(src = "ShinyspeMCA/ggplot2.png", width = "100px"),
        img(src = "ShinyspeMCA/dplyr.png", width = "100px"),
        div(style = "font-weight: bold; font-size: 24px; color: #4285F4; font-family: sans-serif;", "Gemini AI")
      ),
      h3("Developed by/with"),
      img(src = "ShinyspeMCA/rstudio-logo-png_seeklogo-349849.png", width = "100px")
    ),

    tabPanel(
      "File読み込み-MCA実行",
      sidebarLayout(
        sidebarPanel(
          uiOutput("file_input_ui")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("選択情報", uiOutput("selected_info")),
            tabPanel("慣性率", tableOutput("eig_table")),
            tabPanel("結果リスト", listviewer::jsoneditOutput("mca_result_tree")),
            tabPanel(
              "データ確認",
              uiOutput("check_var_selector"),
              plotOutput("barchart2"),
              DT::DTOutput("data_table")
            )
          )
        )
      )
    ),

    tabPanel(
      "変数空間分析",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            h4("追加変数の設定"),
            uiOutput("supvar_selectors")
          ),
          hr(),
          wellPanel(
            h4("交互作用の設定"),
            uiOutput("interaction_selectors")
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "変数マップ",
              plotOutput("var_map_12", height = "600px"),
              plotOutput("var_map_32", height = "600px"),
              plotOutput("var_map_13", height = "600px")
            ),
            tabPanel(
              "追加変数 (マップ)",
              plotOutput("supvars_map_12", height = "600px"),
              plotOutput("supvars_map_32", height = "600px"),
              plotOutput("supvars_map_13", height = "600px")
            ),
            tabPanel(
              "追加変数 (統計量)",
              h4("追加変数の計算結果 (res$supv)"),
              verbatimTextOutput("supvars_out")
            ),
            tabPanel(
              "η2マップ",
              plotOutput("eta2_map_12", height = "600px")
            ),
            tabPanel(
              "交互作用plot",
              plotOutput("interaction_map_12", height = "600px")
            )
          )
        )
      )
    ),

    tabPanel(
      "個体空間分析",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            h4("楕円描画の設定"),
            uiOutput("ellipse_selectors")
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "個体マップ",
              plotly::plotlyOutput("ind_map_12", height = "600px"),
              plotly::plotlyOutput("ind_map_32", height = "600px"),
              plotly::plotlyOutput("ind_map_13", height = "600px")
            ),
            tabPanel(
              "集中楕円",
              plotly::plotlyOutput("kellipses_map_12", height = "600px"),
              plotly::plotlyOutput("kellipses_map_32", height = "600px"),
              plotly::plotlyOutput("kellipses_map_13", height = "600px")
            )
          )
        )
      )
    ),

    navbarMenu(
      "関連リンク集",
      tabPanel("使い方", a("Shinyアプリの使い方ガイド", href = "https://www.fujimotolabo.uk/Shiny_app_how2/", target = "_blank")),
      tabPanel("MCA関連資料", a("MCA関連資料アーカイブ", href = "https://www.fujimotolabo.uk/CAMCA_archive/", target = "_blank")),
      tabPanel(
        "ソースコード/GitHub",
        a(href = "https://github.com/419kfj/Shiny_speMCA_lite", "GitHub Repository"),
        br(),
        img(src = "github-logo.png", width = "100px")
      )
    )
  )
}
