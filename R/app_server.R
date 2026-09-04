#' @import shiny
app_server <- function(input, output, session, external_df = NULL, external_res = NULL) {

  # --- 1. サイドバーの UI 切り替え ---
  # --- 1. サイドバーの UI 切り替え ---
  output$file_input_ui <- renderUI({
    if (!is.null(external_res) || (!is.null(input$upload_mode) && input$upload_mode == "mode_res")) {
      # 【第二段階モード】計算済み結果 (res) がある場合
      tagList(
        if (!is.null(external_res)) {
          tags$div(
            class = "alert alert-success",
            style = "padding: 10px; background-color: #dff0d8; border-color: #d6e9c6; color: #3c763d; border-radius: 4px;",
            h5(icon("check-circle"), " 計算済みspeMCA結果を自動読み込み中", style = "margin-top: 0; font-weight: bold;"),
            p("speMCA実行ステップはスキップされ、直接各種分析を利用できます。", style = "margin-bottom: 0;")
          )
        } else {
          tagList(
            radioButtons("upload_mode", "開始モード:",
                         choices = c("新規データから実行" = "mode_new",
                                     "計算済み結果(.rds)を読み込み" = "mode_res"),
                         selected = "mode_res"),
            hr(style = "margin: 10px 0;"),
            fileInput("file_df_for_res", "1. 元データファイル (.rda / .csv)",
                      accept = c(".rda", ".RData", ".csv")),
            fileInput("file_res", "2. speMCA結果ファイル (.rds)",
                      accept = c(".rds"))
          )
        },
        tags$div(
          class = "alert alert-info",
          style = "margin-top: 15px;",
          p(icon("info-circle"), " このモードでは第一段階（Active変数選択・Junk指定）はスキップされています。上のタブから「変数空間分析」や「個体空間分析」を直接ご利用ください。")
        )
      )
    } else {
      # 【第一段階モード】新規データから実行する場合（既存パネルを表示）
      tagList(
        if (is.null(external_df)) {
          tagList(
            radioButtons("upload_mode", "開始モード:",
                         choices = c("新規データから実行" = "mode_new",
                                     "計算済み結果(.rds)を読み込み" = "mode_res"),
                         selected = "mode_new"),
            hr(style = "margin: 10px 0;"),
            fileInput("file1", "データファイル (.rda / .RData / .csv) を選択",
                      accept = c(".rda", ".RData", ".csv"))
          )
        } else {
          tags$div(
            class = "alert alert-info",
            style = "padding: 10px; background-color: #d9edf7; border-color: #bce8f1; color: #31708f; border-radius: 4px;",
            h5(icon("info-circle"), " 引数データセットを自動読み込み中", style = "margin-top: 0;"),
            p(sprintf("サイズ: %d 行 × %d 列", nrow(external_df), ncol(external_df)), style = "margin-bottom: 0;")
          )
        },
        hr(),
        wellPanel(
          h4("2. Active変数の選択"),
          uiOutput("variable_selectors")
        ),
        hr(),
        wellPanel(
          h4("3. Junkカテゴリの設定と実行"),
          uiOutput("junk_selector")
        ),
        hr(),
        downloadButton("download_mca", "speMCA結果(explor形式)を保存", class = "btn-info", style = "width:100%")
      )
    }
  })

  # --- 2. データフレームの判定と取得 ---
  # --- 2. データフレーム (df_reactive) の取得 ---
  df_reactive <- reactive({
    if (!is.null(external_df)) {
      return(as.data.frame(external_df))
    }

    upload_mode <- input$upload_mode
    if (is.null(upload_mode) || upload_mode == "mode_new") {
      file_target <- input$file1
    } else {
      file_target <- input$file_df_for_res
    }

    req(file_target)
    req(file_target$datapath)

    ext <- tools::file_ext(file_target$name)

    tryCatch({
      if (ext %in% c("rda", "RData")) {
        env <- new.env()
        load(file_target$datapath, envir = env)
        obj_names <- ls(env)
        res_df <- env[[obj_names[1]]]
        return(as.data.frame(res_df))
      } else if (ext == "csv") {
        return(read.csv(file_target$datapath, stringsAsFactors = TRUE))
      } else {
        validate("サポートされていないファイル形式です (.rda, .RData, .csv)")
      }
    }, error = function(e) {
      showNotification(paste0("ファイル読み込みエラー: ", e$message), type = "error")
      return(NULL)
    })
  })
  # --- 3. データプレビュー表示例 ---
  output$data_preview <- renderTable({
    df <- datasetInput()
    head(df)
  })

  # ※ 以降の speMCA 分析・グラフ描画処理等は datasetInput() を参照して実行してください。

  # Active変数の選択 ----
  output$variable_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    selectInput("variables", "Active変数を選んでください",
                choices = choices, multiple = TRUE, selectize = FALSE, size = 7)
  })

  # 追加変数の選択 ----
  output$supvar_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    selectInput("supvars", "追加変数を選んでください",
                choices = choices, multiple = TRUE, selectize = FALSE, size = 7)
  })

  # 交互作用変数の選択 ----
  output$interaction_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    tagList(
      selectInput("inter_v1", "交互作用 v1 を選んでください",
                  choices = choices, selected = choices[1]),
      uiOutput("inter_v1_selector"),
      selectInput("inter_v2", "交互作用 v2 を選んでください",
                  choices = choices, selected = choices[min(2, length(choices))]),
      uiOutput("inter_v2_selector")
    )
  })

  ### 交互作用変数 v1 の「全選択/全解除」----
  observeEvent(input$select_v1_all, {
    req(input$inter_v1)
    df <- df_reactive()
    v1lv <- levels(as.factor(df[[input$inter_v1]]))
    updateCheckboxGroupInput(session, "selected_categories_v1", selected = v1lv)
  })
  observeEvent(input$deselect_v1_all, {
    updateCheckboxGroupInput(session, "selected_categories_v1", selected = character(0))
  })

  ### 交互作用変数 v2 の「全選択/全解除」----
  observeEvent(input$select_v2_all, {
    req(input$inter_v2)
    df <- df_reactive()
    v2lv <- levels(as.factor(df[[input$inter_v2]]))
    updateCheckboxGroupInput(session, "selected_categories_v2", selected = v2lv)
  })
  observeEvent(input$deselect_v2_all, {
    updateCheckboxGroupInput(session, "selected_categories_v2", selected = character(0))
  })

  # 集中楕円変数の選択 ----
  output$ellipse_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    tagList(
      selectInput("var_ellipses", "集中楕円表示変数を選んでください",
                  choices = choices, multiple = FALSE),
      uiOutput("kellipses_cat_selector")
    )
  })

  ## 集中楕円変数の「全選択/全解除」----
  observeEvent(input$select_all, {
    req(input$var_ellipses)
    df <- df_reactive()
    lv <- levels(as.factor(df[[input$var_ellipses]]))
    updateCheckboxGroupInput(session, "selected_categories", selected = lv)
  })

  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_categories", selected = character(0))
  })

  # junkカテゴリの選択 ----
  junk_cat <- reactive({
    req(df_reactive())
    if (is.null(input$variables) || length(input$variables) < 2) return(NULL)
    df <- df_reactive()
    df_sub <- df[, input$variables, drop = FALSE]
    jc <- GDAtools::getindexcat(df_sub)
    if (is.null(jc) || length(jc) == 0) return(NULL)
    jc
  })

  ### junk selector ----
  output$junk_selector <- renderUI({
    jc <- junk_cat()
    if (is.null(jc)) return(NULL)
    tagList(
      selectInput("excluded_cats", "junk指定するカテゴリを選択してください",
                  choices = jc, multiple = TRUE, selectize = FALSE,
                  size = min(10, length(jc))),
      actionButton("run_mca", "speMCAを実行する")
    )
  })

  # speMCA 実行 ----
  # --- speMCA 結果 (mca_result) の取得 / 実行 ---
  mca_result <- reactive({
    if (!is.null(external_res)) {
      return(external_res)
    }

    if (is.null(external_df) && !is.null(input$upload_mode) && input$upload_mode == "mode_res") {
      req(input$file_res)
      ext <- tools::file_ext(input$file_res$name)
      if (ext == "rds") {
        return(readRDS(input$file_res$datapath))
      }
    }

    req(input$run_mca)
    df <- df_reactive()
    req(df, input$variables)

    if (length(input$variables) < 2) {
      showNotification("Active変数は少なくとも2つ選んでください。", type = "warning")
      return(NULL)
    }

    df_sub <- df[, input$variables, drop = FALSE]
    excl_indices <- NULL
    if (!is.null(input$excluded_cats) && length(input$excluded_cats) > 0) {
      jc <- junk_cat()
      excl_indices <- match(input$excluded_cats, jc)
      excl_indices <- excl_indices[!is.na(excl_indices)]
    }

    tryCatch({
      GDAtools::speMCA(df_sub, excl = excl_indices)
    }, error = function(e) {
      showNotification(paste0("speMCA エラー: ", e$message), type = "error")
      NULL
    })
  })
  # speMCAのresultを出力 ----
  output$mca_result_list <- renderPrint({
    req(mca_result())
    print(mca_result())
  })

  # supvars の計算 ----
  supvars_result <- reactive({
    req(mca_result())
    if (is.null(input$supvars) || length(input$supvars) == 0) return(NULL)
    df <- df_reactive()
    tryCatch({
      GDAtools::supvars(resmca = mca_result(), vars = df %>% dplyr::select(dplyr::all_of(input$supvars)))
    }, error = function(e) {
      message("supvars エラー: ", e$message)
      NULL
    })
  })

  # 交互作用変数 v1 のセレクタ生成 ----
  output$inter_v1_selector <- renderUI({
    req(df_reactive(), input$inter_v1)
    df <- df_reactive()
    v1lv <- levels(as.factor(df[[input$inter_v1]]))

    tagList(
      fluidRow(
        column(6, actionButton("select_v1_all", "全選択")),
        column(6, actionButton("deselect_v1_all", "全解除"))
      ),
      checkboxGroupInput(
        inputId = "selected_categories_v1",
        label = "表示するカテゴリを選んでください",
        choices = v1lv,
        selected = v1lv
      )
    )
  })

  # 交互作用変数 v2 のセレクタ生成 ----
  output$inter_v2_selector <- renderUI({
    req(df_reactive(), input$inter_v1)
    df <- df_reactive()
    v2lv <- levels(as.factor(df[[input$inter_v2]]))

    tagList(
      fluidRow(
        column(6, actionButton("select_v2_all", "全選択")),
        column(6, actionButton("deselect_v2_all", "全解除"))
      ),
      checkboxGroupInput(
        inputId = "selected_categories_v2",
        label = "表示するカテゴリを選んでください",
        choices = v2lv,
        selected = v2lv
      )
    )
  })

  # 集中楕円変数のカテゴリセレクタ ----
  output$kellipses_cat_selector <- renderUI({
    req(df_reactive())
    req(input$var_ellipses)
    df <- df_reactive()
    var <- df[[input$var_ellipses]]
    lv <- levels(as.factor(var))

    tagList(
      fluidRow(
        column(6, actionButton("select_all", "全選択")),
        column(6, actionButton("deselect_all", "全解除"))
      ),
      checkboxGroupInput(
        inputId = "selected_categories",
        label = "表示するカテゴリを選んでください",
        choices = lv,
        selected = lv
      )
    )
  })

  # 選択変数タブ ----
  output$selected_info <- renderUI({
    req(df_reactive())
    vars <- input$variables
    junk <- input$excluded_cats
    kellipse_cat <- input$selected_categories

    tagList(
      h4("Active 変数"),
      if (!is.null(vars) && length(vars) > 0) {
        HTML(paste0("<ul>", paste0("<li>", vars, "</li>", collapse = ""), "</ul>"))
      } else {
        em("なし")
      },
      h4("Junk カテゴリ"),
      if (!is.null(junk) && length(junk) > 0) {
        HTML(paste0("<ul>", paste0("<li>", junk, "</li>", collapse = ""), "</ul>"))
      } else {
        em("なし")
      },
      h4("集中楕円描画カテゴリ"),
      if (!is.null(kellipse_cat) && length(kellipse_cat) > 0) {
        HTML(paste0("<ul>", paste0("<li>", kellipse_cat, "</li>", collapse = ""), "</ul>"))
      } else {
        em("なし")
      }
    )
  })

  # res.speMCAのリスト表示 ----
  output$mca_result_tree <- listviewer::renderJsonedit({
    req(mca_result())
    listviewer::jsonedit(
      mca_result(),
      mode = "view",
      modes = c("view", "code")
    )
  })

  # 結果表示：データ表 / 修正慣性率 ----
  output$data_table <- DT::renderDT({
    req(df_reactive())
    DT::datatable(df_reactive(), options = list(pageLength = 10))
  })

  output$eig_table <- renderTable({
    res <- mca_result(); req(res)
    data.frame(
      軸 = seq_along(res$eig$mrate),
      修正慣性率 = res$eig$mrate,
      累積慣性率 = res$eig$cum.mrate
    )
  })

  output$eig_plot <- renderPlot({
    res <- mca_result(); req(res)
    df_eig <- data.frame(
      dim = seq_along(res$eig$mrate),
      mrate = res$eig$mrate,
      cum_mrate = res$eig$cum.mrate
    )
    ggplot(df_eig, aes(x = dim)) +
      geom_col(aes(y = mrate)) +
      geom_line(aes(y = cum_mrate), color = "red", size = 1) +
      geom_point(aes(y = cum_mrate), color = "red") +
      labs(x = "次元", y = "修正慣性率", title = "修正慣性率と累積慣性率") +
      theme_minimal()
  })

  # η2マップ ----
  get_eta2_coord <- function(mca_data, df_data, supvar_names, axes) {
    coord_eta2_sup0 <- GDAtools::dimeta2(
      resmca = mca_data,
      vars = df_data %>% dplyr::select(dplyr::all_of(supvar_names)),
      dim = c(1:5)
    )
    coord_eta2_sup1 <- coord_eta2_sup0[, axes]
    colnames(coord_eta2_sup1) <- c("x", "y")
    coord_eta2_sup1 %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        vnames = supvar_names,
        x = x / 100,
        y = y / 100
      ) %>%
      dplyr::select(3, 1, 2)
  }

  draw_eta2_map <- function(mca_data, axes, df_data, supvar_names, title) {
    p <- GDAtools::ggeta2_variables(resmca = mca_data, axes = axes) +
      theme(aspect.ratio = 1)
    if (!is.null(supvar_names)) {
      coord_eta2_sup <- get_eta2_coord(mca_data, df_data, supvar_names, axes)
      p <- p +
        geom_point(data = coord_eta2_sup, aes(x = x, y = y), size = 2, color = "blue") +
        ggrepel::geom_text_repel(data = coord_eta2_sup, aes(x = x, y = y, label = vnames), size = 3, vjust = -1, box.padding = 0.5)
    }
    p
  }

  output$eta2_map_12 <- renderPlot({
    res <- mca_result()
    df_data <- df_reactive()
    req(res)
    supvar_names <- input$supvars
    draw_eta2_map(res, axes = c(1, 2), df_data, supvar_names, "η2 1-2軸")
  })

  # 変数マップ ----
  draw_vari_plot <- function(mca_data, axes, title) {
    GDAtools::ggcloud_variables(mca_data, axes = axes) +
      theme(aspect.ratio = 1) +
      ggtitle(title)
  }

  output$var_map_12 <- renderPlot({
    res <- mca_result(); req(res)
    draw_vari_plot(res, c(1, 2), "変数マップ 1-2軸")
  })

  output$var_map_32 <- renderPlot({
    res <- mca_result(); req(res)
    draw_vari_plot(res, c(3, 2), "変数マップ 3-2軸")
  })

  output$var_map_13 <- renderPlot({
    res <- mca_result(); req(res)
    draw_vari_plot(res, c(1, 3), "変数マップ 1-3軸")
  })

  # 個体マップ ----
  draw_ind_plot <- function(mca_data, axes, title) {
    p <- GDAtools::ggcloud_indiv(mca_data, axes = axes) +
      theme(aspect.ratio = 1) +
      ggtitle(title)

    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(yaxis = list(scaleanchor = "x", scaleratio = 1))
  }

  output$ind_map_12 <- plotly::renderPlotly({
    res <- mca_result(); req(res)
    draw_ind_plot(res, c(1, 2), "個体マップ 1-2軸")
  })

  output$ind_map_32 <- plotly::renderPlotly({
    res <- mca_result(); req(res)
    draw_ind_plot(res, c(3, 2), "個体マップ 3-2軸")
  })

  output$ind_map_13 <- plotly::renderPlotly({
    res <- mca_result(); req(res)
    draw_ind_plot(res, c(1, 3), "個体マップ 1-3軸")
  })

  # supvars 出力とマップ ----
  output$supvars_out <- renderPrint({
    res <- supvars_result()
    if (is.null(res)) "supvarsの結果がありません" else print(res)
  })

  output$supvars_map_12 <- renderPlot({
    req(mca_result(), input$supvars)
    res <- mca_result()
    df <- df_reactive()
    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, axes = c(1, 2), col = "lightgrey")
      GDAtools::ggadd_supvars(p = base_map, resmca = res, axes = c(1, 2), vlab = FALSE, vars = df %>% dplyr::select(dplyr::all_of(input$supvars))) +
        theme(aspect.ratio = 1)
    }, error = function(e) {
      message("ggadd_supvars エラー: ", e$message)
      NULL
    })
  })

  output$supvars_map_32 <- renderPlot({
    req(mca_result(), input$supvars)
    res <- mca_result()
    df <- df_reactive()
    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, axes = c(3, 2), col = "lightgrey")
      GDAtools::ggadd_supvars(p = base_map, resmca = res, axes = c(3, 2), vlab = FALSE, vars = df %>% dplyr::select(dplyr::all_of(input$supvars))) +
        theme(aspect.ratio = 1)
    }, error = function(e) {
      message("ggadd_supvars エラー: ", e$message)
      NULL
    })
  })

  output$supvars_map_13 <- renderPlot({
    req(mca_result(), input$supvars)
    res <- mca_result()
    df <- df_reactive()
    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, axes = c(1, 3), col = "lightgrey")
      GDAtools::ggadd_supvars(p = base_map, resmca = res, axes = c(1, 3), vlab = FALSE, vars = df %>% dplyr::select(dplyr::all_of(input$supvars))) +
        theme(aspect.ratio = 1)
    }, error = function(e) {
      message("ggadd_supvars エラー: ", e$message)
      NULL
    })
  })

  # 交互作用プロット ----
  output$interaction_map_12 <- renderPlot({
    req(mca_result(), input$inter_v1, input$inter_v2, input$selected_categories_v1, input$selected_categories_v2)
    res <- mca_result()
    df <- df_reactive()
    axes <- c(1, 2)

    v1_f <- df[[input$inter_v1]]
    v1_f[!(v1_f %in% input$selected_categories_v1)] <- NA
    v2_f <- df[[input$inter_v2]]
    v2_f[!(v2_f %in% input$selected_categories_v2)] <- NA

    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, col = "lightgrey", axes = axes)
      GDAtools::ggadd_interaction(p = base_map, resmca = res, v1 = v1_f, v2 = v2_f, axes = axes) +
        theme(aspect.ratio = 1)
    }, error = function(e) {
      message("ggadd_interaction エラー: ", e$message)
      NULL
    })
  })

  # 集中楕円 ----
  output$kellipses_map_12 <- plotly::renderPlotly({
    req(mca_result(), input$var_ellipses, input$selected_categories)
    res <- mca_result()
    df <- df_reactive()
    axes_v <- c(1, 2)

    var_factor <- as.factor(df[[input$var_ellipses]])
    levels_var <- levels(var_factor)
    sel_index <- which(levels_var %in% input$selected_categories)

    tryCatch({
      base_map_ind <- GDAtools::ggcloud_indiv(res, axes = axes_v, col = "lightgrey")
      (GDAtools::ggadd_kellipses(p = base_map_ind, resmca = res, axes = axes_v, var = var_factor, sel = sel_index) +
          coord_fixed(ratio = 1)) %>% plotly::ggplotly(tooltip = "all")
    }, error = function(e) {
      message("ggadd_kellipses エラー: ", e$message)
      NULL
    })
  })

  output$kellipses_map_32 <- plotly::renderPlotly({
    req(mca_result(), input$var_ellipses, input$selected_categories)
    res <- mca_result()
    df <- df_reactive()
    axes_v <- c(3, 2)

    var_factor <- as.factor(df[[input$var_ellipses]])
    levels_var <- levels(var_factor)
    sel_index <- which(levels_var %in% input$selected_categories)

    tryCatch({
      base_map_ind <- GDAtools::ggcloud_indiv(res, axes = axes_v, col = "lightgrey")
      (GDAtools::ggadd_kellipses(p = base_map_ind, resmca = res, axes = axes_v, var = var_factor, sel = sel_index) +
          coord_fixed(ratio = 1)) %>% plotly::ggplotly(tooltip = "all")
    }, error = function(e) {
      message("ggadd_kellipses エラー: ", e$message)
      NULL
    })
  })

  output$kellipses_map_13 <- plotly::renderPlotly({
    req(mca_result(), input$var_ellipses, input$selected_categories)
    res <- mca_result()
    df <- df_reactive()
    axes_v <- c(1, 3)

    var_factor <- as.factor(df[[input$var_ellipses]])
    levels_var <- levels(var_factor)
    sel_index <- which(levels_var %in% input$selected_categories)

    tryCatch({
      base_map_ind <- GDAtools::ggcloud_indiv(res, axes = axes_v, col = "lightgrey")
      (GDAtools::ggadd_kellipses(p = base_map_ind, resmca = res, axes = axes_v, var = var_factor, sel = sel_index) +
          coord_fixed(ratio = 1)) %>% plotly::ggplotly(tooltip = "all")
    }, error = function(e) {
      message("ggadd_kellipses エラー: ", e$message)
      NULL
    })
  })

  # ENQview 単変数集計関連 ----
  output$hist_var_ui <- renderUI({
    req(df_reactive())
    selectInput("select_input_data_for_hist", "確認したい単変数", choices = names(df_reactive()))
  })

  output$barchart2 <- renderPlot({
    req(df_reactive(), input$select_input_data_for_hist)
    df_reactive() %>%
      dplyr::count(!!!rlang::syms(input$select_input_data_for_hist)) %>%
      dplyr::rename(V1 = 1) %>%
      dplyr::filter(V1 != "非該当") %>%
      dplyr::mutate(rate = 100 * .data[["n"]] / sum(.data[["n"]])) %>%
      ggplot2::ggplot(aes(x = V1, y = rate)) +
      ggplot2::geom_col(aes(fill = V1)) +
      ggplot2::ggtitle(input$select_input_data_for_hist)
  })

  output$simple_table2 <- DT::renderDataTable({
    req(df_reactive(), input$select_input_data_for_hist)
    tmp <- table(df_reactive()[[input$select_input_data_for_hist]])
    tmp2 <- round(100 * prop.table(tmp), 1)
    data.frame(tmp, rate = tmp2)[, c(1, 2, 4)]
  })

  # ダウンロードハンドラ ----
  output$download_mca <- downloadHandler(
    filename = function() {
      paste0("speMCA_result_", Sys.Date(), ".rds")
    },
    content = function(file) {
      res <- mca_result()
      if (is.null(res)) {
        showNotification("MCA結果がありません。", type = "error")
        return(NULL)
      }
      saveRDS(res, file)
    }
  )
}
