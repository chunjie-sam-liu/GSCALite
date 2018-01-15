# source by "server.R"
source(file.path(config$wd, "functions", "welcome_function.R"))

# Clear input -------------------------------------------------------------
observeEvent(input$input_gene_set_reset, {
  shinyjs::reset("input_gene_set")
  status$gene_set <- FALSE
})

observeEvent(input$example, {
  status$analysis <- FALSE
  status$gene_set <- FALSE
  shinyjs::js$example_gene_set(id = "seinput_gene_set")
  shinyjs::enable(id = "input_gene_set")
  shinyjs::enable(id = "analysis")
})

observeEvent(input$analysis, {
  # shinyjs::js$checkall()
  shinyjs::disable(id = "input_gene_set")
  shinyjs::disable(id = "analysis")
  # status$analysis <- TRUE
})

observeEvent(input$stop, {
  status$analysis <- FALSE
  status$gene_set <- FALSE
  shinyjs::reset("input_gene_set")
  shinyjs::enable(id = "input_gene_set")
  shinyjs::enable(id = "analysis")
  output$ui_progressbar <- renderUI({
    NULL
  })
})

observeEvent(status$trigger, {
  if (error$gene_set != "" && !is.null(error$gene_set)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = error$gene_set,
      type = "error"
    )
  }
})

observeEvent(input$ctps_switch, {
  if (input$ctps_switch == TRUE) {
    shinyWidgets:::updateMultiInput(session = session, inputId = "select_ctps", selected = ctps)
    shinyjs::js$switch("select_ctps")
  } else {
    shinyWidgets:::updateMultiInput(session = session, inputId = "select_ctps", selected = character(0))
  }
  
})

observeEvent(input$select_analysis, {})


# welcome message ---------------------------------------------------------
output$ui_welcom_msg <- renderUI({fn_welcom_msg()})

# Search box and examples -------------------------------------------------

output$ui_search_example <- renderUI({fn_search_example()})

# Monitor search ----------------------------------------------------------

validate_input_gene_set <- eventReactive(
  eventExpr = input$input_gene_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$gene_set <- TRUE

    if (is.null(input$input_gene_set) || input$input_gene_set == "") {
      error$gene_set <- "Error: Input at least One gene symbol."
      status$trigger <- if (status$trigger == TRUE) FALSE else TRUE
      status$gene_set <- FALSE
      return()
    }

    # check gene
    .v_igs <- check_gene_set(.s = input$input_gene_set, status = status, error = error)

    # validate genes
    validate_gene_set(.v = .v_igs, user_dir = user_dir, user_logs = user_logs, total_gene_symbol = total_gene_symbol, status = status, error = error, gene_set = gene_set)
  }
)


# Statistics of input gene list -------------------------------------------
output$ui_gene_set_stat <- renderUI({if (status$gene_set) {fn_gene_set_stat(gene_set)} else {NULL}})

# Download gene set -------------------------------------------------------
output$download_total_gene_set <- fn_gs_download(user_dir, user_id, user_logs, txt = "total_gene_set.txt", s = 3)
output$download_valid_gene_set <- fn_gs_download(user_dir, user_id, user_logs, txt = "valid_gene_set.txt", s = 4)
output$download_input_logs <- fn_gs_download(user_dir, user_id, user_logs, txt = "input_gene_set_log.txt", s = 0)

# cancer types selection --------------------------------------------------
output$ui_multi_cancer_input <- renderUI({if (status$gene_set) {fn_multi_cancer_input(.ctps = ctps)} else {NULL}})

# Start analysis ----------------------------------------------------------
output$ui_start_analysis <- renderUI({if (status$gene_set) {fn_start_analysis()} else {NULL}})


# progress bar ui -----------------------------------------------------

progressbar_start_analysis <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    output$ui_progressbar <- renderUI({
      shiny::tagList(
        column(
          style = "margin-top:30px;",
          width = 8, offset = 2,
          shinyWidgets::progressBar(id = "progressbar", value = 10, striped = TRUE, status = "primary")
        )
      )
    })
    session$onFlushed(function() {
      status$analysis <- TRUE
    })
  }
)

observeEvent(
  progress$progress_end, {
    if (progress$progress_end == TRUE) {
      print("---helo----")
      output$ui_progressbar <- renderUI({
        NULL
      })

      shinyBS::createAlert(
        session = session, anchorId = "ui_hint_alert", alertId = NULL, title = NULL, style = "primary",
        content = HTML("<h3 style='color:red;'> Please check the results under the top-left menus of TCGA Cancer/Drug Response/GTEx Normal Tissue.</h3>"), append = FALSE
      )

      shinyjs::enable(id = "input_gene_set")
      shinyjs::enable(id = "analysis")
    }
  }
)

# guide result ------------------------------------------------------------

output$ui_guide_result <- renderUI({fn_guide_result()})

output$ui_feature_description <- renderUI({fn_feature_description()})


# Observe -----------------------------------------------------------------

observe(validate_input_gene_set())
observe(progressbar_start_analysis())
