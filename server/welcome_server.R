# source by "server.R"

# Clear input -------------------------------------------------------------
observeEvent(input$input_gene_set_reset, {
  shinyjs::reset("input_gene_set")
  status$gene_set <- FALSE
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
observeEvent(input$example, {
  status$analysis <- FALSE
  status$gene_set <- FALSE
  shinyjs::js$example_gene_set(id = "seinput_gene_set")
  shinyjs::enable(id = "input_gene_set")
  shinyjs::enable(id = "analysis")
})

# after progress done



# Example input gene set --------------------------------------------------

addPopover(
  session = session,
  id = "example",
  title = "Example gene list",
  placement = "bottom",
  trigger = "hover",
  content = shiny::HTML(
    "Please input a gene list with official gene symbol less than 200 genes separated by space or comma or semicolon"
  )
)


# Monitor search ----------------------------------------------------------

validate_input_gene_set <- eventReactive(
  eventExpr = input$input_gene_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$gene_set <- TRUE

    if (is.null(input$input_gene_set) || input$input_gene_set == "") {
      error$gene_set <- "Error: Input at least One symbol."
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
output$gene_set_stat <- renderUI({
  if (status$gene_set) {
    shiny::tagList(
      # value box
      column(
        width = 8, offset = 2,

        downloadLink(
          outputId = "download_total_gene_set", label = NULL, class = NULL,

          valueBox(value = gene_set$n_total, subtitle = "Total Input Gene Set", icon = icon("users"), color = "yellow")
        ),

        downloadLink(
          outputId = "download_valid_gene_set", label = NULL, class = NULL,

          valueBox(
            value = gene_set$n_match, subtitle = "Valid Gene Set", icon = icon("credit-card"),
            color = "green"
          )
        ),
        downloadLink(
          outputId = "download_input_logs", label = NULL, class = NULL,

          valueBox(
            value = gene_set$n_non_match, subtitle = "Invalid Gene Set",
            icon = icon("line-chart"), color = "red"
          )
        )
      ),
      # proceed to analysis
      column(
        width = 8, offset = 2,
        shinyBS::bsButton(inputId = "analysis", label = "Start Gene Set Analysis", icon = icon("play"), class = "btn-lg"),
        shinyBS::bsButton(inputId = "stop", label = "Stop", icon = icon("pause"), class = "btn-lg danger")
      )
    )
  } else {
    NULL
  }
})
output$download_total_gene_set <- downloadHandler(
  filename = function() {
    glue::glue("{user_id}_total_gene_set.txt")
  },
  content = function(con) {
    .f <- file.path(user_dir, user_logs$gene_set)
    .d <- readr::read_delim(file = .f, delim = ":", skip = 3, col_names = FALSE, trim_ws = TRUE) %>%
      head(1) %>%
      .[[2]]
    readr::write_file(.d, con)
  }
)

output$download_valid_gene_set <- downloadHandler(
  filename = function() {
    glue::glue("{user_id}_valid_gene_set.txt")
  },
  content = function(con) {
    .f <- file.path(user_dir, user_logs$gene_set)
    .d <- readr::read_delim(file = .f, delim = ":", skip = 4, col_names = FALSE, trim_ws = TRUE) %>%
      head(1) %>%
      .[[2]]
    readr::write_file(.d, con)
  }
)

output$download_input_logs <- downloadHandler(
  filename = function() {
    glue::glue("{user_id}_input_gene_set_log.txt")
  },
  content = function(con) {
    .f <- file.path(user_dir, user_logs$gene_set)
    .d <- readr::read_file(file = .f)
    readr::write_file(.d, con)
  }
)

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
        content = HTML("<h3 ><i class='fa fa-hand-o-left fa-4'></i> Please check the result on the right panel.</h3>"), append = FALSE
      )

      shinyjs::enable(id = "input_gene_set")
      shinyjs::enable(id = "analysis")
    }
  }
)



#  Oberseve status$trigger -----------------------------------------------
observeEvent(status$trigger, {
  if (error$gene_set != "" && !is.null(error$gene_set)) {
    shinyBS::toggleModal(session = session, modalId = "gse_error_modal", toggle = "open")
  }
})

output$output_gene_set <- renderUI(return(shiny::HTML(error$gene_set)))

observe(validate_input_gene_set())
observe(progressbar_start_analysis())
