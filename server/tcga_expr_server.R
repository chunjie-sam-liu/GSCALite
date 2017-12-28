# source by "server.R"

source(file.path(config$wd, "functions", "data_function.R"))


# Check box ---------------------------------------------------------------
callModule(module = selectAndAnalysis, id = "expr", .id = "expr")

# Expression submit analysis ----------------------------------------------

expr_submit_analysis <- function(input, output, session, status, .expr_clean) {
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      if (status$analysis == TRUE) {
        .expr_clean %>% dplyr::filter(cancer_types %in% c("KICH", "KIRC", "KIRP")) -> .d
        print(.d)
        # output$expr_dt_comparison = DT::renderDataTable({expr_clean_datatable(clean_data)})
        # output$expr_bubble_plot <- renderPlot({expr_clean %>% expr_buble_plot()})
      }
    }
  )
}

# Expr submit analysis ----------------------------------------------------
expr_clean <- NULL

callModule(module = expr_submit_analysis, id = "expr", status = status, .expr_clean = expr_clean)


# From start analysis -----------------------------------------------------
expr_analysis <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      print(gene_set$match)
      
      # load data expr ---- 
      load_data_expr()
      
      # gene_set <- readr::read_rds(path = file.path(config$wd, "userdata", "test_gene_set.rds.gz"))
      # isolate({reactiveValuesToList(gene_set)}) -> gene_set
      
      expr %>%
        dplyr::filter(cancer_types %in% paired_cancer_types) %>%
        clean_expr(.gs = gene_set$match) ->> expr_clean
      
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} clean data complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      
      # The table output
      output$expr_dt_comparison = DT::renderDataTable({expr_clean_datatable(clean_data)})
      
      # plot
      output$expr_bubble_plot <- renderPlot({expr_clean %>% expr_buble_plot()})
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} expr bubble plot complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    }
  }
)
observe(expr_analysis())







