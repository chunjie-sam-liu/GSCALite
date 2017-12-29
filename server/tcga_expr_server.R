# source by "server.R"

source(file.path(config$wd, "functions", "data_function.R"))

expr_clean <- NULL

# Check box ---------------------------------------------------------------
callModule(module = selectAndAnalysis, id = "expr", .id = "expr")

# Selected cancer types ---------------------------------------------------

expr_cancer_type <- callModule(cancerType, id = "expr")

# Expression submit analysis ----------------------------------------------

expr_submit_analysis <- function(input, output, session, status, .expr_clean) {
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      if (status$analysis == TRUE) {
        print(glue::glue("select {expr_cancer_type()}"))
        .expr_clean %>% dplyr::filter(cancer_types %in% expr_cancer_type()) -> .d
        output$expr_dt_comparison = DT::renderDataTable({expr_clean_datatable(.d)})
        output$expr_bubble_plot <- renderPlot({.d %>% expr_buble_plot()})
      }
    }
  )
}

callModule(module = expr_submit_analysis, id = "expr", status = status, .expr_clean = expr_clean)


# Start analysis ----------------------------------------------------------

expr_start_analysis <- function(input, output, session, .expr_clean) {
    output$expr_dt_comparison = DT::renderDataTable({expr_clean_datatable(.expr_clean)})
    output$expr_bubble_plot <- renderPlot({.expr_clean %>% expr_buble_plot()})
}


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
      
      callModule(module = expr_start_analysis, id = "expr", .expr_clean = expr_clean)
      
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} expr bubble plot complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    }
  }
)

observe(expr_analysis())







