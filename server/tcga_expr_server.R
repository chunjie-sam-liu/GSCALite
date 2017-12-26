# source by "server.R"

source(file.path(config$wd, "functions", "data_function.R"))


expr_analysis <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      print(gene_set$match)
      
      # load data ---- 
      if (is.null(expr)) {
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading expr data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        expr <<- readr::read_rds(file.path(config$database, "TCGA", "expr", "pancan33_expr_filtered.rds.gz"))
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} loading expr data complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      }
      
      # gene_set <- readr::read_rds(path = file.path(config$wd, "userdata", "test_gene_set.rds.gz"))
      # isolate({reactiveValuesToList(gene_set)}) -> gene_set
      
      expr %>%
        dplyr::filter(cancer_types %in% paired_cancer_types) %>%
        clean_expr(.gs = gene_set$match) -> expr_clean
      
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} clean data complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      
      # plot
      expr_clean %>% expr_buble_plot() -> p
      
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} expr bubble plot complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      output$expr_bubble_plot <- renderPlot({
        p
      })
    }
  }
)

observe(expr_analysis())
