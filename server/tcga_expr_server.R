# source by "server.R"

source(file.path(config$wd, "functions", "data_function.R"))

expr_analysis <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      
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
        clean_expr() -> expr_clean
      
      # filter and rank
      expr_clean %>% filter_fc_pval() -> expr_clean_filter
      expr_clean %>% get_pattern() -> expr_clean_pattern
      expr_clean_pattern %>% get_cancer_types_rank() -> cancer_rank
      expr_clean_pattern %>% get_gene_rank() -> gene_rank
      
      # plot
      expr_clean_filter %>% expr_buble_plot() -> p
      
    }
  }
)

observe(expr_analysis())
