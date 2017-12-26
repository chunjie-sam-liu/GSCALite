# source by "server.R"

source(file.path(config$wd, "functions", "data_function.R"))

expr_analysis <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      
      if (is.null(expr)) {
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading expr data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        expr <<- readr::read_rds(file.path(config$database, "TCGA", "expr", "pancan33_expr_filtered.rds.gz"))
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} loading expr data complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      }
      
      expr %>%
        dplyr::filter(cancer_types %in% paired_cancer_types) %>%
        purrr::map(
          .x = expr,
          .f = function(.x) {
            colnames(.x)[-1] %>%
              barcode_process() %>%
              filter_tumor_normal() %>%
              paired_sample()
          }
        )
        
    }
  }
)

observe(expr_analysis())
