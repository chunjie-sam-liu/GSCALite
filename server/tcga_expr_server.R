# source by "server.R"

source(file.path(config$wd, "functions", "data_function.R"))

expr_analysis <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      
      print(ra_expr())
      # ra_expr() %>% 
      #   dplyr::filter(cancer_types %in% paired_cancer_types) %>% 
      #   purrr::map(
      #     .x = expr,
      #     .f = function(.x) {
      #       colnames(.x)[-1] %>% 
      #         barcode_process() %>% 
      #         filter_tumor_normal() %>% 
      #         paired_sample() 
      #     }
      #   )
        
    }
  }
)

observe(expr_analysis())
