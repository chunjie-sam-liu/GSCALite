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
      
      gene_set <- readr::read_rds(path = file.path(config$wd, "userdata", "test_gene_set.rds.gz"))
      isolate({reactiveValuesToList(gene_set)}) -> gene_set
      
      expr %>%
        dplyr::filter(cancer_types %in% paired_cancer_types) %>%
        clean_expr(.gs = gene_set$match) -> expr_clean
      
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} clean data complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      
      # The table output
      output$expr_dt_comparison = DT::renderDataTable({
        DT::datatable(
          data = expr_clean,
          options = list(
            pageLength = 10, 
            autoWidth = TRUE, 
            order = list(list(5, "asc"), list(7, "desc"), list(6, "desc")),
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'print')
          ),
          rownames = FALSE,
          colnames = c("Cancer Types", "Symbol", "Normal expr.", "Tumor expr.", "Fold Change", "P-value", "FDR", "#Nomal", "#Tumor"),
          filter = "top",
          extensions = "Buttons",
          style = 'bootstrap',
          class = 'table-bordered table-condensed'
          ) %>% 
          DT::formatSignif(columns = c("Normal", "Tumor", "fc", "p.value", "fdr"), digits = 2) %>% 
          DT::formatRound(columns = c("Normal", "Tumor", "fc", "p.value", "fdr"), 2)
        })
      
      # plot
      expr_clean %>% expr_buble_plot() -> p
      
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} expr bubble plot complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      output$expr_bubble_plot <- renderPlot({p})
    }
  }
)

observe(expr_analysis())
