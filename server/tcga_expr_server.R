# source by "server.R"

source(file.path(config$wd, "functions", "data_function.R"))

expr_clean <- NULL

# Selected cancer types ---------------------------------------------------

expr_cancer_type <- callModule(module = cancerType, id = "expr")

# Cancer types value box selection ----------------------------------------

expr_cancer_type <- callModule(cancerType, id = "expr")

callModule(module = cancerTypesSelect, id = "expr", .sctps = expr_cancer_type)

# Check box ---------------------------------------------------------------

callModule(module = selectAndAnalysis, id = "expr", .id = "expr")


# Expression submit analysis ----------------------------------------------

expr_submit_analysis <- function(input, output, session, status, .expr_clean, paired_cancer_types) {
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      if (status$analysis == TRUE) {
        output$expr_dt_comparison <- DT::renderDataTable({NULL})
        output$expr_bubble_plot <- renderPlot({NULL})
        
        print(glue::glue("select {expr_cancer_type()}"))
        # filter for paired sample
        .expr_clean %>% dplyr::filter(cancer_types %in% expr_cancer_type()) -> .d
        
        .valid_ctps <- intersect(paired_cancer_types, expr_cancer_type())
        .invalid_ctps <- setdiff(expr_cancer_type(), paired_cancer_types)
        
        if (nrow(.d) > 0) {
          .msg <- glue::glue("
The analysis based on paired sample in each cancer types.
In this analysis, only {length(.valid_ctps)} cancer types have paired samples. 
They are {paste0(.valid_ctps, collapse = ',')}. The cancer type {paste0(.invalid_ctps, collapse = ',')} don't have paired samples.
                             ")
          
          output$expr_dt_comparison <-  DT::renderDataTable({expr_clean_datatable(.d)})
          output$expr_bubble_plot <- renderPlot({.d %>% expr_buble_plot()})
          
        } else {
          .msg <- glue::glue("No paired sample in your selected cancer types.")
        }
        
        # alert for information
        shinyBS::createAlert(
          session = session, anchorId = "expr-no_gene_set", title = "Information", style = "info",
          content = .msg, append = FALSE
        )
        
      } else {
        shinyBS::createAlert(
          session = session, anchorId = "expr-no_gene_set", title = "Oops",
          content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
        )
      }
    }
  )
}

callModule(
  module = expr_submit_analysis, id = "expr", status = status, 
  .expr_clean = expr_clean, paired_cancer_types = paired_cancer_types
  )


# Start analysis ----------------------------------------------------------

expr_start_analysis <- function(input, output, session, .expr_clean) {
    output$expr_dt_comparison = DT::renderDataTable({expr_clean_datatable(.expr_clean)})
    output$expr_bubble_plot <- renderPlot({.expr_clean %>% expr_buble_plot()})
}


# From start analysis -----------------------------------------------------
expr_analysis <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {

      print(gene_set$match)
      
      # load data expr ---- 
      processing$start_loading_start <- TRUE
      print(glue::glue("processing$start_loading_start {processing$start_loading_start}"))
      
      updateProgressBar(session = session, id = "progressbar", value = 40, status = "danger")
      session$onFlushed(function() {progress$expr_loading <- TRUE})
      
      observeEvent(
        eventExpr = progress$expr_loading,
        handlerExpr = {
          
          if (progress$expr_loading == TRUE) {
            
            load_data_expr()
            
            processing$start_loading_end <- TRUE
            }
        })
      
      observeEvent(processing$start_loading_end, {
        if (processing$start_loading_end == TRUE) {
          updateProgressBar(session = session, id = "progressbar", value = 70, status = "warning")
          session$onFlushed(function() {progress$expr_calc <- TRUE})
        }
      })
      
      observeEvent(
        eventExpr = progress$expr_calc,
        handlerExpr = {
          if (progress$expr_calc == TRUE) {
            
            expr %>%
              dplyr::filter(cancer_types %in% paired_cancer_types) %>%
              clean_expr(.gs = gene_set$match) ->> expr_clean
            
            print(glue::glue("{paste0(rep('-', 10), collapse = '')} clean data complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
            
            callModule(module = expr_start_analysis, id = "expr", .expr_clean = expr_clean)
            
            print(glue::glue("{paste0(rep('-', 10), collapse = '')} expr bubble plot complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
            
            processing$expr_calc_end <- TRUE
          }
        })
      
      observeEvent(processing$expr_calc_end, {
        if (processing$expr_calc_end == TRUE) {
          updateProgressBar(session = session, id = "progressbar", value = 100, status = "info")
          progress$progress_end <- TRUE
        }
      })
    }
  }
)

observe(expr_analysis())







