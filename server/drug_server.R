# source by "server.R"

# source(file.path(config$wd, "functions", "drug_analysis.R"))
source(file.path(config$wd, "functions", "drug_function.R"))

# drug welcome info -------------------------------------------------------

output$ui_drug_welcome <- shiny::renderUI({fn_drug_welcome()})

output$ui_drug_help <- shiny::renderUI({fn_drug_help()})


# drug result -------------------------------------------------------------

output$ui_drug_result <- shiny::renderUI({fn_drug_result(selected_analysis$drug)})

drug_output <- function(input, output, session, ctrp_gene_list_sig_drug, gdsc_gene_list_sig_drug, t_gdsc){
  output$gdsc <- renderPlot({NULL})
  output$ctrp <- renderPlot({NULL})
  # GDSC 
  if(nrow(gdsc_gene_list_sig_drug)>0){
    gdsc_height <- gdsc_gene_list_sig_drug$drug_name %>% unique() %>% length()*18
    output$gdsc <- renderPlot(expr = {gdsc_plot(gdsc_gene_list_sig_drug, t_gdsc)},height = function(){ifelse(gdsc_height<200,200,gdsc_height)})
    output$`gdsc-picdownload` <- downloadHandler(
      filename = function() {
        paste("GDSC_drug_sensitivity", ".", input$`gdsc-pictype`, sep = "")
      },
      content = function(file){
        print(file)
        ggsave(file,gdsc_plot(gdsc_gene_list_sig_drug, t_gdsc),device = input$`gdsc-pictype`,width = input$`gdsc-d_width`,height = input$`gdsc-d_height`)
      }
    )
    .msg_gdsc <- NULL
    } else {
  .msg_gdsc <- paste(glue::glue("Your selected genes: {paste0(gene_set$match, collapse = ', ')} have on correlation with GDSC drug sensitivity."), sep = " ")
  }
  
  
  #CTRP
  if(nrow(ctrp_gene_list_sig_drug)>0){
    ctrp_height <- ctrp_gene_list_sig_drug$drug_name %>% unique() %>% length()*20
  output$ctrp <- renderPlot(expr = {ctrp_plot(ctrp_gene_list_sig_drug)},height = function(){ifelse(ctrp_height<200,200,ctrp_height)})
  output$`ctrp-picdownload` <- downloadHandler(
    filename = function() {
      paste("CTRP_drug_sensitivity", ".", input$`ctrp-pictype`, sep = "")
    },
    content = function(file){
      print(file)
      ggsave(file,ctrp_plot(ctrp_gene_list_sig_drug),device = input$`ctrp-pictype`,width = input$`ctrp-d_width`,height = input$`ctrp-d_height`)
    }
  )
  .msg_ctrp <- NULL
  
  } else {
    .msg_ctrp <- paste(glue::glue("Your selected genes: {paste0(gene_set$match, collapse = ', ')} have on correlation with CTRP drug sensitivity."), sep = " ")
  }
  
  # message output ----
  output[["gdsc_message"]] <- renderUI({
    tagList(
      shiny::tags$p(.msg_gdsc, style = "color:#CD3700")
    )
  })
  output[["ctrp_message"]] <- renderUI({
    tagList(
      shiny::tags$p(.msg_ctrp, style = "color:#CD3700")
    )
  })
  
}

drug_analysis <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE && selected_analysis$drug == TRUE) {
      load_data_drug()
      drug_gdsc %>%
        dplyr::filter(symbol %in% gene_set$match) %>%
        dplyr::mutate(cor_drug = purrr::map(.x = drug, .f = fn_filter_drug)) %>%
        tidyr::unnest(cor_drug) -> gdsc_gene_list_sig_drug
      
      
      drug_ctrp %>%
        dplyr::filter(symbol %in% gene_set$match) %>%
        dplyr::mutate(cor_drug = purrr::map(.x = drug, .f = fn_filter_drug_ctrp)) %>%
        tidyr::unnest(cor_drug) -> ctrp_gene_list_sig_drug
      
      .msg_alert <- c("NOTE: Drug module performs correlation analysis for all cancer cell lines. Selecting cancer types at the home page will make no difference for the results.")
      shinyBS::createAlert(
        session = session, anchorId = "drug-note", title = "Information", style = "info",
        content = .msg_alert, append = FALSE
      )
      callModule(module = drug_output, id = "drug",  ctrp_gene_list_sig_drug, gdsc_gene_list_sig_drug, t_gdsc)
      
    }
  }
)

observe(drug_analysis())