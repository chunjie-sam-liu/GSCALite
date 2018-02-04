# source by "server.R"

# source(file.path(config$wd, "functions", "drug_analysis.R"))
source(file.path(config$wd, "functions", "drug_function.R"))

# drug welcome info -------------------------------------------------------

output$ui_drug_welcome <- shiny::renderUI({fn_drug_welcome()})

output$ui_drug_help <- shiny::renderUI({fn_drug_help()})


# drug result -------------------------------------------------------------

output$ui_drug_result <- shiny::renderUI({fn_drug_result(selected_analysis$drug)})

drug_output <- function(input, output, session, .path, .gs){
  output$gdsc <- renderPlot({NULL})
  output$ctrp <- renderPlot({NULL})
  # GDSC 
  output$gdsc <- renderPlot(expr = {gdsc_plot(.path, .gs)})
  output$`gdsc-picdownload` <- downloadHandler(
    filename = function() {
      paste("GDSC_drug_sensitivity", ".", input$`gdsc-pictype`, sep = "")
    },
    content = function(file){
      print(file)
      ggsave(file,gdsc_plot(.path, .gs),device = input$`gdsc-pictype`,width = input$`gdsc-d_width`,height = input$`gdsc-d_height`)
    }
  )
  
  #CTRP
  output$ctrp <- renderPlot(expr = {ctrp_plot(.path, .gs)})
  output$`ctrp-picdownload` <- downloadHandler(
    filename = function() {
      paste("CTRP_drug_sensitivity", ".", input$`ctrp-pictype`, sep = "")
    },
    content = function(file){
      print(file)
      ggsave(file,ctrp_plot(.path, .gs),device = input$`ctrp-pictype`,width = input$`ctrp-d_width`,height = input$`ctrp-d_height`)
    }
  )
}

drug_analysis <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE && selected_analysis$drug == TRUE) {
      callModule(module = drug_output, id = "drug", .path = config$database, .gs = gene_set$match)
      
    }
  }
)

observe(drug_analysis())