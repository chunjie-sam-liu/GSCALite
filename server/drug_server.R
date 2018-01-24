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
  
  output$gdsc <- renderPlot({gdsc_plot(.path, .gs)})
  output$ctrp <- renderPlot({ctrp_plot(.path, .gs)})
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