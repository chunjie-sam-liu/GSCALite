# source by "server.R"

source(file.path(config$wd, "functions", "drug_analysis.R"))

drug_output <- function(input, output, session, .path, .gs){
  output$gdsc <- renderPlot({NULL})
  output$ctrp <- renderPlot({NULL})
  print("---run drug---")
  
  output$gdsc <- renderPlot({gdsc_plot(.path, .gs)})
  output$ctrp <- renderPlot({ctrp_plot(.path, .gs)})
}

drug_analysis <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      print(gene_set$match)
      # load gdsc
      
      callModule(module = drug_output, id = "drug", .path = config$database, .gs = gene_set$match)
      
    }
  }
)

observe(drug_analysis())