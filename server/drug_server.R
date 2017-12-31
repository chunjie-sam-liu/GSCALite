# source by "server.R"

drug_analysis <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      print(gene_set$match)
      
      
    }
  }
)

observe(drug_analysis())