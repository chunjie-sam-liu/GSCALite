
# source by "server.R"
source(file.path(config$wd, "functions", "tutorial_function.R"))

output$ui_tutorial <- shiny::renderUI({
  fn_tutorial()
})

output$ui_document <- shiny::renderUI({
  fn_document()
})

output$ui_tutorial_content <- shiny::renderUI({
  fn_tutorial_content()
})


shinyjs::onclick(id = "doc_expr", expr = shinyjs::js$openTab(id = "tcga_expr"))
shinyjs::onclick(id = "doc_snv", expr = shinyjs::js$openTab(id = "tcga_snv"))
shinyjs::onclick(id = "doc_cnv", expr = shinyjs::js$openTab(id = "tcga_cnv"))
shinyjs::onclick(id = "doc_meth", expr = shinyjs::js$openTab(id = "tcga_meth"))
shinyjs::onclick(id = "doc_path", expr = shinyjs::js$openTab(id = "tcga_rppa"))
shinyjs::onclick(id = "doc_mirna", expr = shinyjs::js$openTab(id = "tcga_mirna"))
shinyjs::onclick(id = "doc_drug", expr = shinyjs::js$openTab(id = "drug"))
shinyjs::onclick(id = "doc_gtex_expr", expr = shinyjs::js$openTab(id = "gtex_expr"))
shinyjs::onclick(id = "doc_gtex_eqtl", expr = shinyjs::js$openTab(id = "gtex_eqtl"))