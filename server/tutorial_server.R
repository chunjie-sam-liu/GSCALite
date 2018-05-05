
# source by "server.R"
source(file.path(config$wd, "functions", "tutorial_function.R"))

output$ui_tutorial <- shiny::renderUI({fn_tutorial()})

output$ui_document <- shiny::renderUI({fn_document()})


shinyjs::onclick(id = "doc_expr", expr = shinyjs::js$openTab(id="tcga_expr"))
shinyjs::onclick(id = "doc_snv", expr = shinyjs::js$openTab(id="tcga_snv"))