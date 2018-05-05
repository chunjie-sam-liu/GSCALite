
# source by "server.R"
source(file.path(config$wd, "functions", "tutorial_function.R"))

output$ui_tutorial <- shiny::renderUI({fn_tutorial()})

output$ui_document <- shiny::renderUI({fn_document()})