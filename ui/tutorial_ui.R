# sourced by 'ui.R'
# save as 'help_ui.R'
# ui elements for help


tabItem(
  tabName = "help", align = "center",

  # help information ----
  # fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_tutorial")),
  
  # fluidRow(style = "width:80%", shiny::uiOutput(outputId = "ui_document")),
  
  fluidRow(style = "width:80%", shiny::uiOutput(outputId = "ui_tutorial_content")),
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem
