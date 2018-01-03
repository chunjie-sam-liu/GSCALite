# sourced by 'ui.R'
# save as 'about_ui.R'
# ui elements for about


tabItem(
  tabName = "about", align = "center",
  
  # Welcome message ----
  fluidRow(
    style = "width:95%;",
    
    column(
      width = 12, offset = 0,
      shiny::tags$img(
        src = "./imgs/about.png",
        class = "center-block img-responsive",
        style = "height: 600px;"
      ),
      shiny::tags$h1("Main authors contribute to this work.")
    )
  ),
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem