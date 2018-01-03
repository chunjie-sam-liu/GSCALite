

tabItem(
  tabName = "drug", align = "center",
  
  fluidRow(
    style = "width:80%;",
    column(
      width = 12, offset = 0,
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "Drug Resistance Analysis"
      ),
      
      shiny::tags$p(
        class = "lead text-left",
        "Drug resistance analaysis based on two databases ",
        shiny::tags$a("GDSC", href = "http://www.cancerrxgene.org/", target = "_blank", style = "color:#008176"),
        " and ",
        shiny::tags$a("CTRP", href = "http://www.cancerrxgene.org/", target = "_blank", style = "color:#008176"),
        "."
      )
    )
  ),
  
  fluidRow(drugOutput("drug")),
  
  
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
  
  
)