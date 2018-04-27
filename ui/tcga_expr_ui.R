# sourced by "ui.R"


# Basic description -------------------------------------------------------

tabItem(
  tabName = "tcga_expr", align = "center",
  
  # welcome info
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_expr_welcome")),

  # For help page
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_expr_help")),

  fluidRow(column(width = 10, offset = 1, cancerTypeInput("expr"))),

  # Cancer type selection ----
  fluidRow(selectAndAnalysisInput("expr")),

  # Plot result ----
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_expr_result")),
  
  # fluidRow(exprOutput("expr")),

  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
)
