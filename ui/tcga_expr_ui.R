# sourced by "ui.R"


# Basic description -------------------------------------------------------

tabItem(
  tabName = "tcga_expr", align = "center",

  fluidRow(
    style = "width:80%;",
    column(
      width = 12, offset = 0,
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "Gene Set Expression"
      ),

      shiny::tags$p(
        class = "lead text-left",
        "TCGA expression data will be used to give you a visualization of your gene set for seleted cancer types."
      )
    )
  ),

  # For help page
  # source(file.path(config$ui,"tcga_cnv_help.R"))$value,

  fluidRow(column(width = 10, offset = 1, cancerTypeInput("expr"))),

  # Cancer type selection ----
  fluidRow(selectAndAnalysisInput("expr")),

  # Plot result ----
  fluidRow(exprOutput("expr")),


  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
)
