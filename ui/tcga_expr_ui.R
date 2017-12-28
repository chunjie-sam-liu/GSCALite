# sourced by "ui.R"


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
        class = "lead",
        "TCGA expression data will be used to give you a visualization of your gene set for seleted cancer types."
      )
    )
  ),
  
  # For help page
  # source(file.path(config$ui,"tcga_cnv_help.R"))$value,
  
  fluidRow(column(width = 10, offset = 1, cancerTypeInput("expr"))),
  
  # Cancer type selection
  fluidRow(selectAndAnalysisInput("expr")),
  
  # Plot result ----
  fluidRow(
    column(
      width = 10, offset = 1,
      shinydashboard::tabBox(
        id = "expr_plot", title = "PLOT",width = 12,
        # bubble plot for tumor vs. normal
        tabPanel(
          title = "Tumor vs. Normal", 
          plotOutput(outputId = "expr_bubble_plot")
          ),
        # datatable
        tabPanel(
          title = "Table of comparison",
          DT::dataTableOutput(outputId = "expr_dt_comparison")
          )
      )
    )
  ),
  
  
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
)