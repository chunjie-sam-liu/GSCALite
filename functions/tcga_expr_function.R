# sourced by "tcga_expr_server.R"

fn_expr_welcome <- function(){
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
}

# Expr output -------------------------------------------------------------

exprOutput <- function(id) {
  ns <- NS(id)
  column(
    width = 10, offset = 1,
    shinydashboard::tabBox(
      id = "expr_plot", title = "PLOT", width = 12,
      # bubble plot for tumor vs. normal
      tabPanel(
        title = "Tumor vs. Normal",
        plotOutput(outputId = ns("expr_bubble_plot")) %>% withSpinner()
      ),
      # datatable
      tabPanel(
        title = "Table of comparison",
        DT::dataTableOutput(outputId = ns("expr_dt_comparison")) %>% withSpinner()
      ),
      tabPanel(
        title = "Survival",
        plotOutput(outputId = ns("survival")) %>% withSpinner()
      ),
      tabPanel(
        title = "Subtype",
        plotOutput(outputId = ns("subtype")) %>% withSpinner()
      )
    )
  )
}

fn_expr_result <- function(.expr){
  
  if (.expr == TRUE) {
    exprOutput("expr")
  } else{
    column(
      width = 10, offset = 1,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}