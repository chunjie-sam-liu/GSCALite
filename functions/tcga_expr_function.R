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
      "GCSALite mRNA expression module calculate the gene set differential expression across cancers based on the TCGA expression data.", 
      "The module analysis result provides differential expression, survival analysis and subtype analysis.",
      "See the details in",
      shiny::tags$code("help page.")
    )
  )
}

fn_expr_help <- function(){
  column(
    width = 12, offset = 0,
    
    shiny::tags$div(
      class = "panel panel-default",
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          HTML('<a data-toggle="collapse" href="#help_expr">
               <i class="fa fa-question fa-fw"></i> 
               Click here for help</a>')
          )
        )
      ),
    shiny::tags$div(
      id = "help_expr", class = "panel-collapse collapse",
      shiny::tags$div(
        class = "panel-body",
        column()
        )
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