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
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#help_expr",
            shiny::icon(name = "question", class = "fa-fw"),
            "Click here for help"
            )
          )
        )
      ),
    shiny::tags$div(
      id = "help_expr", class = "panel-collapse collapse",
      shiny::tags$div(
        class = "panel-body",
        column(
          width = 12, offset = 0,
          # methods
          column(
            width = 10, offset = 1,
            shiny::tags$h3("Method", class = "text-success"),
            shiny::tags$p(
              class = "text-justify",
              "1. The mRNA expression and clinical data was downloaded from", shiny::tags$a("href" = "https://gdc.cancer.gov/", "NCI Genomic Data Commons", style = "color:#08176" )
            ),
            
            shiny::tags$p(
              class = "text-justify",
              "2. TCGA maintains 33 cancer types, but only 14 cancer types have paired tumor vs. normal data. The  gene set mRNA differential expression was based on the 14 cancer types."
            ),
            
            shiny::tags$p(
              class = "text-justify",
              "3. Survival and subtype was analysis across all the cancer types the user chose"
            ),
            shiny::tags$hr(width = "100%")
            )
          )
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