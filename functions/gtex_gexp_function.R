fn_gtex_exp_welcome <- function() {
  column(
    width = 12, offset = 0,
    shiny::tags$h1(
      class = "text-success text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Gene Set Expression in GTEx Dataset"
    ),
    shiny::hr(),
    
    shiny::tags$p(
      class = "lead text-justify",
      "Expression profiles and gene set variation analysis score (GSVAs) of query gene set in selected GTEx normal tissues will be visualized here in forms of heatmap and boxplot."
    )
  )
}

fn_gtex_exp_help <- function(){
  column(
    width = 12, offset = 0,
    
    shiny::tags$div(
      class = "panel panel-primary",
      
      #Head
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#sqGTEx_expression",
            shiny::icon(name = "info-circle", class = "fa-fw"),
            "Click here for the detailed description of methods and results"
          )
        )
      ),
      
      # Body
      shiny::tags$div(
        id = "sqGTEx_expression", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          column(
            width = 12, offset = 0,
            
            # Methods
            shiny::tags$div(
              class = "bs-callout bs-callout-primary",
              shiny::tags$h3("Method"),
              
              shiny::tags$dl(
                class = "dl-horizontal",
                
                shiny::tags$dt("Data & Scripts:"),
                shiny::tags$dd("")
              )
            ),
            
            # Result
            shiny::tags$div(
              class = "bs-callout bs-callout-danger",
              shiny::tags$h3("Result"),
              
              shiny::tags$dl(
                class = "dl-horizontal",
                
                shiny::tags$dt("Data & Scripts:"),
                shiny::tags$dd("")
              )
            )
          )
        )
      )
    )
  )
}


gexpOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    fluidRow(shinydashboard::tabBox(
      id = "GTEx_PLOT", title = "", width = 12,
      tabPanel(title = "GTEx expression", PlotInput(id = "GTEx_exp")) #,tabPanel(title = "GSVA score", PlotInput(id="GTEx_gsva"))
    ))
  )
}

fn_gexp_result <- function(.gexp){
  if (.gexp == TRUE) {
    gexpOutput()
  } else{
    column(
      width = 10, offset = 1,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}