# sourced by 'ui.R'
# save as 'gtex_expr_ui.R'
# ui elements 'gtex_expr' sub tab of 'gtex' tab

tabItem(
  tabName = "gtex_expr", align = "center",
  shinyjs::useShinyjs(),

  ## GTEx expr message ----
  fluidRow(
    style = "width:80%;",
    HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Gene Set Expression in GTEx Dataset
                      <font color='#777777'>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>expression profiles and gene set variation analysis score (GSVAS) of query gene set in selected GTEx normal tissues will be visualized here in forms of heatmap and boxplot. See details in <code>help page</code> below.) </p>
                      </div>
                      </div>
                      </div>
                      </div>")
  ),
  
  source(file.path(config$ui, "GTEx_exp_help.R"))[1],

  shiny::tags$br(),
  shiny::tags$hr(width = "85%"),


  # GTExTissueTypeInput("GTEx_exp"),

  # Selected Tissue show ----
  shiny::tags$h3("Tissue Check", class = "text-success"),
  shiny::tags$h4(
    "The tissues you selected: ",
    textOutput("selected_tissues"),
    " Confirm and start analysis by click Submit!"
  ),

  # Confirm and submit ----
  fluidRow(
    column(width = 4),
    column(
      width = 2, offset = 0,
      actionButton("GTEx_tissue_submit", label = "Submit!", icon = icon("check"))
    ),
    column(
      width = 2, offset = 0,
      actionButton("analysis_stop", label = "Stop!", icon = icon("pause")) 
    ),
    column(width = 4)
  ),
  shiny::tags$hr(width = "85%"),

  # output plot -------------------------------------------------------------
  # Tabset Panel ----
  fluidRow(
    column(
      width = 10,
      offset = 1,
      shiny::tags$br(),
      shinydashboard::tabBox(
        id = "GTEx_PLOT", title = "PLOT", width = 12,
        tabPanel(title = "GTEx expression", PlotInput(id = "GTEx_exp")),
        tabPanel(title = "GSVA score", PlotInput(id="GTEx_gsva"))
      )
    )
  ),
  # load footer ----
  source(file.path(config$ui, "footer.R"))[1]
) # close tab
