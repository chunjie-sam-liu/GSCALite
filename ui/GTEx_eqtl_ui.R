# sourced by 'ui.R'
# save as 'gtex_expr_ui.R'
# ui elements 'gtex_expr' sub tab of 'gtex' tab

tabItem(
  tabName = "GTEx_eqtl", align = "center",
  shinyjs::useShinyjs(),

  ## GTEx eqtl message ----
  fluidRow(
    style = "width:80%;",
    HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Expression quantitative trait locis (eQTLs) in GTEx Dataset
                      <font color='#777777'>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>The eQTLs of gene set in the selected GTEx normal tissues will be displayed here in the form of table. See detailed explaination for each column in the talble <code>help page</code> below.) </p>
                      </div>
                      </div>
                      </div>
                      </div>")
  ),

  source(file.path(config$ui, "GTEx_eqtl_help.R"))[1],
  shiny::tags$hr(width = "85%"),

  # fluidRow(column(width = 12, GTExTissueeqtl("gtex_eqtl"))),
  fluidRow(selectAndAnalysisInput("gtex_eqtl")),
  # tissue type selection----
  fluidRow(column(width = 10, offset = 1, tissueTypeInput("gtex_eqtl"))),
  # generate result panel ----
  fluidRow(shiny::uiOutput(outputId = "ui_eqtl_result")),
  #  GTExTissueType("gtex_eqtl"),

  # Selected Tissue show ----
  #  shiny::tags$h3("Tissue Check", class = "text-success"),
  #  shiny::tags$h4(
  #    "The tissues you selected: ",
  #    textOutput("selected_tissues"),
  #    " Confirm and start analysis by click Submit!"
  #  ),

  # Confirm and submit ----


  #  fluidRow(
  #    column(width = 4),
  #    column(
  #      width = 2, offset = 0,
  #      actionButton("GTEx_tissue_submit", label = "Submit!", icon = icon("check"))
  #    ),
  #    column(
  #      width = 2, offset = 0,
  #      actionButton("analysis_stop", label = "Stop!", icon = icon("pause"))
  #    ),
  #    column(width = 4)
  # ),
  #  shiny::tags$hr(width = "85%"),


  # output plot -------------------------------------------------------------
  # Tabset Panel ----
  #  fluidRow(
  #    column(
  #      width = 10,
  #      offset = 1,
  #      shiny::tags$br(),
  #      shinydashboard::tabBox(
  #        id = "GTEx_TABLE", title = "TABLE", width = 12,
  #        tabPanel(title = "GTEx eQTLs", TableInput(id = "GTEx_eqtl"))
  #      )
  #    )
  #  ),
  # fluidRow(GTEx_eqtl_Output("gtex_eqtl")),
  # load footer ----
  source(file.path(config$ui, "footer.R"))[1]
) # close tab
