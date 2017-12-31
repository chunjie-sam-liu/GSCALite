# sourced by 'ui.R'
# save as 'tcga_meth_ui.R'
# ui elements 'tcga_meth' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_meth", align = "center",
  shinyjs::useShinyjs(),

  ## meth message ----
  fluidRow(
    style = "width:80%;",
    HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Methylation
                      <font color='#777777'>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>TCGA methylation data will be used to give you a visualization of you gene set for seleted cancer types.
                      <br>GSCALite offers different types of results (Differential Methylation, Methylation to Survival, Methylation correlate to gene expression, see details in <code>help page</code> below.), you can have a visualization of methylation changes and the effect of it to survival and expression.</p>
                      </div>
                      </div>
                      </div>
                      </div>")
  ),
  ## Hlep message including in tcga_meth_help.ui----
  source(file.path(config$ui, "tcga_meth_help.R"))[1],

  shiny::tags$br(),
  shiny::tags$hr(width = "85%"),

  # cancer type selection and result output---------------------------------------------------
  # cancer type selection----
  cancerTypeInput("meth"),

  # Confirm and submit ----
  fluidRow(
    selectAndAnalysisInput("meth")
  ),
  shiny::tags$hr(width = "85%"),

  # output plot -------------------------------------------------------------
  # Tabset Panel
  fluidRow(
    column(
      width = 10,
      offset = 1,
      shiny::tags$br(),
      shinydashboard::tabBox(
        id = "snv_PLOT", title = "PLOT", width = 12,
        tabPanel(title = "Differential Methylation", PlotInput(id = "meth_diff")),
        tabPanel(title = "Methylation Survival", PlotInput(id = "meth_survival")),
        tabPanel(title = "Methylation to Expression", PlotInput(id = "meth_exp"))
      )
    )
  ),
  # load footer
  source(file.path(config$ui, "footer.R"))[1]
) # close tab
