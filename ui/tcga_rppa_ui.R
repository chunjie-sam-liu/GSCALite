# sourced by 'ui.R'
# save as 'tcga_rppa_ui.R'
# ui elements 'tcga_rppa' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_rppa", align = "center",
  shinyjs::useShinyjs(),

  ## RPPA message ----
  fluidRow(
    style = "width:80%;",
    HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Protein Expression
                      <font color='#777777'>
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>TCPA RPPA data</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>RPPA data from TCPA are used to calculate score for <b>10 cancer related pathways</b> and <b>32 cancer types</b>, and a relationship is generated between candidate gene expression and a specific pathway score (see details <code>help page</code> below). Here we show you the relationship between gene expression and pathway activity.</p>
                      </div>
                      </div>
                      </div>
                      </div>")
  ),
  ## Hlep message including in tcga_rppa_help.ui----
  source(file.path(config$ui, "tcga_rppa_help.R"))[1],

  shiny::tags$br(),
  shiny::tags$hr(width = "85%"),

  # cancer type selection and result output---------------------------------------------------
  # cancer type selection----
  cancerTypeInput("rppa"),

  # Confirm and submit ----
  fluidRow(
    selectAndAnalysisInput("rppa")
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
        id = "rppa_PLOT", title = "PLOT", width = 12,
        tabPanel(title = "Global percentage", imagePlotInput(id = "rppa_pie", width = "100%", height = "100%")),
        tabPanel(title = "Heatmap percentage", imagePlotInput(id = "rppa_per", width = "100%", height = "100%")),
        tabPanel(
          title = "Relation network",
          imageOutput("rppa_rela_plot", width = "100%", height = "100%"),
          hr()
        )
      )
    )
  ),

  # load footer ----
  source(file.path(config$ui, "footer.R"))[1]
) # close tab
