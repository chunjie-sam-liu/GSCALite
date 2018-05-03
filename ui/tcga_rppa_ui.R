# sourced by 'ui.R'
# save as 'tcga_rppa_ui.R'
# ui elements 'tcga_rppa' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_rppa", align = "center",
  shinyjs::useShinyjs(),

  ## RPPA message ----
  fluidRow(
    style = "width:85%;",
    HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Pathway Activity
                      <font color='#777777'>
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>TCPA RPPA data</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead text-justify'>RPPA data from TCPA are used to calculate score for <b>10 cancer related pathways</b> and <b>32 cancer types</b>, and a relationship is predicted between gene expression and pathway score (see details <code>help page</code> below). Here we show you the relationship between gene expression and pathway activity.</p>
                      </div>
                      </div>
                      </div>
                      </div>")
  ),
  ## Hlep message including in tcga_rppa_help.ui----
  source(file.path(config$ui, "tcga_rppa_help.R"))[1],

  # cancer type selection and result output---------------------------------------------------
  # cancer type selection----
  fluidRow(column(width = 10, offset = 1, cancerTypeInput("rppa"))),

  # Confirm and submit ----
  fluidRow(
    selectAndAnalysisInput("rppa")
  ),
  
  # generate result panel ----
  fluidRow(shiny::uiOutput(outputId = "ui_rppa_result")),
  # output plot -------------------------------------------------------------
  # Tabset Panel
  # fluidRow(
  #   column(
  #     width = 10,
  #     offset = 1,
  #     shinydashboard::tabBox(
  #       id = "rppa_PLOT", title = "PLOT", width = 12,
  #       tabPanel(title = "Global percentage", imagePlotInput(id = "rppa_pie", width = "100%", height = "100%")),
  #       tabPanel(title = "Heatmap percentage", imagePlotInput(id = "rppa_per", width = "100%", height = "100%")),
  #       tabPanel(
  #         title = "Relation network",
  #         br(),
  #         br(),
  #         br(),
  #         imageOutput("rppa_rela_plot", width = "100%", height = "100%") %>% withSpinner(color="#0dc5c1"),
  #         hr()
  #       )
  #     )
  #   )
  # ),

  # load footer ----
  source(file.path(config$ui, "footer.R"))[1]
) # close tab
