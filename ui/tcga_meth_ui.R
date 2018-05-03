# sourced by 'ui.R'
# save as 'tcga_meth_ui.R'
# ui elements 'tcga_meth' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_meth", align = "center",
  shinyjs::useShinyjs(),

  ## meth message ----
  fluidRow(
    style = "width:85%;",
    column(
      width = 12, offset = 0,
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "Methylation"
      ),
      shiny::hr(),
      shiny::tags$p(
        class = "lead text-justify",
        "Methylation module explores the differential methylation between tumor and paired normal, the correlation between methylation with expression and the OS affected by methyla-tion level for selected cancer types."
      )
  )),
  ## Hlep message including in tcga_meth_help.ui----
  source(file.path(config$ui, "tcga_meth_help.R"))[1],

  # shiny::tags$hr(width = "85%"),
  

  # cancer type selection and result output---------------------------------------------------
  # cancer type selection----
  fluidRow(style = "width:85%;",column(width = 10, offset = 1, cancerTypeInput("meth"))),
  
  # Confirm and submit ----
  fluidRow(
    selectAndAnalysisInput("meth")
  ),
  
  # generate result panel ----
  fluidRow(style = "width:85%;",shiny::uiOutput(outputId = "ui_meth_result")),

  # output plot -------------------------------------------------------------
  # Tabset Panel
  # fluidRow(
  #   column(
  #     width = 10,
  #     offset = 1,
  #     shinydashboard::tabBox(
  #       id = "snv_PLOT", title = "PLOT", width = 12,
  #       tabPanel(title = "Differential Methylation", PlotInput(id = "meth_diff")),
  #       tabPanel(title = "Methylation Survival", PlotInput(id = "meth_survival")),
  #       tabPanel(title = "Methylation to Expression", PlotInput(id = "meth_exp"))
  #     )
  #   )
  # ),
  # load footer
  source(file.path(config$ui, "footer.R"))[1]
) # close tab
