# sourced by 'ui.R'
# save as 'gtex_expr_ui.R'
# ui elements 'gtex_expr' sub tab of 'gtex' tab

tabItem(
  tabName = "gtex_expr", align = "center",

  ## GTEx expr message ----
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_gtex_exp_welcome")),
  
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_gtex_exp_help")),
  
  fluidRow(style = "width:80%", shiny::tags$hr(style = "width:80%")),

  # source(file.path(config$ui, "GTEx_exp_help.R"))[1],

  
  fluidRow(selectAndAnalysisInput("GTEx_exp")),
  # tissue type selection----
  fluidRow(column(width = 10, offset = 1, tissueTypeInput("GTEx_exp"))),


  #  GTExTissueType("GTEx_exp"),

  # Selected Tissue show ----
  #  shiny::tags$h3("Tissue Check", class = "text-success"),
  #  shiny::tags$h4(
  #    "The tissues you selected: ",
  #    textOutput("selected_tissues"),
  #    " Confirm and start analysis by click Submit!"
  #  ),

  #  # Confirm and submit ----
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
  #  ),
  #  shiny::tags$hr(width = "85%"),

  # output plot -------------------------------------------------------------
  # # Tabset Panel ----


  # fluidRow(column(width = 12, GTExTissueType("gtex_expr"))),

  # Cancer type selection ----
  fluidRow(selectAndAnalysisInput("gtex_expr")),
  
  # generate result panel ----
  fluidRow(style = "width:80%", shiny::uiOutput(outputId = "ui_gexp_result")),

# Plot result ----
# fluidRow(
#   column(
#     width = 10,
#     offset = 1,
#     shinydashboard::tabBox(
#       id = "GTEx_PLOT", title = "PLOT", width = 12,
#       tabPanel(title = "GTEx expression", PlotInput(id = "GTEx_exp")) #,tabPanel(title = "GSVA score", PlotInput(id="GTEx_gsva"))
#     )
#   )
#   ),

  # load footer ----
  source(file.path(config$ui, "footer.R"))[1]
) # close tab
