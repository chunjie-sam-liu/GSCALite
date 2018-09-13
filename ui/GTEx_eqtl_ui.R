# sourced by 'ui.R'
# save as 'gtex_expr_ui.R'
# ui elements 'gtex_expr' sub tab of 'gtex' tab

tabItem(
  tabName = "gtex_eqtl", align = "center",

  ## GTEx eqtl message ----
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_gtex_eqtl_welcome")),
  
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_gtex_eqtl_help")),
  
  fluidRow(style = "width:80%", shiny::tags$hr(style = "width:80%")),

  # source(file.path(config$ui, "GTEx_eqtl_help.R"))[1],
  # shiny::tags$hr(width = "85%"),

  # fluidRow(column(width = 12, GTExTissueeqtl("gtex_eqtl"))),
  fluidRow(selectAndAnalysisInput("gtex_eqtl")),
  # tissue type selection----
  fluidRow(column(width = 10, offset = 1, tissueTypeInput("gtex_eqtl"))),
  # generate result panel ----
  fluidRow(style = "width:80%", shiny::uiOutput(outputId = "ui_eqtl_result")),
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
