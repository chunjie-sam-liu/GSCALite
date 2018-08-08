# sourced by 'ui.R'
# save as 'tcga_rppa_ui.R'
# ui elements 'tcga_rppa' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_rppa", align = "center",
  shinyjs::useShinyjs(),

  ## RPPA message ----
  fluidRow(
    style = "width:85%;",
    column(
      width = 12, offset = 0,
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "Pathway Activity"
      ),
      shiny::hr(),
      shiny::tags$p(
        class = "lead text-justify",
        "Pathway Activity module presents the difference of genes expression between pathway activity groups (activation and inhibition) that defined by pathway scores. "
      )
    )
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
  fluidRow(style = "width:85%;",shiny::uiOutput(outputId = "ui_rppa_result")),
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
