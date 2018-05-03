# sourced by 'ui.R'
# save as 'tcga_snv_ui.R'
# ui elements 'tcga_snv' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_snv", align = "center",
  shinyjs::useShinyjs(),

  ## SNV message --------------------------------------------
  fluidRow(
    style = "width:85%;",
    column(width = 12,offset = 0,
           shiny::tags$h1(
             class = "text-success text-left",
             shiny::icon(name = "angle-double-right", class = "fa-fw"),
             "Single Nucleotide Mutation"
           ),
           shiny::hr(),
           shiny::tags$p(
             class = "lead text-justify",
             "Single Nucleotide Variation(SNV) module presents the SNV frequency and variant types of the gene set in selected cancer types. The effects of mutations to overall survivalOS are given by means of the log-rank test which facilitate to evaluate the relationship between gene set mutations and clinical outcomes."
           )
           )
  ),

  # HELP as including of tcga_snv_help.R ---------------------
  source(file = file.path(config$wd, "ui", "tcga_snv_help.R"))[1],


  # cancer type selection and result output---------------------------------------------------
  # cancer type selection----
  fluidRow(style="width:85%;",column(width = 10, offset = 1, cancerTypeInput("snv"))),

  # Confirm and submit ----
  fluidRow(
    selectAndAnalysisInput("snv")
  ),
  fluidRow(style="width:85%;",shiny::uiOutput(outputId = "ui_snv_result")),
  # shiny::tags$hr(width="85%"),
  #
  # output plot -------------------------------------------------------------
  # Tabset Panel
  # fluidRow(
  #   column(width = 10,
  #          offset = 1,
  #          shinydashboard::tabBox(id = "snv_PLOT",title = "PLOT",width = 12,
  #                                 tabPanel(title= "SNV percentage profile",PlotInput(id="snv_percentage")),
  #                                 tabPanel(title="SNV summary",imagePlotInput("snv_summary",width=700,height="100%")),
  #                                 tabPanel(title="SNV oncoplot",imagePlotInput("snv_oncoplot",width=700,height="100%")),
  #                                 tabPanel(title="SNV survival",PlotInput("snv_survival"))
  #          )
  #   )
  # ),

  # load footer
  source(file.path(config$ui, "footer.R"))[1]
) # close tab