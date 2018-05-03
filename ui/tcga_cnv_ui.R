# sourced by 'ui.R'
# save as 'tcga_cnv_ui.R'
# ui elements 'tcga_cnv' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_cnv", align = "center",
  shinyjs::useShinyjs(),

  ## SNV message ----
  fluidRow(
    style = "width:85%;",
    column(
      width = 12, offset = 0,
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "Copy Number variation"
      ),
      shiny::hr(),
      shiny::tags$p(
        class = "lead text-justify",
        "On Copy Number Variation module, the statistics of hetero-zygous and homozygous CNV of each cancer type are dis-played as pie chat for gene set, and Pearson correlation is performed between gene expression and CNV of each gene in each cancer to help to analyze the gene expression signifi-cantly affected by CNV."
      )
    )
  ),
  ## Hlep message including in tcga_cnv_help.ui----
  source(file.path(config$ui, "tcga_cnv_help.R"))[1],

  # shiny::tags$hr(width = "85%"),

  # cancer type selection and result output---------------------------------------------------
  # cancer type selection----
  fluidRow(column(width = 10, offset = 1, cancerTypeInput("cnv"))),


  # Confirm and submit ----
  fluidRow(
    selectAndAnalysisInput("cnv")
  ),
  
  # generate result panel ----
  fluidRow(style = "width:85%;", shiny::uiOutput(outputId = "ui_cnv_result")),
  # output plot -------------------------------------------------------------
  # Tabset Panel
  # fluidRow(
  #   column(width = 10,
  #          offset = 1,
  #          shinydashboard::tabBox(id = "cnv_PLOT",title = "PLOT",width = 12,
  #                                 tabPanel(title="CNV Pie distribution",imagePlotInput(id="cnv_pie",width="100%",height="100%")),
  #                                 tabPanel(title= "Hete CNV profile",PlotInput(id="cnv_hete")),
  #                                 tabPanel(title="Homo CNV profile",PlotInput(id="cnv_homo")),
  #                                 tabPanel(title="Overall CNV frenquency",PlotInput("cnv_bar")),
  #                                 tabPanel(title="CNV to Expression",PlotInput("cnv_exp"))
  #                                 # tabPanel(title="CNV oncostrip",PlotInput("cnv_oncostrip")),
  #                                 # tabPanel(title="Exclusive CNV",PlotInput("cnv_exclusive"))
  #          )
  #   )
  # ),

  # load footer ------------------------------------------------------
  source(file.path(config$ui, "footer.R"))[1]
) # close tab