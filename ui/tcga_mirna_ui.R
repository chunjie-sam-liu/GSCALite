# sourced by 'ui.R'
# save as 'tcga_mirna_ui.R'
# ui elements 'tcga_mirna' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_mirna", align = "center",
  shinyjs::useShinyjs(),

  ## SNV message ----
  fluidRow(
    style = "width:85%;",
    column(
      width = 12, offset = 0,
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "miRNA Regulation"
      ),
      shiny::hr(),
      shiny::tags$p(
        class = "lead text-justify",
        "miRNA Regulation will give you a miRNA regulation network, for you to visualize the potential regulation of miRNAs to your genes."
      )
    )
  ),
  ## Hlep message including in tcga_mirna_help.ui----
  source(file.path(config$ui, "tcga_mirna_help.R"))[1],

  # shiny::tags$hr(width = "85%"),
  
  # Confirm and submit ----
  fluidRow(
    selectAndAnalysisInput("mirna")
  ),

  # generate result panel ----
  fluidRow(style = "width:85%;",shiny::uiOutput(outputId = "ui_mirna_result")),

  # load footer ------------------------------------------------------
  source(file.path(config$ui, "footer.R"))[1]
) # close tab