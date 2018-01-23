# sourced by 'ui.R'
# save as 'welcome_ui.R'
# ui elements for welcome


tabItem(
  tabName = "welcome", align = "center",

  # Welcome ----
  fluidRow(shiny::uiOutput(outputId = "ui_welcom_msg")),

  shiny::tags$hr(width = "80%"),

  # Input gene list ----
  fluidRow(shiny::uiOutput(outputId = "ui_search_example")),
  
  # Select cancer types ----
  shiny::uiOutput(outputId = "ui_multi_cancer_input"),
  
  # Start analysis ---- 
  fluidRow(shiny::uiOutput(outputId = 'ui_start_analysis')),
  
  # progress bar for running -----
  fluidRow(shiny::uiOutput(outputId = "ui_progressbar")),
  
  # Guide to result
  fluidRow(shiny::uiOutput(outputId = "ui_guide_result")),
  
  # gene set input stat output ----
  fluidRow(shiny::uiOutput(outputId = "ui_gene_set_stat")),

  # Feature and descriptions ----
  fluidRow(shiny::uiOutput(outputId = "ui_feature_description")),
  
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem
