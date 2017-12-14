# sourced by 'ui.R'
# save as 'welcome_ui.R'
# ui elements for welcome


tabItem(
  tabName = "welcome", align = "center",

  shinyjs::useShinyjs(),

  # Welcome message ----
  fluidRow(
    style = "width:80%;",
    HTML("<div class='section'>
                  <div class='container text-center'>
                    <div class='row'>
                      <div class='col-md-12'>
                        <img src='./imgs/01.GSCA_logo_01.png' class='center-block img-responsive' style='height: 200px;'>
                      </div>
                    </div>
                  </div>
                </div>
                <div class='section'>
                  <div class='container'>
                    <div class='row'>
                      <div class='col-md-12'>
                        <p class='lead'>GSCALite offers you a web-based plattform for your gene set analysis of cancer.</p>
                     </div>
                    </div>
                  </div>
                </div>")
  ),

  # GSCA version ----
  # fluidRow(
  #   column(
  #     width = 4, offset = 4,
  #     shiny::tags$h4("Last Update: 2017-11-27", shiny::tags$span(class = "label label-default", "Version 0.0.1"))
  #   )
  # ),
  shiny::tags$hr(width = "50%"),

  # Input gene list ----
  fluidRow(
    column(
      width = 10, offset = 1,
      textInput(inputId = "gene_set", label = "Input Gene Symbol", value = "", placeholder = "TP53, ATG7"),
      actionButton(inputId = "analysis", label = "Analysis", icon = icon(name = "bolt", lib = "font-awesome")),
      verbatimTextOutput("gene_set")
    )
  ),

  # Feature and descriptions ----
  fluidRow(
    column(
      width = 10, offset = 1,
      shiny::tags$h1("GSCALite is a user-friendly analysis suite for gene set in cancer."),

      # Descriptions ----
      column(
        width = 6,
        shiny::tags$p(class = "lead", "Explore your Data, Explore your Analysis."),
        shiny::tags$p(
          class = "text-justify",
          "CRISPRAnalyzeR is a web-based analysis platform for pooled CRISPR screens.",
          "CRISPRAnalyzeR was developed with user experience in mind and provides you with a one-in-all data analysis workflow.",
          "And once you are finished, you can download all the data as well as your analysis as an interactive HTML report."
        ),
        shiny::tags$br()
      ),

      # Features ----
      column(
        width = 6,
        shiny::tags$ul(
          class = "cloud", style = "width:80%;",
          shiny::tags$li("Interactive"),
          shiny::tags$li("Easy-to-use"),
          shiny::tags$li("Gene set analysis"),
          shiny::tags$li("Cancer"),
          shiny::tags$li("GTEx"),
          shiny::tags$li("TCGA"),
          shiny::tags$li("GDSC"),
          shiny::tags$li("CTRP"),
          shiny::tags$li("Methylation"),
          shiny::tags$li("Network"),
          shiny::tags$li("Copy Number Variation"),
          shiny::tags$li("Single Nucleotide Mutation"),
          shiny::tags$li("Gene Set Analysis"),
          shiny::tags$li("Drug correlation")
        )
      )
    )
  ),

  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem
