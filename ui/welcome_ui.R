# sourced by 'ui.R'
# save as 'welcome_ui.R'
# ui elements for welcome


tabItem(
  tabName = "welcome", align = "center",

  # Welcome message ----
  fluidRow(
    style = "width:80%;",

    column(
      width = 12, offset = 0,
      shiny::tags$img(
        src = "./imgs/01.GSCA_logo_01.png",
        class = "center-block img-responsive",
        style = "height: 200px;"
      ),

      shiny::tags$h1("GSCALite offers you a web-based platform for Gene Set Cancer Analysis.")
    )
  ),

  shiny::tags$hr(width = "50%"),

  # Input gene list ----
  fluidRow(
    column(
      width = 7, offset = 2,
      shinyWidgets::searchInput(
        inputId = "input_gene_set",
        label = "",
        placeholder = "Input gene list with comma seprated...",
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        width = "100%"
      )
    ),

    # for exmaple ----
    column(
      width = 1,
      shiny::tags$div(
        class = "form-group shiny-input-container",
        shiny::tags$label("for" = "margin"),
        shiny::tags$div(
          class = "input-group search-text",
          shiny::tags$span(
            class = "input-group-btn",
            shinyBS::bsButton(inputId = "example", label = "Show me example", icon = icon(name = "fire"))
          )
        )
      ),

      # shinybs popover ----
      shiny::uiOutput(outputId = "example_popover")
    ),

    # Control errors
    shinyBS::bsModal(
      id = "gse_error_modal", title = "",
      trigger = "gse_error_trigger", size = "large",
      fluidRow(
        style = "width:100%;",
        column(
          width = 8, offset = 2, class = "alert alert-danger text-justify",
          shiny::tags$span(style = "float:left; padding:10px;", shiny::icon(name = "exclamation-triangle", class = "fa-4x")),
          shiny::tags$span(shiny::tags$p(class = "lead text-center", shiny::uiOutput("output_gene_set")))
        )
      )
    )
  ),
  # gene set input stat output ----
  fluidRow(shiny::uiOutput(outputId = "gene_set_stat")),

  # progress bar for running -----
  fluidRow(shiny::uiOutput(outputId = "ui_progressbar")),

  fluidRow(shiny::uiOutput(outputId = "ui_hint")),

  # Feature and descriptions ----
  fluidRow(
    column(
      width = 10, offset = 1,
      style = "margin-top:30px;",
      # Descriptions ----
      shinydashboard::box(
        title = "GSCALite Introduction.",
        solidHeader = TRUE,
        width = 6,
        status = "primary",
        shiny::tags$p(
          class = "text-justify",
          "GSCALite is a web-based analysis platform for gene set cancer analysis. The alterations on DNA or RNA of cancer related genes may be contribute to the cancer initiation, progress, diagnosis, prognosis, therapy. As the cancer genomics big data available, it is very useful and urgent to provide a platform for gene set analysis in cancer."
        ),

        shiny::tags$p(
          style = "margin-top:30px; margin-bottom:10px;",
          class = "text-justify",
          "In this GSCALite, we integrated cancer genomics data of 33 cancer types from",
          shiny::tags$a("TCGA", href = "https://cancergenome.nih.gov/", target = "_blank", style = "color:#008176"),
          ", Drug response data from ",
          shiny::tags$a("GDSC", href = "http://www.cancerrxgene.org/", target = "_blank", style = "color:#008176"),
          " and ",
          shiny::tags$a("CTRP", href = "http://www.cancerrxgene.org/", target = "_blank", style = "color:#008176"),
          " as well as normal tissue data from",
          shiny::tags$a("GTEx", href = "https://www.gtexportal.org/home/datasets", target = "_blank", style = "color:#008176"),
          "for gene set analysis in a one-in-all data analysis workflow. When the analysis finished, users can download all the results and figures as an interactive HTML report."
        )
      ),


      # Features ----

      shinydashboard::box(
        title = HTML("In GSCALite, users can do following analysis for a <strong><font color='red'>gene set</font></strong>:"),
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " mRNA: Differential expression and cancer survival correlation."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " SNV: Statistics, distribution, types and its survival significance."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " CNV: The statistics of deletion/amplification of hetero/homozygous CNV."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Methylation: Differential methylation, correlation to survival and expression."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Cancer pathway activity: The activity of 10 cancer related pathways."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " miRNA network: Gene regulatory network by miRNAs."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Drug response: Correlation of gene expression and drug sensitivity (IC50)."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " GTEx: Gene expression in normal tissue and eQTL."
        )
      )
    )
  ),
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem