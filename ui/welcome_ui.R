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
      id = "gse_error_modal", title = "Error: Input symbol errors.",
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

  fluidRow(shiny::uiOutput(outputId = "gene_set_stat")),

  # Feature and descriptions ----
  fluidRow(
    style = "margin-top: 30px;",
    column(
      width = 10, offset = 1,

      # Descriptions ----
      # column(width = 6,
      shinydashboard::box(
        title = "Explore your data and analysis.",
        solidHeader = TRUE,
        width = 6,
        status = "primary",
        shiny::tags$p(
          class = "text-justify",
          "GSCALite is a web-based analysis platform for gene set cancer analysis. The alterations on DNA or RNA of cancer related genes may be contribute to the cancer initiation, progress, diagnosis, prognosis, therapy. As the cancer genomics big data available, it is very useful and urgent to provide a platform for gene set analysis in cancer."
          ),
        shiny::tags$p(
          class = "text-justify",
          "In this GSCALite, we integrated cancer genomics data of 33 cancer types from",
          shiny::tags$a("TCGA", href = "https://cancergenome.nih.gov/", target = "_blank", style = "color:blue"),
          ", Drug response data from ",
          shiny::tags$a("GDSC", href = "http://www.cancerrxgene.org/", target = "_blank", style = "color:blue"),
          " and ",
          shiny::tags$a("CTRP", href = "http://www.cancerrxgene.org/", target = "_blank", style = "color:blue"),
          " as well as normal tissue data from",
          shiny::tags$a("GTEx", href = "https://www.gtexportal.org/home/datasets", target = "_blank", style = "color:blue"),
          "for gene set analysis in a one-in-all data analysis workflow. When the analysis finished, users can download all the results and figures as an interactive HTML report."
        )
      ),
      # ),

      # Features ----
      # column(
      #   width = 6,
      shinydashboard::box(
        title = "In GSCALite, you can get:",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Gene set of mRNA expression differences between tumor and normal samples, ",
          "and clinical features in all cancers."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Single nucleotide mutation analysis of gene set in all cancers, ",
          "and analysis of SNV on overall survival."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Overall Copy number variation of gene set in all cancers and influence of CNV on gene expression."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Gene set analysis of methylation differences between tumor and normal samples,",
          " and methylation level on gene expression and overall survival."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          "  Genes potentially regulate the activity of 10 famous cancer related pathways."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          "  A network in which genes potentially regulated by the miRNAs."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Gene set analysis of expression which have drugs resistance."
        ),
        shiny::tags$p(
          class = "text-left",
          icon("hand-o-right"),
          " Gene set analysis of expression profile and variation in normal tissue."
        )
      )
      # )
    )
  ),
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem