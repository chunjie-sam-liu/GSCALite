# sourced by "welcome_server.R



# welcome message ---------------------------------------------------------

fn_welcom_msg <- function(){
  column(
    width = 12, offset = 0,
    shiny::tags$img(
      src = "./imgs/01.GSCA_logo_01.png",
      class = "center-block img-responsive",
      style = "height: 200px;"
    ),
    shiny::tags$h1("GSCALite offers you a web-based platform for Gene Set Cancer Analysis.")
  )
}
# search and examples -----------------------------------------------------

fn_search_example <- function(){
  tagList(
    column(
      width = 7, offset = 2,
      shinyWidgets::searchInput(
        inputId = "input_gene_set",
        label = "",
        placeholder = "Please input HGNC symbol gene set",
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        width = "100%"
      )
    ),
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
      )
    )
  )
}


# Gene set stat -----------------------------------------------------------
fn_gene_set_stat <- function(gene_set){
  column(
    width = 8, offset = 2, style = "margin-top:20px",
    downloadLink(
      outputId = "download_total_gene_set", label = NULL, class = NULL,
      valueBox(value = gene_set$n_total, subtitle = "Total Input Gene Set", icon = icon("users"), color = "yellow")
    ),
    
    downloadLink(
      outputId = "download_valid_gene_set", label = NULL, class = NULL,
      valueBox(
        value = gene_set$n_match, subtitle = "Valid Gene Set", icon = icon("credit-card"),
        color = "green"
      )
    ),
    downloadLink(
      outputId = "download_input_logs", label = NULL, class = NULL,
      valueBox(
        value = gene_set$n_non_match, subtitle = "Invalid Gene Set",
        icon = icon("line-chart"), color = "red"
      )
    )
  )
}


# multi cancer types input ------------------------------------------------

fn_multi_cancer_input <- function(.ctps){
  shiny::fluidRow(
    column(
      width = 4, offset = 2,
      multiInput(
        inputId = "select_ctps", label = "Select Cancer or Tissue (Selected in right)",
        choices = .ctps, selected = c('LUSC', 'LUAD', 'KICH', 'KIRP', 'KIRC'), width = "550px"
      ),
      
      shinyjs::hide(switchInput(
        inputId = "ctps_switch", label = "Cancers", value = FALSE,
        onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
      ))
    ),
    column(
      width = 4,
      multiInput(
        inputId = "select_analysis", label = "Select Analysis (Selected in right)", width = "550px",
        choices = c("mRNA Expression" = "expr", 
                    "Single Nucleotide Variation" = "snv",
                    "Copy Number Variation" = "cnv",
                    "Methylation" = "meth",
                    "Pathway Activity" = "rppa",
                    "miRNA Network" = "mirna",
                    "Drug Response" = "drug",
                    "GTEx Expression" = "gtex_exp",
                    "GTEx eQTL" = "eqtl"), selected = c("expr", "meth", "drug")
      ),
       shinyjs::hide(switchInput(
        inputId = "ana_switch", label = "Analysis", value = FALSE,
        onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
      ))
    )
  )
}

# start analysis widgets --------------------------------------------------
fn_start_analysis <- function(){
  column(
    width = 8, offset = 2, style = "margin-top:20px",
    shinyBS::bsButton(inputId = "analysis", label = "Start Gene Set Analysis", icon = icon("play"), class = "btn-lg"),
    shinyBS::bsButton(inputId = "stop", label = "Stop", icon = icon("pause"), class = "btn-lg danger")
  )
}

# download gene set button ------------------------------------------------

fn_gs_download <- function(user_dir, user_id, user_logs, txt, s){
  downloadHandler(
    filename = function() {
      glue::glue("{user_id}_{txt}")
    },
    content = function(con) {
      .f <- file.path(user_dir, user_logs$gene_set)
      .d <- readr::read_delim(file = .f, delim = ":", skip = s, col_names = FALSE, trim_ws = TRUE) %>%
        head(1) %>%
        .[[2]]
      if (s == 0) .d <- readr::read_file(file = .f)
      readr::write_file(.d, con)
    }
  )
}



# guide result ------------------------------------------------------------
fn_guide_result <- function(){
  column(
    sytle = "margin-top:30px;",
    width = 8, offset = 2,
    shinyBS::bsAlert(anchorId = "ui_hint_alert")
  )
}


# feature and description -------------------------------------------------

fn_feature_description <- function(){
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
        " CNV: Statistics of deletion/amplification of hetero/homozygous CNV."
      ),
      shiny::tags$p(
        class = "text-left",
        icon("hand-o-right"),
        " Methylation: Methylation affects survival and expression."
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
        " Drug analysis: Correlation of expression and drug sensitivity (IC50)."
      ),
      shiny::tags$p(
        class = "text-left",
        icon("hand-o-right"),
        " GTEx: Gene expression in normal tissue and eQTL."
      )
    )
  )
}
