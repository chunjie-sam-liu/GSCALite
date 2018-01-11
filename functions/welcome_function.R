# sourced by "welcome_server.R

# Gene set stat -----------------------------------------------------------
fn_gene_set_stat <- function(gene_set){
  column(
    width = 8, offset = 2,
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

fn_multi_cancer_input <- function(){
  .ctps <- c("BRCA", "KIRC", "KIRP")
  
  multiInput(
    inputId = "multi_cancer_types", label = "Select Cancer Types",
    choices = .ctps, selected = "", width = "350px"
  )
}

# start analysis widgets --------------------------------------------------
fn_start_analysis <- function(){
  column(
    width = 8, offset = 2,
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
