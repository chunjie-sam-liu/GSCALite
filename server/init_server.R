# sourced by "server.R"
# save as "init_server.R"


# Create session ----------------------------------------------------------

start_time <- Sys.time()
user_id <- paste0(format(x = start_time, format = "%y%m%d_%H%M%S_"), paste(sample(0:9, 4), collapse = ""))

# Test cdata
cdata <- session$clientData
# cdata <- readr::read_rds(file.path(config$wd, "userdata", "cdata_test.rds.gz"))

# Temp user data directory
user_dir <- file.path(config$wd, "userdata", user_id)
pngs_dir <- file.path(config$wd, "userdata", user_id, "pngs")
jsons_dir <- file.path(config$wd, "userdata", user_id, "jsons")
ifelse(dir.exists(user_dir), glue::glue("Directory {user_dir} exists!"), dir.create(user_dir))
cmd <- "chmod"
args <- c("-R", "777", user_dir)
system2(command = cmd, args = args)
dir.create(pngs_dir)
dir.create(jsons_dir)

session$onSessionEnded(function() {
  unlink(user_dir, recursive = TRUE)
  log <- glue::glue("{user_id} : shiny session finished at {Sys.time()}")
  write(x = log, file = log_file, append = TRUE)
})

# Log user access  --------------------------------------------------------

log_file <- file.path(config$logs, "app.log")

local({
  log <- c(
    glue::glue("{user_id} : shiny session starting at {Sys.time()}"),
    glue::glue("{user_id} : with user_dir {user_dir}")
  )
  if (!file.exists(log_file)) {
    write(x = log, file = log_file)
  } else {
    write(x = log, file = log_file, append = TRUE)
  }
})

observe({
  log <- c(
    glue::glue("{user_id} : protocol : {isolate(cdata$url_protocol)}"),
    glue::glue("{user_id} : hostname : {isolate(cdata$url_hostname)}"),
    glue::glue("{user_id} : pathname : {isolate(cdata$url_pathname)}"),
    glue::glue("{user_id} : port : {isolate(cdata$url_port)}"),
    glue::glue("{user_id} : pixelratio : {isolate(cdata$pixelratio)}")
  )
  write(x = log, file = log_file, append = TRUE)
})

# Log user counts ---------------------------------------------------------

counter_file <- file.path(config$logs, "counter.log")

local({
  counter <- glue::glue("{Sys.time()} {user_id}")
  if (!file.exists(counter_file)) {
    write(x = counter, file = counter_file)
  } else {
    write(x = counter, file = counter_file, append = TRUE)
  }
})


# Log analysis ------------------------------------------------------------

user_logs <- list(
  "tcga_snv" = "tcga_snv.log",
  "tcga_cnv" = "tcga_cnv.log",
  "gene_set" = "gene_set.log",
  "tcga_expr" = "tcga_expr.log"
) 

user_logs %>%
  tibble::enframe() %>%
  tidyr::unnest() %>% 
  purrr::pwalk(
    .f = function(name, value) {
      .log_file <- file.path(user_dir, value)
      .log <- glue::glue("{paste0(rep('-', 10), collapse = '')} User : {user_id} @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}")
      
      if (!file.exists(.log_file)) {
        write(x = .log, file = .log_file)
      } else {
        write(x = .log, file = .log_file, append = TRUE)
      }
    }
  )

# Time events -------------------------------------------------------------

time <- reactiveValues(
  "start_gene_set" = Sys.time(),
  "end_gene_set" = Sys.time(),
  "start_tcga_expr" = Sys.time(),
  "end_tcga_expr" = Sys.time()
)


# Info files --------------------------------------------------------------

info_files <- list(
  "gene_set" = file.path(user_dir, "gene_set.info"),
  "tcga_analysis" = file.path(user_dir, "tcga_analysis.info"),
  "gtex_analysis" = file.path(user_dir, "gtex_analysis.info"),
  "drug" = file.path(user_dir, "drug.info")
)

local({
  info <- c("progress;0", "info;")
  info_files %>%
    purrr::walk(
      .f = function(x) {
        write(info, x)
      }
    )
})


# Status and error --------------------------------------------------------

progress <- reactiveValues(
  "expr_loading" = FALSE,
  "expr_calc" = FALSE,
  "progress_end" = FALSE 
)


processing <- reactiveValues(
  "expr_loading_start" = FALSE,
  "expr_loading_end" = FALSE,
  
  "expr_calc_start" = FALSE,
  "expr_calc_end" = FALSE
)


status <- reactiveValues(
  "gene_set" = FALSE,
  "analysis" = FALSE,
  "tcga_expr" = FALSE,
  "trigger" = FALSE,
  "cnv_submit" = FALSE,
  "snv_submit" = FALSE,
  "meth_submit" = FALSE,
  "rppa_submit" = FALSE,
  "gtex_expr_submit" = FALSE
)

error <- reactiveValues(
  "gene_set" = "",
  "tcga_expr" = "",
  "gtex_expr" = ""
)


# Poll handle -------------------------------------------------------------

info_trigger_gene_set <- function() {
  .x <- scan(info_files$gene_set, what = "", sep = "\n", n = 1, quiet = TRUE)
  .xlist <- strsplit(.x, split = ";", fixed = TRUE)
  return(.xlist[[1]][-1])
}

info_read_gene_set <- function() {
  .x <- scan(info_files$gene_set, what = "", sep = "\n", n = 2, quiet = TRUE)
  .xlist <- strsplit(.x, split = ";", fixed = TRUE)
  return(list("progress" = as.numeric(.xlist[[1]][-1]), "info" = .xlist[[2]][-1]))
}


# Gene sets ---------------------------------------------------------------
gene_set <- reactiveValues(
  match = "",
  non_match = "",
  n_match = "",
  n_non_match = "",
  n_total = ""
)

# Load gene list ----------------------------------------------------------

print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading symbol @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
total_gene_symbol <- readr::read_rds(file.path(config$database, "01_gene_symbol.rds.gz"))
paired_cancer_types <- readr::read_rds(file.path(config$database, "TCGA", "expr", "paired_cancer_types.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading symbol @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))


# Global load data --------------------------------------------------------

expr <- NULL








