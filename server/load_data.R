# sourced by server.R



# Load expr ---------------------------------------------------------------
load_data_expr <- function() {
  if (is.null(expr)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading expr data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    expr <<- readr::read_rds(file.path(config$database, "TCGA", "expr", "pancan14_expr_fc_pval.rds.gz"))
    expr_survival <<- readr::read_rds(file.path(config$database, "TCGA", "expr", "expr_survival.rds.gz"))
    expr_subtype <<- readr::read_rds(file.path(config$database, "TCGA", "expr", "expr_subtype.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} loading expr data complete @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}


# load cnv data --------------------------------------------------------------
load_data_cnv <- function() {
  # load cnv percent
  if (is.null(cnv)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv percent data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    cnv <<- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_cnv_percent.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} end loading cnv percent data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
  # load cnv raw
  if (is.null(cnv_raw)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv raw data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    cnv_raw <<- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_cnv_threshold.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv raw data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
  if (is.null(cnv_cor)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv cor data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    cnv_cor <<- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_all_gene_exp-cor-cnv.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv cor data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}


# load methylation data ---------------------------------------------------

load_data_meth <- function() {
  # load cnv percent
  if (is.null(meth_diff)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading methy diff data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    meth_diff <<- readr::read_rds(file.path(config$database, "TCGA", "meth", "pan33_allgene_methy_diff.simplification.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading methy diff data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
  # genes' survival diffenence hypermethylation and hypomethylation
  if (is.null(meth_survival)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading methy survival data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    meth_survival <<- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan32_meth_survival_genelist_sig_pval0.05.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading methy survival data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
  # genes' expression correlate with methylation
  if (is.null(meth_cor)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading methy cor to expression data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    meth_cor <<- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan34_all_gene_exp-cor-meth.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading methy cor to expression data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}
