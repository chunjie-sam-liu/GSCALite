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


# load snv data -----------------------------------------------------------

load_data_snv <- function() {
  if (is.null(mc3_pass)) {
    # load snv data  ----------------------------------------------------------
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start Load snv data@ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))
    snv <<- readr::read_rds(file.path(config$database, "TCGA", "snv", ".rds_snv_all_gene_snv_count.rds.gz"))
    mc3_pass <<- readr::read_rds(file.path(config$database, "TCGA", "snv", "01-snv_mutation_mc3_public.pass.filtered_maf.rds.gz"))
    snv_survival <<- readr::read_rds(file.path(config$database, "TCGA", "snv", "pancan32_snv_survival_genelist_sig_pval.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} end Load snv data @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))
  }
}

# load cnv data --------------------------------------------------------------
load_data_cnv <- function() {
  # load cnv percent
  if (is.null(cnv_raw)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    cnv <<- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_cnv_percent.rds.gz"))
    cnv_raw <<- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_cnv_threshold.rds.gz"))
    cnv_cor <<- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_all_gene_exp-cor-cnv.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading cnv data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}


# load methylation data ---------------------------------------------------

load_data_meth <- function() {
  # load cnv percent
  if (is.null(meth_diff)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading methy diff data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    meth_diff <<- readr::read_rds(file.path(config$database, "TCGA", "meth", "pan33_allgene_methy_diff.simplification.rds.gz"))
    meth_survival <<- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan32_meth_survival_genelist_sig_pval0.05.rds.gz"))
    meth_cor <<- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan34_all_gene_exp-cor-meth.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading methy cor to expression data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}


# load miRNA data ---------------------------------------------------------
load_data_mirna <- function() {
  # load cnv percent
  if (is.null(mirna2target)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start Load mirna data @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))
    mirna2target <<- readr::read_rds(file.path(config$database, "TCGA", "mirna", "pan_overall_gene_cor-0.5_with_mirna.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} end Load mirna data @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))
  }
}

# load pathway data ----------------------------------------------------------
load_data_rppa <- function() {
  # load cnv percent
  if (is.null(rppa_per)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading rppa percent data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    rppa_per <<- readr::read_rds(file.path(config$database, "TCGA", "rppa", "pan32_gene_activate.inhibit_pathway_percent.rds.gz"))
    rppa_relation <<- readr::read_rds(file.path(config$database, "TCGA", "rppa", "pan32_gene_A-I-N_sig_pval_class.siplification.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading rppa regulate data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}


# Load drug data ----------------------------------------------------------


# load GTEx eqtl data -----------------------------------------------------

load_data_eqtl <- function(){
  if (is.null(GTEx_egene)) {
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading GTEx eqtl data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    GTEx_egene <<- readr::read_rds(file.path(config$database, "GTEx", "eqtl", "GTEx_egene.merged.tissue.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} end loading GTEx eqtl data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}

# load GTEx expr data -----------------------------------------------------
load_data_gexp <- function(){
  if (is.null(gtex_expr_mean)) {
    # load GTEx expression data ---------------------------------------------------------
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} start loading GTEx data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
    #      gtex_expr <- readr::read_rds(file.path(config$database, "GTEx", "expression", "gtex_gene_tmp_annotation_phenotype_v7.rds.gz"))
    gtex_expr_mean <<- readr::read_rds(file.path(config$database, "GTEx", "expression", "gtex_gene_mean_exp.rds.gz"))
    print(glue::glue("{paste0(rep('-', 10), collapse = '')} end loading GTEx data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
  }
}

