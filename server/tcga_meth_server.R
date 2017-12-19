# sourced by 'server.R'
# save as 'tcga_meth_server.R'
# server elements 'tcga_meth' sub tab of 'tcga' tab


# load methy data ---------------------------------------------------------
# diff methylation between tumor and normal
meth_diff <- readr::read_rds(file.path(config$database, "TCGA", "meth", "pan33_allgene_methy_diff.rds.gz"))

# genes' survival diffenence hypermethylation and hypomethylation
meth_survival <- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan32_meth_survival_genelist_sig_pval.rds.gz"))

# genes' expression correlate with methylation
meth_cor <- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan34_all_gene_exp-cor-meth.rds.gz"))

#  get cancer type --------------------------------------------------------

meth_cancer_type <- callModule(cancerType, "meth")
output$meth_selected_cancer <- renderText(
  meth_cancer_type()
)
####### PS: reset need to do more.
observeEvent(input$meth_reset, {
  output$meth_selected_cancer <- shiny::renderText({
    ""
  })
})

# analysis core -----------------------------------------------------------

# get gene set meth ----
meth_diff %>%
  dplyr::mutate(filter_methyDiff = purrr::map(methy_comparison, filter_gene_list, gene_list = gene_list)) %>%
  dplyr::select(-methy_comparison) -> gene_list_meth_diff

meth_survival %>%
  dplyr::mutate(filter_SurDiff = purrr::map(diff_pval, filter_gene_list, gene_list = gene_list)) %>%
  dplyr::select(-diff_pval) -> gene_list_meth_sur

meth_cor %>%
  dplyr::mutate(filter_cor = purrr::map(cnv_exp, filter_gene_list, gene_list = gene_list)) %>%
  dplyr::select(-cnv_exp) -> gene_list_meth_cor

# reset cancer type -------------------------------------------------------

observeEvent(input$meth_submit, {
  # get cancer type meth ----
  gene_list_meth_diff %>%
    dplyr::filter(cancer_types %in% meth_cancer_type()) %>%
    tidyr::unnest() %>%
    tidyr::drop_na() -> gene_list_cancer_methdiff
  gene_list_meth_sur %>%
    dplyr::filter(cancer_types %in% meth_cancer_type()) %>%
    tidyr::unnest() %>%
    tidyr::drop_na() -> gene_list_cancer_methsur
  gene_list_meth_cor %>%
    dplyr::filter(cancer_types %in% meth_cancer_type()) %>%
    tidyr::unnest() %>%
    tidyr::drop_na() -> gene_list_cancer_methcor


  # ploting -----------------------------------------------------------------
  # meth diff point ----

  # meth survival point ----

  # meth correlate to expression point ----
})