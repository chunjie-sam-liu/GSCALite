# sourced by 'server.R'
# save as 'tcga_snv_server.R'
# server elements 'tcga_snv' sub tab of 'tcga' tab

# load snv data  ----------------------------------------------------------
snv <- readr::read_rds(file.path(config$database, "TCGA", "snv", ".rds_snv_all_gene_snv_count.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} Load snv @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))

mc3_pass <- readr::read_rds(file.path(config$database, "TCGA", "snv", "snv_mutation_mc3_public.pass.filtered_maf.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} Load mc3_pass @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))

# mc3_maf <- readr::read_rds(file.path(config$database,"TCGA","snv","snv_mutation_mc3_public.pass.filtered_maf.rds.gz"))

snv_survival <- readr::read_rds(file.path(config$database, "TCGA", "snv", "pancan32_snv_survival_genelist_sig_pval.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} Load snv_survival @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))


#  get cancer type --------------------------------------------------------
snv_cancer_type <- callModule(cancerType, "snv")

output$snv_selected_cancer <- renderText({
  snv_cancer_type()
  
})
observeEvent(input$cnv_reset, {
  snv_cancer_type<-callModule(resetcancerType,"snv")
})



# analysis core -----------------------------------------------------------

# get gene set snv --------------------------------------------------------
snv %>%
  dplyr::mutate(filter_snv = purrr::map(mut_count, filter_gene_list, gene_list = gene_list)) %>%
  dplyr::select(-mut_count) -> gene_list_snv

snv_survival %>%
  dplyr::mutate(filter_survival = purrr::map(survival, filter_gene_list, gene_list = gene_list)) %>%
  dplyr::select(-survival) -> gene_list_snv_survival

snv_ct<-observeEvent(input$snv_submit, {
  snv_ct<- snv_cancer_type()
  print(snv_ct)
  # get snv percent ---------------------------------------------------------
  # get cancer type snv ----
  gene_list_snv %>%
    dplyr::filter(cancer_types %in% snv_ct) -> gene_list_cancer_snv
  # gene_list_snv %>%
  #   dplyr::filter(cancer_types %in% isolate(cancer_type()) ) -> gene_list_cancer_snv
  gene_list_snv_survival %>%
    dplyr::filter(cancer_types %in% snv_ct) -> gene_list_cancer_survival


  # plot out ----------------------------------------------------------------

  # snv percentage ----------------------------------------------------------
  gene_list_cancer_snv %>%
    tidyr::unnest(filter_snv) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(x_label = paste(cancer_types, " (n=", n, ")", sep = "")) %>%
    dplyr::mutate(sm_count = ifelse(sm_count > 0, sm_count, NA)) %>%
    dplyr::mutate(per = ifelse(per > 0.02, per, 0)) -> snv_per_plot_ready
  snv_per_plot_ready %>%
    dplyr::group_by(x_label) %>%
    dplyr::summarise(s = sum(per)) %>%
    dplyr::arrange(dplyr::desc(s)) -> snv_per_cancer_rank
  snv_per_plot_ready %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise(s = sum(sm_count)) %>%
    dplyr::left_join(gene_list, by = "symbol") %>%
    dplyr::arrange(s) -> snv_per_gene_rank

  callModule(
    snv_per_heatmap, "snv_percentage", data = snv_per_plot_ready,
    cancer = "x_label", gene = "symbol", fill = "per", label = "sm_count",
    cancer_rank = snv_per_cancer_rank, gene_rank = snv_per_gene_rank
  )



  # maf ---------------------------------------------------------------------

  # 1. summary -----------------------------------------------------------------
  #reactiveValues(snv_ct= "bar")
  #snv_cancer_type <-reactiveValues(x="KICH")
  # snv_ct <- snv_cancer_type() %>% as.character()
  print(snv_ct)

  # test<-reactiveValues(snv_ct = c("KICH", "KIRP"))
  # quote(test$snv_ct)
  query <- as.expression(quote("cancer_types %in% snv_ct"))

  print(query)
  maftools::subsetMaf(mc3_pass, genes = gene_list %>% dplyr::pull(symbol), query = query, mafObj = T) -> gene_list_maf

  # maftools::subsetMaf(mc3_pass,genes = gene_list %>% dplyr::pull(symbol),query = "cancer_types %in% isolate(cancer_type())",mafObj = T) -> gene_list_maf
  callModule(snv_maf_summaryPlot,"snv_summary",gene_list_maf=gene_list_maf,figname="snv_summary")


  # 2. oncoplot ----------------------------------------------------------------
  callModule(snv_maf_oncoPlot,"snv_oncoplot",gene_list_maf=gene_list_maf,figname="snv_oncoplot",cancer_type=snv_ct)

  # survival ----------------------------------------------------------------
  gene_list_snv_survival %>%
    tidyr::unnest(filter_survival) %>%
    # tidyr::drop_na() %>%
    dplyr::mutate(logP = -log10(logRankP)) %>%
    dplyr::mutate(logP = ifelse(logP > 15, 15, logP)) -> snv_sur_plot_ready

  snv_sur_plot_ready %>%
    dplyr::mutate(s = ifelse(estimate > 0, 1, -1)) %>%
    dplyr::mutate(s = ifelse(logRankP > 0.05, 0, s)) %>%
    dplyr::group_by(cancer_types) %>%
    dplyr::summarise(r = sum(s)) %>%
    dplyr::arrange(dplyr::desc(r)) -> snv_sur_cancer_rank

  snv_sur_plot_ready %>%
    dplyr::mutate(s = ifelse(estimate > 0, 1, -1)) %>%
    dplyr::mutate(s = ifelse(logRankP > 0.05, 0, s)) %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise(r = sum(s)) %>%
    dplyr::arrange(dplyr::desc(r)) -> snv_sur_gene_rank

  callModule(
    snv_sur_pointPlot, "snv_survival", data = snv_sur_plot_ready, cancer = "cancer_types",
    gene = "symbol", size = "logP", color = "worse", cancer_rank = snv_sur_cancer_rank,
    gene_rank = snv_sur_gene_rank, sizename = "-Log10(P)", colorname = "Mutation Worse",title="Overall survival difference between mutation and non mutation genes."
  )
  
})