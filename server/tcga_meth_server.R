# sourced by 'server.R'
# save as 'tcga_meth_server.R'
# server elements 'tcga_meth' sub tab of 'tcga' tab


# load methy data ---------------------------------------------------------
# diff methylation between tumor and normal
print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading methy diff data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
meth_diff <- readr::read_rds(file.path(config$database, "TCGA", "meth", "pan33_allgene_methy_diff.simplification.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading methy diff data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

# genes' survival diffenence hypermethylation and hypomethylation
print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading methy survival data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
meth_survival <- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan32_meth_survival_genelist_sig_pval0.05.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading methy survival data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

# genes' expression correlate with methylation
print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start loading methy cor to expression data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
meth_cor <- readr::read_rds(file.path(config$database, "TCGA", "meth", "pancan34_all_gene_exp-cor-meth.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} End loading methy cor to expression data @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))


#  get cancer type --------------------------------------------------------

meth_cancer_type <- callModule(cancerType, "meth")

output$meth_selected_cancer <- renderText(
  meth_cancer_type()
)

# button control --------------------------------------------------------
#######reset cancer selection when click.
# observeEvent(input$meth_reset, {
#   meth_cancer_type<-callModule(resetcancerType,"meth")
# })

# submit cancer type -------------------------------------------------------
observeEvent(input$meth_submit, {
  status$meth_submit <- TRUE
  shinyjs::disable(id = "meth_submit")
  shinyjs::enable(id = "meth_stop")
})
# stop analysis -----------------------------------------------------------
observeEvent(input$meth_stop, {
  status$meth_submit <- FALSE
  shinyjs::enable(id = "meth_submit")
  meth_hide <- c("meth_diff", "meth_survival", "meth_exp")
  hidePic(meth_hide) # hide pic when stop clicked!
})

# monitor for gene list change-----------------------------------
meth_gene_list <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      status$meth_submit <- TRUE
      shinyjs::disable(id = "meth_submit")
      shinyjs::enable(id = "meth_stop")
      as.character(gene_set$match)
    }
  }
)
  
# analysis core -----------------------------------------------------------
meth_analysis <- eventReactive(
  {
    status$meth_submit == TRUE
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$meth_submit == TRUE) {
      print(paste("Methy gene",meth_gene_list()))
      
      print(paste("Methy cancers",meth_cancer_type()))
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start methy part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      
      # get gene set meth ----------------------------------------
      meth_diff %>%
        dplyr::mutate(filter_methyDiff = purrr::map(methy_comparison, filter_gene_list, gene_list = meth_gene_list())) %>%
        dplyr::select(-methy_comparison) -> gene_list_meth_diff
      
      meth_survival %>%
        dplyr::mutate(filter_SurDiff = purrr::map(diff_pval, filter_gene_list, gene_list = meth_gene_list())) %>%
        dplyr::select(-diff_pval) -> gene_list_meth_sur
      
      meth_cor %>%
        dplyr::mutate(filter_cor = purrr::map(spm, filter_gene_list, gene_list = meth_gene_list())) %>%
        dplyr::select(-spm) -> gene_list_meth_cor
      
      # get cancer type meth ----------------------------------------
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
      gene_list_cancer_methdiff %>%
        dplyr::group_by(symbol) %>%
        dplyr::summarise(rank=sum(diff)) %>%
        dplyr::arrange(rank) ->gene_rank.methdiff
      gene_list_cancer_methdiff %>%
        dplyr::group_by(cancer_types) %>%
        dplyr::summarise(rank=sum(diff)) %>%
        dplyr::arrange(rank) ->cancer_rank.methdiff
      
      callModule(methy_diff_pointPlot,"meth_diff",data=gene_list_cancer_methdiff, cancer="cancer_types", gene="symbol", size="fdr", color="diff", cancer_rank=cancer_rank.methdiff,gene_rank=gene_rank.methdiff,sizename="-Log10(FDR)", colorname="Methylation diff (T - N)",title="Methylation difference between tumor and normal samples.")
      
      # meth survival point ----
      gene_list_cancer_methsur %>%
        dplyr::mutate(a=ifelse(Hyper_worse=="Low",-1,1)) %>%
        dplyr::group_by(symbol) %>%
        dplyr::summarise(rank=sum(a)) %>%
        dplyr::arrange(rank) ->gene_rank.methsur
      
      gene_list_cancer_methsur %>%
        dplyr::mutate(a=ifelse(Hyper_worse=="Low",-1,1)) %>%
        dplyr::group_by(cancer_types) %>%
        dplyr::summarise(rank=sum(a)) %>%
        dplyr::arrange(rank) ->cancer_rank.methsur
      
      callModule(snv_sur_pointPlot,"meth_survival", data=gene_list_cancer_methsur, cancer="cancer_types", gene="symbol", size="log10logrankP", color="Hyper_worse", cancer_rank=cancer_rank.methsur,gene_rank=gene_rank.methsur,sizename="logRank Pvalue", colorname="HyperMethy Worse", title="Overall survival difference between hypermethylation and hypomethylation.")
      
      # meth correlate to expression point ----
      gene_list_cancer_methcor %>%
        dplyr::group_by(symbol) %>%
        dplyr::summarise(rank=sum(spm)) %>%
        dplyr::arrange(rank) ->gene_rank.methcor
      
      gene_list_cancer_methcor %>%
        dplyr::group_by(cancer_types) %>%
        dplyr::summarise(rank=sum(spm)) %>%
        dplyr::arrange(rank) ->cancer_rank.methcor
      
      callModule(methy_diff_pointPlot,"meth_exp", data=gene_list_cancer_methcor, cancer="cancer_types", gene="symbol", size="logfdr", color="spm", cancer_rank=cancer_rank.methcor,gene_rank=gene_rank.methcor,sizename="-Log10(P.value)", colorname="Spearman Correlation Coefficient", title="Spearman Correlation Coefficient of methylation and gene expression.")
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} End methy part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      meth_show <- c("meth_diff", "meth_survival", "meth_exp")
      showPic(meth_show) # hide pic when stop clicked!
    }
  }
)


# monitors -------------------------------------------------------
observe(meth_gene_list())
observe(meth_analysis())
