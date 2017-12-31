# sourced by 'server.R'
# save as 'tcga_meth_server.R'
# server elements 'tcga_meth' sub tab of 'tcga' tab


# load methy data ---------------------------------------------------------
meth_diff <- NULL
meth_survival <- NULL
meth_cor <- NULL


#  get cancer type --------------------------------------------------------

meth_cancer_type <- callModule(cancerType, "meth")

# Cancer types value box selection ----------------------------------------

callModule(module = cancerTypesSelect, id = "meth", .sctps = meth_cancer_type)
# Check box ---------------------------------------------------------------

callModule(module = selectAndAnalysis, id = "meth", .id = "meth")

# button control --------------------------------------------------------

# submit cancer type -------------------------------------------------------

meth_submit_analysis <- function(input, output, session){
  observeEvent(input$submit, {
    status$meth_submit <- TRUE
    print(status$meth_submit)
  })
}

callModule(meth_submit_analysis,"meth")

# analysis core -----------------------------------------------------------
# monitor for gene list change-----------------------------------
meth_gene_list <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      status$meth_submit <- TRUE
      shinyjs::disable(id = "meth-submit")
      shinyjs::disable(id = "meth-switch")
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
      if(length(meth_gene_list())!=0){
      .msg <- c("NOTICE: ")
      # load data----
      load_data_meth()
      
      # remove pic result generate before ----
      # callModule(removePic,"meth_diff",outtype="plot")
      # callModule(removePic,"meth_survival",outtype="plot")
      # callModule(removePic,"meth_exp",outtype="plot")
      
      print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start methy part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      
      # get gene set meth ----------------------------------------
      meth_diff %>%
        dplyr::mutate(filter_methyDiff = purrr::map(methy_comparison, filter_gene_list, gene_list = meth_gene_list())) %>% 
        dplyr::select(-methy_comparison) %>%
        dplyr::filter(cancer_types %in% meth_cancer_type()) -> gene_list_meth_diff
      
      meth_survival %>%
        dplyr::mutate(filter_SurDiff = purrr::map(diff_pval, filter_gene_list, gene_list = meth_gene_list())) %>%
        dplyr::select(-diff_pval) -> gene_list_meth_sur
      
      meth_cor %>%
        dplyr::mutate(filter_cor = purrr::map(spm, filter_gene_list, gene_list = meth_gene_list())) %>%
        dplyr::select(-spm) -> gene_list_meth_cor
      
      # get cancer type meth ----------------------------------------
      if(nrow(gene_list_meth_diff)>0){
      gene_list_meth_diff %>%
        tidyr::unnest() %>%
        tidyr::drop_na() -> gene_list_cancer_methdiff
      
      
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
      } else{
        .msg <- paste(.msg, glue::glue("The [Differential Methylation] analysis based on paired sample in each cancer types.
In this analysis, only {nrow(meth_diff)} cancer types have paired samples. They are {paste0(meth_diff$cancer_types, collapse = ', ')}."),sep=" ")
      }
      # meth survival point ----
      gene_list_meth_sur %>%
        dplyr::filter(cancer_types %in% meth_cancer_type()) %>%
        tidyr::unnest() %>%
        tidyr::drop_na() -> gene_list_cancer_methsur

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
      gene_list_meth_cor %>%
        dplyr::filter(cancer_types %in% meth_cancer_type()) %>%
        tidyr::unnest() %>%
        tidyr::drop_na() -> gene_list_cancer_methcor
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
      
      .msg <- paste(.msg,glue::glue("Since we just show significant results, so a small size of gene and cancer set may cause no significant result in some plots, if it happens, try more genes and cancer types."),sep=" ")
      # alert for information
      shinyBS::createAlert(
        session = session, anchorId = "meth-no_gene_set", title = "Information", style = "info",
        content = .msg, append = FALSE
      )
      status$meth_submit <- FALSE
      shinyjs::enable("meth-submit")
      }else{
        shinyBS::createAlert(
          session = session, anchorId = "meth-no_gene_set", title = "Oops",
          content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
        )
      }
    }
  }
)


# monitors -------------------------------------------------------
observe(meth_gene_list())
observe(meth_analysis())
