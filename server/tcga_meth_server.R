# sourced by 'server.R'
# save as 'tcga_meth_server.R'
# server elements 'tcga_meth' sub tab of 'tcga' tab

source(file.path(config$wd, "functions", "tcga_meth_function.R"))

#  get cancer type --------------------------------------------------------

# meth_cancer_type <- callModule(cancerType, "meth")




# analysis core -----------------------------------------------------------
# generate meth result out ui -------------------------------------------------------

output$ui_meth_result <- shiny::renderUI({
  fn_meth_result(selected_analysis$meth)
})

# analysis core -----------------------------------------------------------
meth_analysis <- eventReactive(
  {
    status$analysis == TRUE
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      if (selected_analysis$meth == TRUE) {
        # Cancer types value box selection ----------------------------------------

        callModule(module = cancerTypesSelect, id = "meth", .sctps = intersect(selected_ctyps(), tcga_data))
        # Check box ---------------------------------------------------------------

        callModule(module = selectAndAnalysis, id = "meth", .id = "meth")
        load_data_meth()
        if (length(gene_set$match) != 0) {
          .msg <- c("NOTICE: ")
          # load data----
          load_data_meth()
          
          # cancer overlap
          cancer_in_tcga_data_meth <- intersect(selected_ctyps(),tcga_data)

          print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start methy part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

          # get gene set meth ----------------------------------------
          meth_diff %>%
            dplyr::mutate(filter_methyDiff = purrr::map(methy_comparison, filter_gene_list, gene_list = gene_set$match)) %>%
            dplyr::select(-methy_comparison) %>%
            dplyr::filter(cancer_types %in% selected_ctyps()) -> gene_list_meth_diff

          meth_survival %>%
            dplyr::mutate(filter_SurDiff = purrr::map(diff_pval, filter_gene_list, gene_list = gene_set$match)) %>%
            dplyr::select(-diff_pval) -> gene_list_meth_sur

          meth_cor %>%
            dplyr::mutate(filter_cor = purrr::map(spm, filter_gene_list, gene_list = gene_set$match)) %>%
            dplyr::select(-spm) -> gene_list_meth_cor

          # get cancer type meth ----------------------------------------
          if (nrow(gene_list_meth_diff) > 0) {
            gene_list_meth_diff %>%
              tidyr::unnest() %>%
              tidyr::drop_na() -> gene_list_cancer_methdiff


            # ploting -----------------------------------------------------------------
            # meth diff point ----
            gene_list_cancer_methdiff %>%
              dplyr::group_by(symbol) %>%
              dplyr::summarise(rank = sum(diff)) %>%
              dplyr::arrange(rank) -> gene_rank.methdiff
            gene_list_cancer_methdiff %>%
              dplyr::group_by(cancer_types) %>%
              dplyr::summarise(rank = sum(diff)) %>%
              dplyr::arrange(rank) -> cancer_rank.methdiff

            callModule(methy_diff_pointPlot, "meth_diff", data = gene_list_cancer_methdiff, cancer = "cancer_types", gene = "symbol", size = "fdr", color = "diff", cancer_rank = cancer_rank.methdiff, gene_rank = gene_rank.methdiff, sizename = "-Log10(FDR)", colorname = "Methylation diff (T - N)", title = "Methylation difference between tumor and normal samples.", status_monitor = "analysis", status, downloadname="Differential_methylation")
            .msg_meth_diff <- NULL
          } else {
            .msg_meth_diff <- paste(glue::glue("The [Differential Methylation] analysis based on paired sample in each cancer types.
In this analysis, only {nrow(meth_diff)} cancer types have paired samples. They are {paste0(meth_diff$cancer_types, collapse = ', ')}."), sep = " ")
            output[["meth_diff-plot"]] <- renderPlot({NULL})
          }

          # meth survival point ----
          gene_list_meth_sur %>%
            dplyr::filter(cancer_types %in% selected_ctyps()) %>%
            tidyr::unnest() %>%
            tidyr::drop_na() -> gene_list_cancer_methsur
          if (nrow(gene_list_cancer_methsur) > 0) {
            gene_list_cancer_methsur %>%
              dplyr::mutate(a = ifelse(Hyper_worse == "Low", -1, 1)) %>%
              dplyr::group_by(symbol) %>%
              dplyr::summarise(rank = sum(a)) %>%
              dplyr::arrange(rank) -> gene_rank.methsur

            gene_list_cancer_methsur %>%
              dplyr::mutate(a = ifelse(Hyper_worse == "Low", -1, 1)) %>%
              dplyr::group_by(cancer_types) %>%
              dplyr::summarise(rank = sum(a)) %>%
              dplyr::arrange(rank) -> cancer_rank.methsur

            callModule(snv_sur_pointPlot, "meth_survival", data = gene_list_cancer_methsur, cancer = "cancer_types", gene = "symbol", size = "log10logrankP", color = "Hyper_worse", cancer_rank = cancer_rank.methsur, gene_rank = gene_rank.methsur, sizename = "logRank Pvalue", colorname = "HyperMethy Worse", title = "Overall survival difference between hypermethylation and hypomethylation.", status_monitor = "analysis", status, downloadname="Methylation_survival")
            .msg_meth_survival <- NULL
          } else {
            .msg_meth_survival <- paste(.msg, glue::glue("No significant [Methylation Survival] result of gene: {paste0(gene_set$match, collapse = ', ')} in your selected cancer type: {paste0(cancer_in_tcga_data_meth,collapse='', )}. Please try more cancers or more genes."), sep = " ")
            output[["meth_survival-plot"]] <- renderPlot({
              NULL
            })
          }

          # meth correlate to expression point ----
          gene_list_meth_cor %>%
            dplyr::filter(cancer_types %in% selected_ctyps()) %>%
            tidyr::unnest() %>%
            tidyr::drop_na() -> gene_list_cancer_methcor
          
          if (nrow(gene_list_cancer_methcor) > 0) {
            gene_list_cancer_methcor %>%
              dplyr::group_by(symbol) %>%
              dplyr::summarise(rank = sum(spm)) %>%
              dplyr::arrange(rank) -> gene_rank.methcor

            gene_list_cancer_methcor %>%
              dplyr::group_by(cancer_types) %>%
              dplyr::summarise(rank = sum(spm)) %>%
              dplyr::arrange(rank) -> cancer_rank.methcor

            callModule(methy_diff_pointPlot, "meth_exp", data = gene_list_cancer_methcor, cancer = "cancer_types", gene = "symbol", size = "logfdr", color = "spm", cancer_rank = cancer_rank.methcor, gene_rank = gene_rank.methcor, sizename = "-Log10(P.value)", colorname = "Spearman Correlation Coefficient", title = "Spearman Correlation Coefficient of methylation and gene expression.", status_monitor = "analysis", status, downloadname="Methylation_affect_exp")
            .msg_meth_exp <- NULL
          } else {
            .msg_meth_exp <- paste(.msg, glue::glue("No significant [Methylation to Expression] result of gene: {paste0(gene_set$match, collapse = ', ')} in your selected cancer type: {paste0(cancer_in_tcga_data_meth,collapse=', ')}. Please try more cancers or more genes."), sep = " ")
            output[["meth_exp-plot"]] <- renderPlot({
              NULL
            })
          }
          print(glue::glue("{paste0(rep('-', 10), collapse = '')} End methy part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
          
          # infomation UI for each part --------------------------
          output[["meth_diff-massage"]] <- renderUI({
            tagList(
              shiny::tags$p(.msg_meth_diff,style= "color:#CD3700")
            )
          })
          output[["meth_survival-massage"]] <- renderUI({
            tagList(
              shiny::tags$p(.msg_meth_survival,style= "color:#CD3700")
            )
          })
          output[["meth_exp-massage"]] <- renderUI({
            tagList(
              shiny::tags$p(.msg_meth_exp,style= "color:#CD3700")
            )
          })

          .msg <- paste(.msg, glue::glue("Since we just show significant results, so a small size of gene and cancer set may cause no significant result in some plots, if it happens, try more genes and cancer types."), sep = " ")
          # alert for information
          shinyBS::createAlert(
            session = session, anchorId = "meth-no_gene_set", title = "Information", style = "info",
            content = .msg, append = FALSE
          )
        } else {
          shinyBS::createAlert(
            session = session, anchorId = "meth-no_gene_set", title = "Oops",
            content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
          )
        }
      }
    }
  }
)


# monitors -------------------------------------------------------
observe(meth_analysis())