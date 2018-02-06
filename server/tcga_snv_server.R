# sourced by 'server.R'
# save as 'tcga_snv_server.R'
# server elements 'tcga_snv' sub tab of 'tcga' tab

source(file.path(config$wd, "functions", "tcga_snv_function.R"))


#  get cancer type --------------------------------------------------------
# selected_ctyps <- callModule(cancerType, "snv")



# button control --------------------------------------------------------

# submit cancer type -------------------------------------------------------

# snv_submit_analysis <- function(input, output, session){
#   observeEvent(input$submit, {
#     status$snv_submit <- TRUE
#     print(status$snv_submit)
#   })
# }
#
# callModule(snv_submit_analysis,"snv")

# analysis core -----------------------------------------------------------
# monitor for gene list change-----------------------------------
# snv_gene_list <- eventReactive(
#   eventExpr = status$analysis,
#   ignoreNULL = TRUE,
#   valueExpr = {
#     # be sure the following code run after start analysis
#     if (status$analysis == TRUE) {
#       status$snv_submit <- TRUE
#       shinyjs::disable(id = "snv-submit")
#       shinyjs::disable(id = "snv-switch")
#       as.character(gene_set$match)
#     }
#   }
# )

# snv result out ui -------------------------------------------------------

output$ui_snv_result <- shiny::renderUI({
  fn_snv_result(selected_analysis$snv)
})

# get gene set snv --------------------------------------------------------
snv_analysis <- eventReactive(
  {
    eventExpr <- status$analysis
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$analysis == TRUE) {
      # laod snv data ----
      if (selected_analysis$snv == TRUE) {
        load_data_snv()
        # Cancer types value box selection ----------------------------------------

        callModule(module = cancerTypesSelect, id = "snv", .sctps = intersect(selected_ctyps(), tcga_data))
        # Check box ---------------------------------------------------------------

        callModule(module = selectAndAnalysis, id = "snv", .id = "snv")
        if (length(gene_set$match) != 0) {
          if (length(selected_ctyps() != 0)) {
            .msg <- c("NOTICE: ")

            print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start snv part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
            
            # cancer overlap
            cancer_in_tcga_data_snv <- intersect(selected_ctyps(),tcga_data)

            # snv percent -------------------------------------------------------------
            snv %>%
              dplyr::mutate(filter_snv = purrr::map(mut_count, filter_gene_list, gene_list = gene_set$match)) %>%
              dplyr::select(-mut_count) %>%
              dplyr::filter(cancer_types %in% selected_ctyps()) -> gene_list_cancer_snv

            # plot out ----------------------------------------------------------------

            # snv percentage ----------------------------------------------------------
            if (nrow(gene_list_cancer_snv) > 0) {
              gene_list_cancer_snv %>%
                tidyr::unnest(filter_snv) ->gene_list_cancer_snv
              if(nrow(gene_list_cancer_snv)>0){
                gene_list_cancer_snv %>%
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
                  dplyr::arrange(s) -> snv_per_gene_rank
                
                callModule(
                  snv_per_heatmap, "snv_percentage", data = snv_per_plot_ready,
                  cancer = "x_label", gene = "symbol", fill = "per", label = "sm_count",
                  cancer_rank = snv_per_cancer_rank, gene_rank = snv_per_gene_rank, status_monitor = "analysis", status,
                  downloadname = "SNV_percentage_profile"
                )
                .msg_snv_percentage <- NULL
              } else {
                .msg_snv_percentage <- paste(glue::glue("No significant [SNV percentage profile] result of gene: {paste0(gene_set$match, collapse = ',')} in your selected cancer types: {paste0(cancer_in_tcga_data_snv, collapse=", ")}. Please try other cancers or genes."), sep = " ")
                output[["snv_percentage-plot"]] <- renderPlot({
                  NULL
                })
              }
            } else {
              .msg_snv_percentage <- paste(glue::glue("No significant [SNV percentage profile] result of gene: {paste0(gene_set$match, collapse = ',')} in your selected cancer types: {paste0(cancer_in_tcga_data_snv, collapse=", ")}. Please try other cancers or genes."), sep = " ")
              output[["snv_percentage-plot"]] <- renderPlot({
                NULL
              })
            }
            # snv survival ------------------------------------------------------------
            snv_survival %>%
              dplyr::mutate(filter_survival = purrr::map(diff_pval, filter_gene_list, gene_list = gene_set$match)) %>%
              dplyr::select(-diff_pval) %>%
              dplyr::filter(cancer_types %in% selected_ctyps()) %>%
              tidyr::unnest() %>%
              dplyr::mutate(logP = -log10(logRankP)) %>%
              dplyr::mutate(logP = ifelse(logP > 15, 15, logP)) %>%
              dplyr::mutate(logP = ifelse(logP < -log10(0.05), NA, logP)) %>%
              tidyr::drop_na() -> snv_sur_plot_ready -> snv_sur_plot_ready
            # survival ----------------------------------------------------------------
            if (nrow(snv_sur_plot_ready) > 0) {
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
                gene_rank = snv_sur_gene_rank, sizename = "logRank P", colorname = "Mutation Worse", title = "Overall survival difference between mutation and non mutation genes.", status_monitor = "analysis", status, downloadname = "SNV_affect_survival"
              )
              .msg_snv_survival <- NULL
            } else {
              .msg_snv_survival <- paste(glue::glue("No significant [SNV survival] result of gene: {paste0(gene_set$match, collapse = ', ')} in your selected cancer type: {paste0(cancer_in_tcga_data_snv, collapse=", ")}. Please try other cancers or genes."), sep = " ")
              output[["snv_survival-plot"]] <- renderPlot({
                NULL
              })
            }


            .msg <- paste(.msg, glue::glue("Please be patient, need some time to draw pictrue. Since we just show significant results, so a small size of gene and cancer set may cause no significant result in some plots. If it happens, try more genes and cancer types."), sep = " ")
            # alert for information
            shinyBS::createAlert(
              session = session, anchorId = "snv-no_gene_set", title = "Information", style = "info",
              content = .msg, append = FALSE
            )

            # maf ---------------------------------------------------------------------
            if (length(gene_set$match) >= 2) {
              snv_InpSel <- paste0(selected_ctyps(), collapse = "','")
              query <- as.expression(paste0("Cancer_Types %in% c('", snv_InpSel, "')"))
              # my_subsetMaf(mc3_pass, genes = gene_set$match, mafObj = T,query = query) -> gene_list_maf #
              gene_set_in_maf <- intersect(maf_gene_all,gene_set$match) %>% length()
              cancer_in_maf <- intersect(maf_cancer_all,selected_ctyps()) 
              cancer_noin_maf <- setdiff(selected_ctyps(),maf_cancer_all)
              
              if(length(cancer_noin_maf)>0){
                cancer_no_data.msg <- glue::glue(" {paste0(cancer_noin_maf, collapse=",")} don't have data in this analysis.")
              } else {cancer_no_data.msg <- NULL}
              
              if(gene_set_in_maf>=2 && length(cancer_in_maf)>0){
                maftools::subsetMaf(mc3_pass, genes = gene_set$match, mafObj = T, query = query) -> gene_list_maf
                # 1. snv summary
                snv_su_out <- file.path(user_dir, "pngs", paste(user_id, "-SNV_summary_profile.png", sep = ""))
                callModule(snv_maf_summaryPlot, "snv_summary", gene_list_maf = gene_list_maf, outfile = snv_su_out, status_monitor = "analysis", status, downloadname = "SNV_summary")

                # 2. oncoplot
                snv_onco_out <- file.path(user_dir, "pngs", paste(user_id, "-SNV_oncoplot_profile.png", sep = ""))
                callModule(snv_maf_oncoPlot, "snv_oncoplot", gene_list_maf = gene_list_maf, pancan_color = pancan_color, outfile = snv_onco_out, status_monitor = "analysis", status, downloadname = "SNV_oncoplot")
                .msg_snv_oncoplot <- cancer_no_data.msg
                .msg_snv_summary <- cancer_no_data.msg
              } else {
                .msg_snv_oncoplot <- paste(glue::glue("Your selected genes: {paste0(gene_set$match, collapse = ', ')} are not mutate in your selected cancer type: {paste0(cancer_in_tcga_data_snv, collapse = ', ')}.{cancer_no_data.msg} Please try other cancers or genes."), sep = " ")
                .msg_snv_summary <- paste(glue::glue("Your selected genes: {paste0(gene_set$match, collapse = ', ')} are not mutate in your selected cancer type: {paste0(cancer_in_tcga_data_snv, collapse = ', ')}.{cancer_no_data.msg} Please try other cancers or genes."), sep = " ")

                callModule(white_plot, "snv_oncoplot", status_monitor = "analysis", status = status, outfile = file.path(user_dir, "pngs", paste(user_id, "-white_2.png", sep = "")))
                callModule(white_plot, "snv_summary", status_monitor = "analysis", status = status, outfile = file.path(user_dir, "pngs", paste(user_id, "-white_1.png", sep = "")))
              }
            } else {
              .msg_snv_oncoplot <- "Cannot create SNV oncoplot for single gene. Minimum two genes required ! "
              .msg_snv_summary <- "Cannot create SNV summary plot for single gene. Minimum two genes required ! "
              callModule(white_plot, "snv_oncoplot", status_monitor = "analysis", status = status, outfile = file.path(user_dir, "pngs", paste(user_id, "-white_2.png", sep = "")))
              callModule(white_plot, "snv_summary", status_monitor = "analysis", status = status, outfile = file.path(user_dir, "pngs", paste(user_id, "-white_1.png", sep = "")))
            }

            print(glue::glue("{paste0(rep('-', 10), collapse = '')} End maf part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))

            # infomation UI for each part
            output[["snv_percentage-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_snv_percentage, style = "color:#CD3700")
              )
            })
            output[["snv_summary-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_snv_summary, style = "color:#CD3700")
              )
            })
            output[["snv_oncoplot-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_snv_oncoplot, style = "color:#CD3700")
              )
            })
            output[["snv_survival-massage"]] <- renderUI({
              tagList(
                shiny::tags$p(.msg_snv_survival, style = "color:#CD3700")
              )
            })
          } else {
            shinyBS::createAlert(
              session = session, anchorId = "snv-no_cancer_set", title = "Oops",
              content = "No cancer selected! Please select at least one cancer type.", style = "danger", append = FALSE
            )
          }
        } else {
          shinyBS::createAlert(
            session = session, anchorId = "snv-no_gene_set", title = "Oops",
            content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
          )
        }
      }
    }
  }
)

# monitors -------------------------------------------------------
# observe(snv_global_analysis())
observe(snv_analysis())