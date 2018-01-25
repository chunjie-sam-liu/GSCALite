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

output$ui_snv_result <- shiny::renderUI({fn_snv_result(selected_analysis$snv)})

# get gene set snv --------------------------------------------------------
snv_analysis <- eventReactive(
  {
    eventExpr = status$analysis
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
        if(length(gene_set$match)!=0){
          if(length(selected_ctyps()!=0)){
            .msg <- c("NOTICE: ")
            
            print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start snv part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
            
            # snv percent -------------------------------------------------------------
            snv %>%
              dplyr::mutate(filter_snv = purrr::map(mut_count, filter_gene_list, gene_list = gene_set$match)) %>%
              dplyr::select(-mut_count) %>%
              dplyr::filter(cancer_types %in%  selected_ctyps()) -> gene_list_cancer_snv
            
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
              dplyr::arrange(s) -> snv_per_gene_rank
            
            callModule(
              snv_per_heatmap, "snv_percentage", data = snv_per_plot_ready,
              cancer = "x_label", gene = "symbol", fill = "per", label = "sm_count",
              cancer_rank = snv_per_cancer_rank, gene_rank = snv_per_gene_rank,status_monitor="analysis",status
            )
            
            # snv survival ------------------------------------------------------------
            snv_survival %>%
              dplyr::mutate(filter_survival = purrr::map(diff_pval, filter_gene_list, gene_list = gene_set$match)) %>%
              dplyr::select(-diff_pval) %>%
              dplyr::filter(cancer_types %in% selected_ctyps()) %>%
              tidyr::unnest() %>%
              dplyr::mutate(logP = -log10(logRankP)) %>%
              dplyr::mutate(logP = ifelse(logP > 15, 15, logP)) %>%
              dplyr::mutate(logP = ifelse(logP < -log10(0.05), NA, logP)) %>%
              tidyr::drop_na()-> snv_sur_plot_ready-> snv_sur_plot_ready
            # survival ----------------------------------------------------------------
            if(nrow(snv_sur_plot_ready)>0){
              
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
                gene_rank = snv_sur_gene_rank, sizename = "logRank P", colorname = "Mutation Worse",title="Overall survival difference between mutation and non mutation genes.",status_monitor="analysis",status
              )
              
            } else{
              .msg <- paste(.msg,glue::glue("No significant [SNV survival] result of gene: {paste0(gene_set$match, collapse = ', ')} in your selected cancer type: {paste0(selected_ctyps(), collapse = ', ')}."),sep=" ")
              output[["snv_survival-plot"]] <- renderPlot({NULL})
            }
            
            
            .msg <- paste(.msg,glue::glue("Since we just show significant results, so a small size of gene and cancer set may cause no significant result in some plots, if it happens, try more genes and cancer types."),sep=" ")
            # alert for information
            shinyBS::createAlert(
              session = session, anchorId = "snv-no_gene_set", title = "Information", style = "info",
              content = .msg, append = FALSE
            )
            
            # maf ---------------------------------------------------------------------
            snv_InpSel <-  paste0(selected_ctyps(), collapse = "','")
            query =  as.expression(paste0("Cancer_Types %in% c('",snv_InpSel,"')"))
            # my_subsetMaf(mc3_pass, genes = gene_set$match, mafObj = T,query = query) -> gene_list_maf #
            maftools::subsetMaf(mc3_pass, genes = gene_set$match, mafObj = T,query = query) -> gene_list_maf
            
            #1. snv summary
            snv_su_out<-file.path(user_dir, "pngs", paste(user_id, "-SNV_summary_profile.png", sep = ""))
            callModule(snv_maf_summaryPlot,"snv_summary",gene_list_maf=gene_list_maf,outfile=snv_su_out,status_monitor="analysis",status)
            
            #2. oncoplot
            snv_onco_out<-file.path(user_dir, "pngs", paste(user_id, "-SNV_oncoplot_profile.png", sep = ""))
            callModule(snv_maf_oncoPlot,"snv_oncoplot",gene_list_maf=gene_list_maf,pancan_color=pancan_color,outfile=snv_onco_out,status_monitor="analysis",status)
            print(glue::glue("{paste0(rep('-', 10), collapse = '')} End maf part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))        
          }else{
            shinyBS::createAlert(
              session = session, anchorId = "snv-no_cancer_set", title = "Oops",
              content = "No cancer selected! Please select at least one cancer type.", style = "danger", append = FALSE
            )
          }
        } else{
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