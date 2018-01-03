# sourced by 'server.R'
# save as 'tcga_snv_server.R'
# server elements 'tcga_snv' sub tab of 'tcga' tab

# load snv data  ----------------------------------------------------------
print(glue::glue("{paste0(rep('-', 10), collapse = '')} start Load snv @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))
snv <- readr::read_rds(file.path(config$database, "TCGA", "snv", ".rds_snv_all_gene_snv_count.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} end Load snv @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))

print(glue::glue("{paste0(rep('-', 10), collapse = '')} start Load snv @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))
mc3_pass <- readr::read_rds(file.path(config$database, "TCGA", "snv", "snv_mutation_mc3_public.pass.filtered_maf-4cancers.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} end Load mc3_pass @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))

# mc3_maf <- readr::read_rds(file.path(config$database,"TCGA","snv","snv_mutation_mc3_public.pass.filtered_maf.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} start Load snv @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))
snv_survival <- readr::read_rds(file.path(config$database, "TCGA", "snv", "pancan32_snv_survival_genelist_sig_pval.rds.gz"))
print(glue::glue("{paste0(rep('-', 10), collapse = '')} end Load snv_survival @ {Sys.time()}{paste0(rep('-', 10), collapse = '')}"))


#  get cancer type --------------------------------------------------------
snv_cancer_type <- callModule(cancerType, "snv")

# Cancer types value box selection ----------------------------------------

callModule(module = cancerTypesSelect, id = "snv", .sctps = snv_cancer_type)
# Check box ---------------------------------------------------------------

callModule(module = selectAndAnalysis, id = "snv", .id = "snv")

# button control --------------------------------------------------------

# submit cancer type -------------------------------------------------------

snv_submit_analysis <- function(input, output, session){
  observeEvent(input$submit, {
    status$snv_submit <- TRUE
    print(status$snv_submit)
  })
}

callModule(snv_submit_analysis,"snv")

# analysis core -----------------------------------------------------------
# monitor for gene list change-----------------------------------
snv_gene_list <- eventReactive(
  eventExpr = status$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    # be sure the following code run after start analysis
    if (status$analysis == TRUE) {
      status$snv_submit <- TRUE
      shinyjs::disable(id = "snv-submit")
      shinyjs::disable(id = "snv-switch")
      as.character(gene_set$match)
    } 
  }
)
# get gene set snv --------------------------------------------------------
snv_analysis <- eventReactive(
  {
    status$snv_submit == TRUE
  },
  ignoreNULL = TRUE,
  valueExpr = {
    if (status$snv_submit == TRUE) {
      print(snv_gene_list())
      .msg <- c("NOTICE: ")
      # load data----
      # load_data_snv()
      
      # remove pic result generate before ----
      # callModule(removePic,"snv_percentage",outtype="plot")
      # callModule(removePic,"snv_summary",outtype="image")
      # callModule(removePic,"snv_oncoplot",outtype="image")
      # callModule(removePic,"snv_survival",outtype="plot")
      if(length(snv_gene_list())!=0){
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start snvy part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        
        # snv percent -------------------------------------------------------------
        snv %>%
          dplyr::mutate(filter_snv = purrr::map(mut_count, filter_gene_list, gene_list = snv_gene_list())) %>%
          dplyr::select(-mut_count) %>%
          dplyr::filter(cancer_types %in% snv_cancer_type()) -> gene_list_cancer_snv
        
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
          cancer_rank = snv_per_cancer_rank, gene_rank = snv_per_gene_rank
        )
        
        # snv survival ------------------------------------------------------------
        snv_survival %>%
          dplyr::mutate(filter_survival = purrr::map(diff_pval, filter_gene_list, gene_list = snv_gene_list())) %>%
          dplyr::select(-diff_pval) %>%
          dplyr::filter(cancer_types %in% snv_cancer_type()) %>%
          tidyr::unnest() -> gene_list_snv_survival
        # survival ----------------------------------------------------------------
        if(nrow(gene_list_snv_survival)>0){
          gene_list_snv_survival %>%
            # tidyr::unnest() %>%
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
        } else{
          .msg <- paste(.msg,glue::glue("No significant [SNV survival] result of gene: {paste0(snv_gene_list(), collapse = ', ')} in your selected cancer type: {paste0(snv_cancer_type(), collapse = ', ')}."),sep=" ")
        }
        
        
        .msg <- paste(.msg,glue::glue("Since we just show significant results, so a small size of gene and cancer set may cause no significant result in some plots, if it happens, try more genes and cancer types."),sep=" ")
        # alert for information
        shinyBS::createAlert(
          session = session, anchorId = "snv-no_gene_set", title = "Information", style = "info",
          content = .msg, append = FALSE
        )
        status$snv_submit <- FALSE
        shinyjs::enable("snv-submit")
      } else{
        shinyBS::createAlert(
          session = session, anchorId = "snv-no_gene_set", title = "Oops",
          content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
        )
      }
    }
  }
)

snv_global_analysis <- eventReactive(
  {
    status$analysis == TRUE
  },
  ignoreNULL = TRUE,
  valueExpr={
    if (status$analysis == TRUE) {
      if(length(snv_gene_list())!=0){
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} Start maf part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
        
        # filter data
        # query = as.expression("cacer_type %in% ")
        maftools::subsetMaf(mc3_pass, genes = snv_gene_list(), mafObj = T) -> gene_list_maf #query = query,
        
        #1. snv summary
        snv_su_out<-file.path(user_dir, "pngs", paste(user_id, "-SNV_summary_profile.png", sep = ""))
        callModule(snv_maf_summaryPlot,"snv_summary",gene_list_maf=gene_list_maf,outfile=snv_su_out)
        
        #2. oncoplot
        snv_onco_out<-file.path(user_dir, "pngs", paste(user_id, "-SNV_oncoplot_profile.png", sep = ""))
        callModule(snv_maf_oncoPlot,"snv_oncoplot",gene_list_maf=gene_list_maf,outfile=snv_onco_out,cancer_type=snv_cancer_type())
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} End maf part analysis @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      }else{
        shinyBS::createAlert(
          session = session, anchorId = "snv-no_gene_set", title = "Oops",
          content = "No input gene set! Please go to Welcome page to input gene set.", style = "danger", append = FALSE
        )
      }
    }
  }
)


# monitors -------------------------------------------------------
observe(snv_global_analysis())
observe(snv_gene_list())
observe(snv_analysis())