# sourced by 'server.R'
# save as 'tcga_rppa_server.R'
# server elements 'tcga_rppa' sub tab of 'tcga' tab


# load rppa data ---------------------------------------------------------
# diff methylation between tumor and normal
rppa_per <- readr::read_rds(file.path(config$database, "TCGA", "rppa", "pan32_gene_activate.inhibit_pathway_percent.rds.gz"))
rppa_relation <- readr::read_rds(file.path(config$database, "TCGA", "rppa", "pan32_gene_A-I-N_sig_pval_class.siplification.rds.gz"))

#  get cancer type --------------------------------------------------------

rppa_cancer_type <- callModule(cancerType, "rppa")
output$rppa_selected_cancer <- renderText(
  rppa_cancer_type()
)
# reset cancer selection when click reset button.
observeEvent(input$rppa_reset, {
  rppa_cancer_type<-callModule(resetcancerType,"rppa")
})



# analysis core -----------------------------------------------------------
# submit cancer type -------------------------------------------------------

observeEvent(input$rppa_submit, {
# get gene set /cancer type data ----
  rppa_per %>%
    dplyr::filter(symbol %in% cnv_gene_list()) ->gene_list_rppa_per

  rppa_relation %>%
    dplyr::filter(cancer_types %in% rppa_cancer_type()) %>% 
    dplyr::mutate(data = purrr::map(data, filter_gene_list, gene_list = cnv_gene_list())) %>%
    tidyr::unnest() -> gene_list_cancer_rppa_rela

  # ploting -----------------------------------------------------------------
  # rppa global pie plot----
  
  # rppa global percentage ----
  
  # rppa line contact ----
  # get data
  cancer_text <- get_rppa_text(gene_list_cancer_rppa_rela)
  plot_seg <- get_rppa_seg(cancer_text,gene_list_cancer_rppa_rela) 
  
  cancer_text %>%
    dplyr::filter(type=="cancer") ->cancer.text
  cancer_text %>%
    dplyr::filter(type=="gene") ->gene.text
  cancer_text %>%
    dplyr::filter(type=="pathway") ->path.text
  callModule(rppa_line_contact,"rppa_rela",seg=plot_seg,cancer=cancer.text,gene=gene.text,pathway=path.text,title="Relation network between gene and cancer related pathways.")
})