# sourced by 'server.R'
# save as 'tcga_rppa_server.R'
# server elements 'tcga_rppa' sub tab of 'tcga' tab


# load rppa data ---------------------------------------------------------
# diff methylation between tumor and normal
rppa_per <- readr::read_rds(file.path(config$database, "TCGA", "rppa", "pan32_gene_A-I-N_percent.rds.gz"))


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

# get gene set data ----
rppa_per %>%
  dplyr::filter(symbol %in% gene_set()) ->gene_list_rppa_per

# submit cancer type -------------------------------------------------------

observeEvent(input$rppa_submit, {
  # get cancer type data ----
  
  # ploting -----------------------------------------------------------------
  # rppa global pie plot----
  
  # rppa global percentage ----
  
})