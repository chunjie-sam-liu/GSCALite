# sourced by 'server.R'
# save as 'tcga_cnv_server.R'
# server elements 'tcga_cnv' sub tab of 'tcga' tab

# data input --------------------------------------------------------------
# ? global data can't load here.
# get gene set ----
# input<-list(gene_set=c("TP53","EZH2","CD274","CD276","CD80",
#                        "CD86","VTCN1","CD40LG","TNFRSF14",
#                        "TNFSF9","TNFSF4","CD70",ICOS",
#                        "BTLA","LAG3","TNFRSF9","TNFRSF4"))
# cnv_gene_list <- reactive({
  # c("TP53","EZH2","CD274","CD276","CD80",
  #   "CD86","VTCN1","CD40LG","TNFRSF14",
  #   "TNFSF9","TNFSF4","CD70","ICOS",
  #   "BTLA","LAG3","TNFRSF9","TNFRSF4")
# })
# cnv_gene_list <- data.frame(symbol=isolate(gene_set()))
# cnv_gene_list$symbol <- gene_list$symbol %>% as.character()


# load cnv data ----
cnv <- readr::read_rds(file.path(config$database, "TCGA","cnv","pancan34_cnv_percent.rds.gz"))
cnv_raw <- readr::read_rds(file.path(config$database, "TCGA","cnv","pancan34_cnv_threshold.rds.gz"))
cnv_cor <- readr::read_rds(file.path(config$database, "TCGA", "cnv", "pancan34_all_gene_exp-cor-cnv.rds.gz"))

#  get cancer type --------------------------------------------------------
cnv_cancer_type <- callModule(cancerType,"cnv")
#give test value for cancer type
# cancer_type <- reactive(c("KIRC","LGG","COAD","LUAD","LUSC","BRCA"))
output$cnv_selected_cancer <- renderText(
  cnv_cancer_type()
)
# reset cancer selection when click.
observeEvent(input$cnv_reset, {
  cnv_cancer_type<-callModule(resetcancerType,"cnv")
})

# analysis core -----------------------------------------------------------

# get gene set cnv ----cnv_
cnv_gene_list <- eventReactive(input$analysis,{
  if(status$analysis){
    cnv_gene_list <- as.character(gene_set$match)
    # shiny::updateActionButton(session,"cnv_submit")
    # print(input$cnv_submit)
    # cnv_cancer_type <- callModule(selectallCancer,"cnv")
    }
})

# submit cancer type -------------------------------------------------------

observeEvent(input$cnv_submit,{
  # get cancer type cnv ----

  print(cnv_gene_list())
  cnv %>%
    dplyr::mutate(filter_cnv = purrr::map(cnv, filter_gene_list, gene_list = cnv_gene_list())) %>%
    dplyr::select(-cnv) %>%
    dplyr::filter(cancer_types %in% cnv_cancer_type()) -> gene_list_cancer_cnv
  
  cnv_raw %>%
    dplyr::mutate(filter_cnv = purrr::map(cnv, filter_gene_list, gene_list = cnv_gene_list())) %>%
    dplyr::select(-cnv) %>%
    dplyr::filter(cancer_types %in% cnv_cancer_type() ) -> gene_list_cancer_cnv_raw
  
  cnv_cor %>%
    dplyr::mutate(spm = purrr::map(spm, filter_gene_list, gene_list = cnv_gene_list())) %>%
    dplyr::filter(cancer_types %in% cnv_cancer_type()) %>%
    tidyr::unnest()-> gene_list_cancer_cnv_cor
  
  # gene_list_cnv %>%
  #   dplyr::filter(cancer_types %in% isolate(cancer_type()) ) -> gene_list_cancer_cnv
  # gene_list_cnv_raw %>%
  #   dplyr::filter(cancer_types %in% cnv_cancer_type() ) -> gene_list_cancer_cnv_raw
  # gene_list_cnv_cor %>%
  #   dplyr::filter(cancer_types %in% cnv_cancer_type() ) -> gene_list_cancer_cnv_cor
  
  # get data for plot ----
  gene_list_cancer_cnv %>%
    tidyr::unnest() %>%
    tidyr::drop_na() -> cnv_plot_ready
  
  # cancer rank ----
  cnv_plot_ready %>% 
    dplyr::group_by(cancer_types) %>% 
    dplyr::summarise(v = sum(a_total-d_total)) %>% 
    dplyr::arrange(dplyr::desc(v)) -> cnv_cancer_rank
  
  #gene rank ----
  cnv_plot_ready %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::summarise(v = sum(a_total-d_total)) %>%
    dplyr::arrange(v) -> cnv_gene_rank
  
  # plot generate -----------------------------------------------------------
  
  # pie plot ----
  cnv_plot_ready %>% 
    dplyr::select(-a_total,-d_total) %>%
    tidyr::gather(key = type, value = per, -c(cancer_types, symbol)) %>% 
    dplyr::mutate(
      symbol = factor(x = symbol, levels = cnv_gene_rank$symbol), 
      cancer_types = factor(x = cancer_types, levels = cnv_cancer_rank$cancer_types)) ->pie_plot_ready
  
  callModule(piePlot, "cnv_pie", data=pie_plot_ready, y="per",
             fill="type", facet_grid="cancer_types ~ symbol")

  # homo cnv plot ----
  cnv_homo_plot_ready <- cnv_plot_ready %>% 
    dplyr::select(cancer_types,symbol,a_homo,d_homo) %>%
    tidyr::gather(key = type, value = per, -cancer_types,-symbol) %>% 
    dplyr::mutate(effect=plyr::revalue(type,replace = c("a_homo"="Homozygous Amplification","d_homo"="Homozygous Deletion"))) %>%
    dplyr::mutate(color=plyr::revalue(type,replace = c("a_homo"="brown4","d_homo"="aquamarine4")))
  
  callModule(pointPlot,"cnv_homo",data=cnv_homo_plot_ready,cancer="cancer_types",
                                  gene="symbol",size="per",color="color",sizename="Homo CNV%",
                                  colorname="SCNA Type",wrap="~ effect")
  
  # hete cnv plot ----
  cnv_hete_plot_ready <- cnv_plot_ready %>% 
    dplyr::select(cancer_types,symbol,a_hete,d_hete) %>%
    tidyr::gather(key = type, value = per, -cancer_types,-symbol) %>% 
    dplyr::mutate(effect=plyr::revalue(type,replace = c("a_hete"="Heterozygous Amplification","d_hete"="Heterozygous Deletion"))) %>%
    dplyr::mutate(color=plyr::revalue(type,replace = c("a_hete"="brown1","d_hete"="aquamarine3")))
  
  callModule(pointPlot,"cnv_hete",data=cnv_hete_plot_ready,cancer="cancer_types",
             gene="symbol",size="per",color="color",sizename="Hete CNV%",
             colorname="SCNA Type",wrap="~ effect")

  # bar stack plot ----
  gene_list_cancer_cnv_raw %>%
    dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_gen_combined_core_atg, g_list = cnv_gene_list(),n=1)) %>%
    dplyr::select( -filter_cnv) %>%
    tidyr::unnest(rs) %>%
    dplyr::mutate(del_a = -del_a) %>%
    dplyr::mutate(del_s = -del_s) %>%
    tidyr::gather(key = type, value = per, -cancer_types) %>%
    dplyr::mutate(cnv_type="Hete CNV") -> cnv_hete_bar_plot_ready

  gene_list_cancer_cnv_raw %>%
    dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_gen_combined_core_atg, g_list = cnv_gene_list(),n=2)) %>%
    dplyr::select( -filter_cnv) %>%
    tidyr::unnest(rs) %>%
    dplyr::mutate(del_a = -del_a) %>%
    dplyr::mutate(del_s = -del_s) %>%
    tidyr::gather(key = type, value = per, -cancer_types) %>%
    dplyr::mutate(cnv_type="Homo CNV")-> cnv_homo_bar_plot_ready

  rbind(cnv_hete_bar_plot_ready,cnv_homo_bar_plot_ready) -> cnv_bar_plot_ready

  callModule(cnvbarPlot,"cnv_bar",data=cnv_bar_plot_ready,x="cancer_types",y="per",fill="type")
  
  # cnv to expression plot  ----
  gene_list_cancer_cnv_cor %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise(rank=sum(spm)) %>%
    dplyr::arrange(rank) ->gene_rank.cnvcor
  
  gene_list_cancer_cnv_cor %>%
    dplyr::group_by(cancer_types) %>%
    dplyr::summarise(rank=sum(spm)) %>%
    dplyr::arrange(rank) ->cancer_rank.cnvcor
  
  callModule(methy_diff_pointPlot,"cnv_exp", data=gene_list_cancer_cnv_cor, cancer="cancer_types", gene="symbol", size="logfdr", color="spm", cancer_rank=cancer_rank.cnvcor,gene_rank=gene_rank.cnvcor,sizename="-Log10(P.value)", colorname="Spearman Correlation Coefficient", title="Spearman Correlation Coefficient of CNV and gene expression.")

    # callModule(Plot,"cnv_oncostrip")


  
})


# monitor the analysis buttons change -------------------------------------

# cnv_analysis_status <- eventReactive(
#   eventExpr = input$analysis,
#   valueExpr = {
#   status$analysis == TRUE
#   })
# observe(cnv_analysis_status() == TRUE,{
#   
# })
