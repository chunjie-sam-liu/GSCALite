# data input --------------------------------------------------------------
# ? global data can't load here.
# get gene set ----
# input<-list(gene_set=c("TP53","EZH2","CD274","CD276","CD80",
#                        "CD86","VTCN1","CD40LG","TNFRSF14",
#                        "TNFSF9","TNFSF4","CD70",ICOS",
#                        "BTLA","LAG3","TNFRSF9","TNFRSF4"))
gene_set <- reactive({
  c("TP53","EZH2","CD274","CD276","CD80",
    "CD86","VTCN1","CD40LG","TNFRSF14",
    "TNFSF9","TNFSF4","CD70","ICOS",
    "BTLA","LAG3","TNFRSF9","TNFRSF4")
})
gene_list <- data.frame(symbol=isolate(gene_set()))
gene_list$symbol <- gene_list$symbol %>% as.character()

# load cnv data ----
# cnv <- readr::read_rds(file.path(config$database, "TCGA","cnv","pancan34_cnv.rds.gz"))
cnv <- readr::read_rds(file.path(config$database, "TCGA","cnv","pancan34_cnv_threshold.rds.gz"))

#  get cancer type --------------------------------------------------------
cancer_type <- callModule(cancerType,"cnv")
#give test value for cancer type
# input$Kidney="KIRC"
# input$Adrenal_Gland=c()
# input$Brain=c("LGG")
# input$Colorectal="COAD"
# input$Lung=c("LUAD","LUSC")
# input$Uterus=""
# input$Bile_Duct=""
# input$Bone_Marrow=""
# input$Breast="BRCA"
# input$Cervix=""
# input$other_tissue=""
# cancer_type <- reactive(c("KIRC","LGG","COAD","LUAD","LUSC","BRCA"))


output$selected_cancer <- renderText(
  cancer_type()
)


# analysis core -----------------------------------------------------------

# get gene set cnv ----
cnv %>%
  dplyr::mutate(filter_cnv = purrr::map(cnv, filter_gene_list, gene_list = gene_list)) %>%
  dplyr::select(-cnv) -> gene_list_cnv

observeEvent(input$cnv_submit,{
  # get cancer type cnv ----
  gene_list_cnv %>%
    dplyr::filter(cancer_types %in% cancer_type() ) -> gene_list_cancer_cnv
  # gene_list_cnv %>%
  #   dplyr::filter(cancer_types %in% isolate(cancer_type()) ) -> gene_list_cancer_cnv
  
  # get gene set cnv percent  ----
  gene_list_cancer_cnv %>%
    fn_cnv_percecnt() ->gene_list_cnv_per
  
  # get data for plot ----
  gene_list_cnv_per %>%
    tidyr::drop_na() %>%
    dplyr::mutate(other=1 - a_total - d_total)-> cnv_plot_ready
  
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
  gene_list_cancer_cnv %>% 
    dplyr::mutate(rs = purrr::map2(cancer_types, filter_cnv, fn_gen_combined_core_atg, g_list = gene_list,n=1)) %>% 
    dplyr::select( -filter_cnv) %>% 
    tidyr::unnest(rs) %>%
    dplyr::mutate(del_a = -del_a) %>% 
    dplyr::mutate(del_s = -del_s) %>%
    tidyr::gather(key = type, value = per, -cancer_types) ->cnv_bar_plot_ready
  
  callModule(cnvbarPlot,"cnv_bar",data=cnv_bar_plot_ready,x="cancer_types",y="per",fill="type")
  
  # cnv oncostrip plot  ----

    callModule(Plot,"cnv_oncostrip")

# cnv exclusive -----------------------------------------------------------
  # cl <- parallel::detectCores()
  # system.time(cluster <- multidplyr::create_cluster(floor(cl * 5 / 6)))
  # system.time(gene_list_cancer_cnv %>% 
  #   purrr::pmap(.f = fn_cnv_mutal_exclusive,cluster=cluster) %>% 
  #   dplyr::bind_rows() -> mutual_exclusive)
  # parallel::stopCluster(cluster)

  
    callModule(Plot,"cnv_exclusive")
  
})















# plot generation ---------------------------------------------------------
# observeEvent(input$cnv_submit,{
#   callModule(Plot,"cnv_pie")
#   callModule(Plot,"cnv_hete")
#   callModule(Plot,"cnv_homo")
#   callModule(Plot,"cnv_bar")
#   callModule(Plot,"cnv_oncostrip")
#   callModule(Plot,"cnv_exclusive")
# })

