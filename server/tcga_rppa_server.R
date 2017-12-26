# sourced by 'server.R'
# save as 'tcga_rppa_server.R'
# server elements 'tcga_rppa' sub tab of 'tcga' tab


# load rppa data ---------------------------------------------------------
# diff methylation between tumor and normal
rppa_per<- readr::read_rds(file.path(config$database, "TCGA", "rppa", "pan32_gene_activate.inhibit_pathway_percent.rds.gz"))
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
observeEvent(input$analysis, {
  # get gene set /cancer type data ----
  rppa_per %>%
    dplyr::filter(symbol %in% cnv_gene_list()) ->gene_list_rppa_per
  # rppa global pie plot----
  gene_list_rppa_per %>%
    tidyr::unnest() %>%
    tidyr::gather(-symbol,-pathway,key="class",value="per") %>%
    dplyr::mutate(class=plyr::revalue(class,replace = c("a"="Activation","i"="Inhibition","n"="None")))->rppa_pie_plot_ready
  
  callModule(rppaPiePlot,"rppa_pie",data=rppa_pie_plot_ready, y="per",fill="class", facet_grid=" symbol~pathway")  
  # rppa global percentage ----
  # data process
  gene_list_rppa_per %>%
    tidyr::unnest() %>%
    dplyr::filter(a+i > 5/32) %>%
    tidyr::gather(-symbol,-pathway,key="class",value="per") %>%
    dplyr::mutate(per=ifelse(class=="i",-per*100,per*100)) %>%
    tidyr::unite(pathway,c(pathway,class)) -> rppa_per_ready
  # pic draw
  output$rppa_per_plot <- renderPlot({
    rppa_per_ready %>%
      ggplot(aes(x = pathway, y = symbol))+
      xlab("Pathway")+ylab("Symbol") +
      guides(fill=guide_colorbar("Percent")) +
      geom_tile(aes(fill = per), col = "white") +
      geom_text(label=ceiling(rppa_per_ready$per)) +
      scale_fill_gradient2(
        high = "red",
        mid = "white",
        low = "blue"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1),
        axis.text.y = element_text()
      ) + 
      xlab("Pathway (a:activate; i:inhibit)")
  })
})
observeEvent(input$rppa_submit, {
# get gene set /cancer type data ----

  rppa_relation %>%
    dplyr::filter(cancer_types %in% rppa_cancer_type()) %>% 
    dplyr::mutate(data = purrr::map(data, filter_gene_list, gene_list = cnv_gene_list())) %>%
    tidyr::unnest() -> gene_list_cancer_rppa_rela

  # ploting -----------------------------------------------------------------
  
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
  # plot draw
  output$rppa_rela_plot <- renderImage({
    ggplot() +
      geom_segment(data = plot_seg, mapping = aes(
        x = x1, 
        y = y1,
        xend = x2,
        yend = y2,
        colour = Cancer,
        linetype = Regulation
      )) +
      guides(color=FALSE) +
      geom_text(
        data = cancer.text, 
        mapping = aes(x = x, y = y, label = text,color=text),
        hjust = 1,
        sieze = 2
      ) +
      geom_text(
        data = gene.text, 
        mapping = aes(x = x-0.4, y = y, label = text),
        hjust=0,
        size=2) +
      geom_text(
        data = path.text, 
        mapping = aes(x = x, y = y, label = text),
        hjust=0,
        size=2) +
      expand_limits(x=c(-1,10)) +
      theme(
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # text = element_text(size=5),
        plot.title = element_text(hjust = 0.5,size=7),
        plot.margin = rep(unit(0,"null"),4),
        legend.position = "bottom",
        legend.text = element_text(size=3),
        legend.key.size = unit(0.25,cm)
      ) + 
      xlab("") +
      ylab("") +
      labs(title = "Relation network between gene and cancer related pathways.") ->p
    outfile <- paste("/project/huff/huff/github/GSCALite/userdata","/","TCGA_RPPA_rellation_network",'.png',sep="")
    ggsave(outfile,p,device ="png",width =5, height = 5)
    list(src = outfile,
         contentType = 'image/png',
         # width = "100%" ,
         # height = 900,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  # callModule(rppa_line_contact,"rppa_rela",seg=plot_seg,cancer=cancer.text,gene=gene.text,pathway=path.text,title="Relation network between gene and cancer related pathways.")
})