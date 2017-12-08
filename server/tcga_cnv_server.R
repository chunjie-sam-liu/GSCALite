# data input --------------------------------------------------------------
# ? global data can't load here.
# get gene set ----
gene_set <- reactive({gene_set<-c("TP53","EZH2","CD274","CD276","CD80",
                             "CD86","VTCN1","CD40LG","TNFRSF14",
                             "TNFSF9","TNFSF4","CD70","CD40LG","ICOS",
                             "BTLA","LAG3","TNFRSF9","TNFRSF4")})
# load cnv data ----
# >>>>>>>>>

#  get cancer type --------------------------------------------------------
cancer_type <- callModule(cancerType,"cnv")
output$selected_cancer <- renderText(
  cancer_type()
)


# plot generation ---------------------------------------------------------
observeEvent(input$cnv_submit,{
  callModule(Plot,"cnv_pie")
  callModule(Plot,"cnv_hete")
  callModule(Plot,"cnv_homo")
  callModule(Plot,"cnv_bar")
  callModule(Plot,"cnv_oncostrip")
  callModule(Plot,"cnv_exclusive")
})

