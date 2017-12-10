# data input --------------------------------------------------------------
# ? global data can't load here.
# get gene set ----
gene_set <- reactive({})
# load cnv data ----
cnv <- readr::read_rds(file.path(config$database, "TCGA","cnv","pancan34_cnv.rds.gz"))


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

