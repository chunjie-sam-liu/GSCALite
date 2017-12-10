# source by server.R
# source by ui.R
# saved as functions_server.R

##########################################
# Cancer selection data get and comfim####
##1. cancer select for each part ui ######
##2. cancer type selection confirm  ######
##cancerTypeInput & cancerType############
##########################################
# cancer type selection ---------------------------------------------------
cancerTypeInput<-function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # cancer type selection----
      column(width = 10,
             offset = 1,
             shiny::tags$br(),
             shiny::tags$h3("Cancer Type Selection",class="text-success"),
             shiny::tags$br(),
             
             shinydashboard::tabBox(width = 12, title = "Tissue",
                                    tabPanel("Kidney",
                                             shiny::tags$h4("Kidney",class="text-success"),
                                             checkboxGroupInput(inputId = ns("Kidney"),label = NULL,inline = TRUE,
                                                                choices = list("Kidney Chromophobe(KICH)"="KICH",
                                                                               "Kidney Renal Clear Cell Carcinoma(KIRC)"="KIRC",
                                                                               "Kidney Renal Papillary Cell Carcinoma(KIRP)"="KIRP"))),
                                    tabPanel("Adrenal Gland",
                                             checkboxGroupInput(inputId = ns("Adrenal_Gland"),label = NULL,inline = TRUE,
                                                                choices = list("Adrenocortical Carcinoma(ACC)"="ACC",
                                                                               "Pheochromocytoma and Paraganglioma(PCPG)"="PCPG"))),
                                    tabPanel("Brain",
                                             checkboxGroupInput(inputId = ns("Brain"),label = NULL,inline = TRUE,
                                                                choices = list("Glioblastoma Multiforme(GBM)"="GBM",
                                                                               "Brain Lower Grade Glioma(LGG)"="LGG"))),
                                    tabPanel("Colorectal",
                                             checkboxGroupInput(inputId = ns("Colorectal"),label = NULL,inline = TRUE,
                                                                choices = list("Colon Adenocarcinoma(COAD)"="COAD",
                                                                               "Rectum Adenocarcinoma(READ)"="READ"))),
                                    tabPanel("Lung",
                                             checkboxGroupInput(inputId = ns("Lung"),label = NULL,inline = TRUE,
                                                                choices = list("Lung Adenocarcinoma(LUAD)"="LUAD",
                                                                               "Lung Squamous Cell Carcinoma(LUSC)"="LUSC"))),
                                    tabPanel("Uterus",
                                             checkboxGroupInput(inputId = ns("Uterus"),label = NULL,inline = TRUE,
                                                                choices = list("Uterine Corpus Endometrial Carcinoma(UCEC)"="UCEC",
                                                                               "Uterine Carcinosarcoma(UCS)"="UCS"))),
                                    tabPanel("Bile Duct",
                                             checkboxGroupInput(inputId = ns("Bile_Duct"),label = NULL,inline = TRUE,
                                                                choices = list("Bladder Urothelial Carcinoma(BLCA)"="BLCA"))),
                                    tabPanel("Bone Marrow",
                                             checkboxGroupInput(inputId = ns("Bone_Marrow"),label = NULL,inline = TRUE,
                                                                choices = list("Acute Myeloid Leukemia(LAML)"="LAML"))),
                                    tabPanel("Breast",
                                             checkboxGroupInput(inputId = ns("Breast"),label = NULL,inline = TRUE,
                                                                choices = list("Breast Invasive Carcinoma(BRCA)"="BRCA"))),
                                    tabPanel("Cervix",
                                             checkboxGroupInput(inputId = ns("Cervix"),label = NULL,inline = TRUE,
                                                                choices = list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)"="CESC"))),
                                    tabPanel("Other tissues",
                                             checkboxGroupInput(inputId = ns("other_tissue"),label = NULL,inline = TRUE,
                                                                choices = list("Lymphoid Neoplasm Diffuse Large B-cell Lymphoma(DLBC)"="DLBC",
                                                                               "Esophageal Carcinoma(ESCA)"="ESCA",
                                                                               "Stomach Adenocarcinoma(STAD)"="STAD",
                                                                               "Head and Neck Squamous Cell Carcinoma(HNSC)"="HNSC",
                                                                               "Liver Hepatocellular Carcinoma(LIHC)"="LIHC",
                                                                               "Mesothelioma(MESO)"="MESO",
                                                                               "Ovarian Serous Cystadenocarcinoma(OV)"="OV",
                                                                               "Pancreatic Adenocarcinoma(PAAD)"="PAAD",
                                                                               "Prostate Adenocarcinoma(PRAD)"="PRAD",
                                                                               "Sarcoma(SARC)"="SARC",
                                                                               "Skin Cutaneous Melanoma(SKCM)"="SKCM",
                                                                               "Testicular Germ Cell Tumors(TGCT)"="TGCT",
                                                                               "Thyroid Carcinoma(THCA)"="THCA",
                                                                               "Thymoma(THYM)"="THYM",
                                                                               "Uveal Melanoma(UVM)"="UVM")))
             )),
      shiny::tags$hr(width="85%")
    )
  )
}
# cancerType server function ----------------------------------------------
# pair with cancerTypeInput in functions_ui.R##
# Call by *_*_server.R by callModule(cancerType,"id pair with UI part")
cancerType <- function(input,output, session){
  cancer_type <- reactive({
    c(input$Kidney,input$Adrenal_Gland,input$Brain,input$Colorectal,
      input$Lung,input$Uterus,input$Bile_Duct,input$Bone_Marrow,input$Breast,
      input$Cervix,input$other_tissue) ->cancer_type
  })
  return(cancer_type)
}


###############################################################
# Plot function to generate plot in ui#########################
##1. plotoutout in ui and in server ###########################
##2. draw specific pic type by calling different function  ######
##PlotInput & Plot#################################
###############################################################
# call in ui by PlotInput("cnv_pie",..) OR  PlotInput("cnv_bar",..)
# call in ser by callModule(Plot,"cnv_pie",...) OR ...
PlotInput <- function(id,width,height){
  ns<-NS(id)
  
  tagList(
    plotOutput(ns("plot")),
    hr()
    #sliderInput(ns("num"),label = "Select size of number",min=10,max = 100,value = 50)
  )
  
}

Plot <- function(input,output,session){ #data(raw data, gene set, cancer types),plot type(decide function type) 
  #size <- reactive(as.numeric(input$num))
  # x <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  #   })
  # y <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  # })
  x<-rnorm(50)
  y<-nrow(50)
  
  output$plot <-renderPlot({
    plot(x,y)
  }) # fun argument decide what function will be called.
  
  
}


