# sourced by 'ui.R'
# save as 'tcga_rppa_ui.R'
# ui elements 'tcga_rppa' sub tab of 'tcga' tab

tabItem(tabName = "tcga_rppa", align = "center",
        shinyjs::useShinyjs(),
        
        ## SNV message ----
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>RPPA
                      <font color='#777777'>
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Cancer related pathway activity</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>RPPA data from TCPA are used to calculate score (see details <code>help page</code> below) for <b>10 cancer related pathways</b> and <b>32 cancer types</b>, and a correlation is generated between candidate gene expression and a specific pathway score (see details <code>help page</code> below). Here we show you the relationship between gene expression and pathway activity.
                      <br>Protein expression will be showed in each cancer if it has been measured in TCPA data set.</p>
                      </div>
                      </div>
                      </div>
                      </div>")
                 ),
        ## Hlep message including in tcga_rppa_help.ui----
        source(file.path(config$ui,"tcga_rppa_help.R"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        # cancer type selection and result output---------------------------------------------------
        fluidRow(
          # cancer type selection----
          column(width = 4,
                 shiny::tags$h3("Input"),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Select cancers to do analysis:", solidHeader = TRUE,
                                     collapsible = TRUE,status = "success",
                                     checkboxGroupInput(inputId = "rppa_Kidney",
                                                        label = "Kidney",
                                                        choices = list("Kidney Chromophobe(KICH)"="KICH",
                                                                       "Kidney Renal Clear Cell Carcinoma(KIRC)"="KIRC",
                                                                       "Kidney Renal Papillary Cell Carcinoma(KIRP)"="KIRP")),
                                     checkboxGroupInput(inputId = "rppa_Adrenal_Gland",
                                                        label = "Adrenal Gland",
                                                        choices = list("Adrenocortical Carcinoma(ACC)"="ACC",
                                                                       "Pheochromocytoma and Paraganglioma(PCPG)"="PCPG")),
                                     checkboxGroupInput(inputId = "rppa_Brain",
                                                        label = "Brain",
                                                        choices = list("Glioblastoma Multiforme(GBM)"="GBM",
                                                                       "Brain Lower Grade Glioma(LGG)"="LGG")),
                                     checkboxGroupInput(inputId = "rppa_Colorectal",
                                                        label = "Colorectal",
                                                        choices = list("Colon Adenocarcinoma(COAD)"="COAD",
                                                                       "Rectum Adenocarcinoma(READ)"="READ")),
                                     checkboxGroupInput(inputId = "rppa_Lung",
                                                        label = "Lung",
                                                        choices = list("Lung Adenocarcinoma(LUAD)"="LUAD",
                                                                       "Lung Squamous Cell Carcinoma(LUSC)"="LUSC")),
                                     checkboxGroupInput(inputId = "rppa_Uterus",
                                                        label = "Uterus",
                                                        choices = list("Uterine Corpus Endometrial Carcinoma(UCEC)"="UCEC",
                                                                       "Uterine Carcinosarcoma(UCS)"="UCS")),
                                     checkboxGroupInput(inputId = "rppa_Bile_Duct",
                                                        label = "Bile Duct",
                                                        choices = list("Bladder Urothelial Carcinoma(BLCA)"="BLCA")),
                                     checkboxGroupInput(inputId = "rppa_Bone_Marrow",
                                                        label = "Bone Marrow",
                                                        choices = list("Acute Myeloid Leukemia(LAML)"="LAML")),
                                     checkboxGroupInput(inputId = "rppa_Breast",
                                                        label = "Breast",
                                                        choices = list("Breast Invasive Carcinoma(BRCA)"="BRCA")),
                                     checkboxGroupInput(inputId = "rppa_Cervix",
                                                        label = "Cervix",
                                                        choices = list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)"="CESC")),
                                     checkboxGroupInput(inputId = "rppa_other",
                                                        label = "Other cancers",
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
                                                                       "Uveal Melanoma(UVM)"="UVM"))
                 )
          ),
          
          
          # Tabset Panel
          # output plot -------------------------------------------------------------
          column(width = 8, 
                 shiny::tags$h3("Output"),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Global percentage, click to show protein expression and specific gene profile",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "rppa_pie",
                                       width = "700px",
                                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Protein expression profile",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "rppa_hete_profile",
                                       width = "700px",
                                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Pathway activity of specific gene",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "rppa_homo_profile",
                                       width = "700px",
                                       height = "100%")
                 
                 )
          )
        ),
        # load footer ----
        source(file.path(config$ui, "footer.R"))[1]
                 ) # close tab