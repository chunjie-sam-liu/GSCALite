# sourced by 'ui.R'
# save as 'tcga_cnv_ui.R'
# ui elements 'tcga_cnv' sub tab of 'tcga' tab

tabItem(tabName = "tcga_cnv", align = "center",
        shinyjs::useShinyjs(),
        
        ## SNV message ----
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>CNV
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Copy Number variation</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>TCGA CNV data will be used to give you a visualization of you gene set for seleted cancer types.
                <br>GSAC offers different types of graphic layout (CNV Pie distribution, Hete CNV, Homo CNV, CNV Bar distribution, Oncostrip, see details in <code>help page</code> below.) for you to visualize the CNV of your gene set for your seleted cancer types.</p>
                </div>
                </div>
                </div>
                </div>")
        ),
        ## Hlep message including in tcga_cnv_help.ui----
        source(file.path(config$ui,"tcga_cnv_help.R"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        # cancer type selection and result output---------------------------------------------------
        fluidRow(
          # cancer type selection----
          column(width = 4,
                 shiny::tags$h3("Input"),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Select cancers to do CNV analysis:", solidHeader = TRUE,
                                     collapsible = TRUE,status = "success",
                                     checkboxGroupInput(inputId = "cnv_Kidney",
                                                        label = "Kidney",
                                                        choices = list("Kidney Chromophobe(KICH)"="KICH",
                                                                       "Kidney Renal Clear Cell Carcinoma(KIRC)"="KIRC",
                                                                       "Kidney Renal Papillary Cell Carcinoma(KIRP)"="KIRP")),
                                     checkboxGroupInput(inputId = "cnv_Adrenal_Gland",
                                                        label = "Adrenal Gland",
                                                        choices = list("Adrenocortical Carcinoma(ACC)"="ACC",
                                                                       "Pheochromocytoma and Paraganglioma(PCPG)"="PCPG")),
                                     checkboxGroupInput(inputId = "cnv_Brain",
                                                        label = "Brain",
                                                        choices = list("Glioblastoma Multiforme(GBM)"="GBM",
                                                                       "Brain Lower Grade Glioma(LGG)"="LGG")),
                                     checkboxGroupInput(inputId = "cnv_Colorectal",
                                                        label = "Colorectal",
                                                        choices = list("Colon Adenocarcinoma(COAD)"="COAD",
                                                                       "Rectum Adenocarcinoma(READ)"="READ")),
                                     checkboxGroupInput(inputId = "cnv_Lung",
                                                        label = "Lung",
                                                        choices = list("Lung Adenocarcinoma(LUAD)"="LUAD",
                                                                       "Lung Squamous Cell Carcinoma(LUSC)"="LUSC")),
                                     checkboxGroupInput(inputId = "cnv_Uterus",
                                                        label = "Uterus",
                                                        choices = list("Uterine Corpus Endometrial Carcinoma(UCEC)"="UCEC",
                                                                       "Uterine Carcinosarcoma(UCS)"="UCS")),
                                     checkboxGroupInput(inputId = "cnv_Bile_Duct",
                                                        label = "Bile Duct",
                                                        choices = list("Bladder Urothelial Carcinoma(BLCA)"="BLCA")),
                                     checkboxGroupInput(inputId = "cnv_Bone_Marrow",
                                                        label = "Bone Marrow",
                                                        choices = list("Acute Myeloid Leukemia(LAML)"="LAML")),
                                     checkboxGroupInput(inputId = "cnv_Breast",
                                                        label = "Breast",
                                                        choices = list("Breast Invasive Carcinoma(BRCA)"="BRCA")),
                                     checkboxGroupInput(inputId = "cnv_Cervix",
                                                        label = "Cervix",
                                                        choices = list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)"="CESC")),
                                     checkboxGroupInput(inputId = "cnv_other",
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
                 shinydashboard::box(width = 12, title = "CNV Pie distribution",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "cnv_pie",
                                       width = "700px",
                                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Hete CNV profile",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "cnv_hete_profile",
                                       width = "700px",
                                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Homo CNV profile",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "cnv_homo_profile",
                                       width = "700px",
                                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "CNV Bar distribution",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "cnv_bar_dis",
                                       width = "700px",
                                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "CNV oncostrip",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "cnv_oncostrip",
                                       width = "700px",
                                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Exclusive CNV",solidHeader = TRUE,
                                     collapsible = TRUE,status = "primary",
                                     shiny::tags$br(),
                                     highcharter::highchartOutput(
                                       "cnv_exclusive",
                                       width = "700px",
                                       height = "100%")
          )
        )
        ),
        # load footer
        source(file.path(config$ui, "footer.R"))[1]
) # close tab