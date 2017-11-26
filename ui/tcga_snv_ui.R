tabItem(tabName = "tcga_snv", align = "center",
        shinyjs::useShinyjs(),
        
        ## SNV message --------------------------------------------
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>SNV
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Single Nucleotide Mutation</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>Single Nucleotide Mutation(SNV) is a variation in a single nucleotide that occurs at a specific position in the genome. 
The TCGA data is used to give you a visualization about SNV of you gene set for seleted cancer types.
                <br>GSAC offers different types of graphic layout
(heatmap, oncoplot, lollipop, survival, and mutation load, see details in <code>help page</code> below.) 
for you to visualize the SNV of your gene set for your seleted cancer types.</p>
                </div>
                </div>
                </div>
                </div>")
        ),
        
        # HELP as including of tcga_snv_help.R ---------------------
        source(file = file.path(config$wd, "ui", "tcga_snv_help.R"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="100%"),
        

        # cancer type selection and result output---------------------------------------------------
        fluidRow(
          # cancer type selection----
          column(width = 4,
                 shiny::tags$h3("Input"),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "Select cancers to do SNV analysis:", solidHeader = TRUE,
                                     collapsible = TRUE,status = "success",
                                     checkboxGroupInput(inputId = "snv_Kidney",
                                                        label = "Kidney",
                                                        choices = list("Kidney Chromophobe(KICH)"="KICH",
                                                                       "Kidney Renal Clear Cell Carcinoma(KIRC)"="KIRC",
                                                                       "Kidney Renal Papillary Cell Carcinoma(KIRP)"="KIRP")),
                                     checkboxGroupInput(inputId = "snv_Adrenal_Gland",
                                                        label = "Adrenal Gland",
                                                        choices = list("Adrenocortical Carcinoma(ACC)"="ACC",
                                                                       "Pheochromocytoma and Paraganglioma(PCPG)"="PCPG")),
                                     checkboxGroupInput(inputId = "snv_Brain",
                                                        label = "Brain",
                                                        choices = list("Glioblastoma Multiforme(GBM)"="GBM",
                                                                       "Brain Lower Grade Glioma(LGG)"="LGG")),
                                     checkboxGroupInput(inputId = "snv_Colorectal",
                                                        label = "Colorectal",
                                                        choices = list("Colon Adenocarcinoma(COAD)"="COAD",
                                                                       "Rectum Adenocarcinoma(READ)"="READ")),
                                     checkboxGroupInput(inputId = "snv_Colorectal",
                                                        label = "Colorectal",
                                                        choices = list("Colon Adenocarcinoma(COAD)"="COAD",
                                                                       "Rectum Adenocarcinoma(READ)"="READ")),
                                     checkboxGroupInput(inputId = "snv_Lung",
                                                        label = "Lung",
                                                        choices = list("Lung Adenocarcinoma(LUAD)"="LUAD",
                                                                       "Lung Squamous Cell Carcinoma(LUSC)"="LUSC")),
                                     checkboxGroupInput(inputId = "snv_Uterus",
                                                        label = "Uterus",
                                                        choices = list("Uterine Corpus Endometrial Carcinoma(UCEC)"="UCEC",
                                                                       "Uterine Carcinosarcoma(UCS)"="UCS")),
                                     checkboxGroupInput(inputId = "snv_Uterus",
                                                        label = "Uterus",
                                                        choices = list("Uterine Corpus Endometrial Carcinoma(UCEC)"="UCEC",
                                                                       "Uterine Carcinosarcoma(UCS)"="UCS")),
                                     checkboxGroupInput(inputId = "snv_Bile_Duct",
                                                        label = "Bile Duct",
                                                        choices = list("Cholangiocarcinoma(CHOL)"="CHOL")),
                                     checkboxGroupInput(inputId = "snv_Bile_Duct",
                                                        label = "Bile Duct",
                                                        choices = list("Bladder Urothelial Carcinoma(BLCA)"="BLCA")),
                                     checkboxGroupInput(inputId = "snv_Bone_Marrow",
                                                        label = "Bone Marrow",
                                                        choices = list("Acute Myeloid Leukemia(LAML)"="LAML")),
                                     checkboxGroupInput(inputId = "snv_Breast",
                                                        label = "Breast",
                                                        choices = list("Breast Invasive Carcinoma(BRCA)"="BRCA")),
                                     checkboxGroupInput(inputId = "snv_Cervix",
                                                        label = "Cervix",
                                                        choices = list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)"="CESC")),
                                     checkboxGroupInput(inputId = "snv_other",
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
                 shinydashboard::box(width = 12, title = "SNV summary plot",solidHeader = TRUE,
                     collapsible = TRUE,status = "primary",
                     shiny::tags$br(),
                     highcharter::highchartOutput(
                       "snv_summary",
                       width = "700px",
                       height = "100%")
                     ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "SNV percentage heatmap profile",solidHeader = TRUE,
                     collapsible = TRUE,status = "primary",
                     shiny::tags$br(),
                     highcharter::highchartOutput(
                       "snv_per",
                       width = "700px",
                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "SNV oncoplot",solidHeader = TRUE,
                     collapsible = TRUE,status = "primary",
                     shiny::tags$br(),
                     highcharter::highchartOutput(
                       "snv_oncoplot",
                       width = "700px",
                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "SNV oncostrip",solidHeader = TRUE,
                     collapsible = TRUE,status = "primary",
                     shiny::tags$br(),
                     highcharter::highchartOutput(
                       "snv_oncostrip",
                       width = "700px",
                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "SNV lollipop",solidHeader = TRUE,
                     collapsible = TRUE,status = "primary",
                     shiny::tags$br(),
                     highcharter::highchartOutput(
                       "snv_lollipop",
                       width = "700px",
                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "SNV survival",solidHeader = TRUE,
                     collapsible = TRUE,status = "primary",
                     shiny::tags$br(),
                     highcharter::highchartOutput(
                       "snv_sur",
                       width = "700px",
                       height = "100%")
                 ),
                 shiny::tags$br(),
                 shinydashboard::box(width = 12, title = "SNV mutation load",solidHeader = TRUE,
                     collapsible = TRUE,status = "primary",
                     shiny::tags$br(),
                     highcharter::highchartOutput(
                       "snv_mutload",
                       width = "700px",
                       height = "100%")
                 ),
                 shiny::tags$br()
                 )
        ),
        # load footer
        source(file.path(config$ui, "footer.R"))[1]
        
        
) # close tab