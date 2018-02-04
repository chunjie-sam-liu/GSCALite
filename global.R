# source by server.R
# source by ui.R
# saved as functions_server.R

##########################################
# Cancer selection data get and comfim####
## 1. cancer select for each part ui ######
## 2. cancer type selection confirm  ######
## cancerTypeInput & cancerType############
##########################################

# GTEx normal tissue choice ##############

GTEx_expr_Brain_choice <- list("Brain" = "Brain")
GTEx_expr_Liver_choice <- list("Liver" = "Liver")
GTEx_expr_Heart_choice <- list("Heart" = "Heart")
GTEx_expr_Ovary_choice <- list("Ovary" = "Ovary")
GTEx_expr_Lung_choice <- list("Lung" = "Lung")
GTEx_expr_Breast_choice <- list("Breast" = "Breast")
GTEx_expr_Skin_choice <- list("Skin" = "Skin")
GTEx_expr_Blood_choice <- list("Blood" = "Blood")
GTEx_expr_Testis_choice <- list("Testis" = "Testis")
GTEx_expr_Colon_choice <- list("Colon" = "Colon")
GTEx_expr_other_choice <- list(
  "Adipose Tissue" = "Adipose Tissue", "Muscle" = "Muscle",
  "Blood Vessel" = "Blood Vessel", "Salivary Gland" = "Salivary Gland", "Adrenal Gland" = "Adrenal Gland",
  "Thyroid" = "Thyroid", "Spleen" = "Spleen", "Small Intestine" = "Small Intestine",
  "Cervix Uteri" = "Cervix Uteri", "Bladder" = "Bladder", "Fallopian Tube" = "Fallopian Tube",
  "Uterus" = "Uterus", "Pituitary" = "Pituitary", "Esophagus" = "Esophagus",
  "Nerve" = "Nerve", "Vagina" = "Vagina", "Pancreas" = "Pancreas", "Prostate" = "Prostate",
  "Stomach" = "Stomach", "Kidney" = "Kidney"
)
GTEx_expr_input_selection <- paste("input$", grep("GTEx_expr_.*_choice", ls(), value = T), sep = "")


##### GTEx tissue 4 UI#####
GTExTissueType <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # GTEx tissues selection----
      column(
        width = 10,
        offset = 1,
        shiny::tags$br(),
        shiny::tags$h3("GTEx tissues selection", class = "text-success"),
        shiny::tags$br(),

        shinydashboard::tabBox(
          width = 12, title = "Tissue",
          tabPanel(
            "Brain",
            shiny::tags$h4("Brain", class = "text-success"),
            checkboxGroupButtons(
              inputId = ns("Brain"), label = NULL,
              choices = GTEx_expr_Brain_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Liver",
            checkboxGroupButtons(
              inputId = ns("Liver"), label = NULL,
              choices = GTEx_expr_Liver_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Heart",
            checkboxGroupButtons(
              inputId = ns("Heart"), label = NULL,
              choices = GTEx_expr_Heart_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Ovary",
            checkboxGroupButtons(
              inputId = ns("Ovary"), label = NULL,
              choices = GTEx_expr_Ovary_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Lung",
            checkboxGroupButtons(
              inputId = ns("Lung"), label = NULL,
              choices = GTEx_expr_Lung_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Breast",
            checkboxGroupButtons(
              inputId = ns("Breast"), label = NULL,
              choices = GTEx_expr_Breast_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Skin",
            checkboxGroupButtons(
              inputId = ns("Skin"), label = NULL,
              choices = GTEx_expr_Skin_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Blood",
            checkboxGroupButtons(
              inputId = ns("Blood"), label = NULL,
              choices = GTEx_expr_Blood_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Testis",
            checkboxGroupButtons(
              inputId = ns("Testis"), label = NULL,
              choices = GTEx_expr_Testis_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Colon",
            checkboxGroupButtons(
              inputId = ns("Colon"), label = NULL,
              choices = GTEx_expr_Colon_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Other tissues",
            checkboxGroupButtons(
              inputId = ns("gtex_expr_other_tissue"), label = NULL,
              choices = GTEx_expr_other_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          )
        )
      ),
      shiny::tags$hr(width = "85%")
    )
  )
}



GTEx_normal_Tissue <- function(input, output, session) {
  GTEx_normal_tissue <- reactive({
    eval(parse(text = GTEx_expr_input_selection)) -> GTEx_normal_tissue
  })

  return(GTEx_normal_tissue)
}

GTEx_eqtl_Tissue <- function(input, output, session) {
  GTEx_eqtl_tissue <- reactive({
    eval(parse(text = GTEx_eqtl_input_selection)) -> GTEx_eqtl_tissue
  })

  return(GTEx_eqtl_tissue)
}

# sub normal tissue----------

sub_GTEx_tissue <- list(
  "Brain" = "Brain",
  "Liver" = "Liver",
  "Heart" = "Heart",
  "Ovary" = "Ovary",
  "Lung" = "Lung",
  "Breast" = "Breast",
  "Skin" = "Skin",
  "Blood" = "Blood",
  "Testis" = "Testis",
  "Colon" = "Colon",
  "gtex_expr_other_tissue" = c(
    "Adipose Tissue", "Muscle", "Blood Vessel",
    "Salivary Gland", "Adrenal Gland", "Thyroid",
    "Spleen", "Small Intestine", "Cervix Uteri",
    "Bladder", "Fallopian Tube", "Uterus",
    "Pituitary", "Esophagus", "Nerve",
    "Vagina", "Pancreas", "Prostate", "Stomach", "Kidney"
  )
)

resetGTExTissueType <- function(input, output, session) {
  for (i in c(tabPannel_element_ten, "other_tissue")) {
    shinyjs::reset(i)
  }
  GTEx_normal_tissue <- reactive({
    c("") -> GTEx_normal_tissue
  })
  return(GTEx_normal_tissue)
}



##### GTEx eqtl tissue ####
GTEx_eqtl_Brain_choice <- list("Brain" = "Brain")
GTEx_eqtl_Liver_choice <- list("Liver" = "Liver")
GTEx_eqtl_Heart_choice <- list("Heart" = "Heart")
GTEx_eqtl_Ovary_choice <- list("Ovary" = "Ovary")
GTEx_eqtl_Lung_choice <- list("Lung" = "Lung")
GTEx_eqtl_Breast_choice <- list("Breast" = "Breast")
GTEx_eqtl_Skin_choice <- list("Skin" = "Skin")
GTEx_eqtl_Blood_choice <- list("Blood" = "Blood")
GTEx_eqtl_Testis_choice <- list("Testis" = "Testis")
GTEx_eqtl_Colon_choice <- list("Colon" = "Colon")
GTEx_eqtl_other_choice <- list(
  "Adipose Tissue" = "Adipose Tissue", "Muscle" = "Muscle",
  "Artery" = "Artery", "Salivary Gland" = "Salivary Gland", "Adrenal Gland" = "Adrenal Gland",
  "Thyroid" = "Thyroid", "Spleen" = "Spleen", "Small Intestine" = "Small Intestine",
  "Uterus" = "Uterus", "Pituitary" = "Pituitary", "Esophagus" = "Esophagus",
  "Nerve" = "Nerve", "Vagina" = "Vagina", "Pancreas" = "Pancreas", "Prostate" = "Prostate",
  "Stomach" = "Stomach", "Artery" = "Artery", "Cells" = "Cells"
)
GTEx_eqtl_input_selection <- paste("input$", grep("GTEx_eqtl_.*_choice", ls(), value = T), sep = "")

GTExTissueeqtl <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # GTEx tissues selection----
      column(
        width = 10,
        offset = 1,
        shiny::tags$br(),
        shiny::tags$h3("GTEx tissues selection", class = "text-success"),
        shiny::tags$br(),

        shinydashboard::tabBox(
          width = 12, title = "Tissue",
          tabPanel(
            "Brain",
            shiny::tags$h4("Brain", class = "text-success"),
            checkboxGroupButtons(
              inputId = ns("Brain"), label = NULL,
              choices = GTEx_eqtl_Brain_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Liver",
            checkboxGroupButtons(
              inputId = ns("Liver"), label = NULL,
              choices = GTEx_eqtl_Liver_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Heart",
            checkboxGroupButtons(
              inputId = ns("Heart"), label = NULL,
              choices = GTEx_eqtl_Heart_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Ovary",
            checkboxGroupButtons(
              inputId = ns("Ovary"), label = NULL,
              choices = GTEx_eqtl_Ovary_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Lung",
            checkboxGroupButtons(
              inputId = ns("Lung"), label = NULL,
              choices = GTEx_eqtl_Lung_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Breast",
            checkboxGroupButtons(
              inputId = ns("Breast"), label = NULL,
              choices = GTEx_eqtl_Breast_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Skin",
            checkboxGroupButtons(
              inputId = ns("Skin"), label = NULL,
              choices = GTEx_eqtl_Skin_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Blood",
            checkboxGroupButtons(
              inputId = ns("Blood"), label = NULL,
              choices = GTEx_eqtl_Blood_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Testis",
            checkboxGroupButtons(
              inputId = ns("Testis"), label = NULL,
              choices = GTEx_eqtl_Testis_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Colon",
            checkboxGroupButtons(
              inputId = ns("Colon"), label = NULL,
              choices = GTEx_eqtl_Colon_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          ),
          tabPanel(
            "Other tissues",
            checkboxGroupButtons(
              inputId = ns("gtex_eqtl_other_tissue"), label = NULL,
              choices = GTEx_eqtl_other_choice,
              justified = TRUE,
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
              direction = "vertical",
              individual = TRUE
            )
          )
        )
      ),
      shiny::tags$hr(width = "85%")
    )
  )
}

sub_GTEx_eqtl_tissue <- list(
  "Brain" = "Brain",
  "Liver" = "Liver",
  "Heart" = "Heart",
  "Ovary" = "Ovary",
  "Lung" = "Lung",
  "Breast" = "Breast",
  "Skin" = "Skin",
  "Blood" = "Blood",
  "Testis" = "Testis",
  "Colon" = "Colon",
  "gtex_eqtl_other_tissue" = c(
    "Adipose Tissue", "Muscle", "Artery",
    "Salivary Gland", "Adrenal Gland",
    "Thyroid", "Spleen", "Small Intestine",
    "Uterus", "Pituitary", "Esophagus",
    "Nerve", "Vagina", "Pancreas", "Prostate",
    "Stomach", "Artery", "Cells"
  )
)
# cancer type choice ------------------------------------------------------

Kidney_choice <- list(
  "Kidney Chromophobe(KICH)" = "KICH",
  "Kidney Renal Clear Cell Carcinoma(KIRC)" = "KIRC",
  "Kidney Renal Papillary Cell Carcinoma(KIRP)" = "KIRP"
)
Adrenal_Gland_choice <- list(
  "Adrenocortical Carcinoma(ACC)" = "ACC",
  "Pheochromocytoma and Paraganglioma(PCPG)" = "PCPG"
)
Brain_choice <- list(
  "Glioblastoma Multiforme(GBM)" = "GBM",
  "Brain Lower Grade Glioma(LGG)" = "LGG"
)
Colorectal_choice <- list(
  "Colon Adenocarcinoma(COAD)" = "COAD",
  "Rectum Adenocarcinoma(READ)" = "READ"
)
Lung_choice <- list(
  "Lung Adenocarcinoma(LUAD)" = "LUAD",
  "Lung Squamous Cell Carcinoma(LUSC)" = "LUSC"
)
Uterus_choice <- list(
  "Uterine Corpus Endometrial Carcinoma(UCEC)" = "UCEC",
  "Uterine Carcinosarcoma(UCS)" = "UCS"
)
Bile_Duct_choice <- list("Bladder Urothelial Carcinoma(BLCA)" = "BLCA")
Bone_Marrow_choice <- list("Acute Myeloid Leukemia(LAML)" = "LAML")
Breast_choice <- list("Breast Invasive Carcinoma(BRCA)" = "BRCA")
Cervix_choice <- list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)" = "CESC")
other_tissue_choice <- list(
  "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma(DLBC)" = "DLBC",
  "Esophageal Carcinoma(ESCA)" = "ESCA",
  "Stomach Adenocarcinoma(STAD)" = "STAD",
  "Head and Neck Squamous Cell Carcinoma(HNSC)" = "HNSC",
  "Liver Hepatocellular Carcinoma(LIHC)" = "LIHC",
  "Mesothelioma(MESO)" = "MESO",
  "Ovarian Serous Cystadenocarcinoma(OV)" = "OV",
  "Pancreatic Adenocarcinoma(PAAD)" = "PAAD",
  "Prostate Adenocarcinoma(PRAD)" = "PRAD",
  "Sarcoma(SARC)" = "SARC",
  "Skin Cutaneous Melanoma(SKCM)" = "SKCM",
  "Testicular Germ Cell Tumors(TGCT)" = "TGCT",
  "Thyroid Carcinoma(THCA)" = "THCA",
  "Thymoma(THYM)" = "THYM",
  "Uveal Melanoma(UVM)" = "UVM",
  "Cholangiocarcinoma(CHOL)" = "CHOL"
)

# cancer type selection ---------------------------------------------------
cancerTypeInput <- function(id) {
  ns <- NS(id)

  tagList(
    # value box for selected cancer types ----
    fluidRow(shiny::uiOutput(outputId = ns("cancer_types_select"))),
    shiny::tags$hr(width = "85%")
  )
}

tissueTypeInput <- function(id) {
  ns <- NS(id)

  tagList(
    # value box for selected cancer types ----
    fluidRow(shiny::uiOutput(outputId = ns("tissue_types_select"))),
    shiny::tags$hr(width = "85%")
  )
}

# Value box for selection cancer types ------------------------------------


cancerTypesSelect <- function(input, output, session, .sctps) {
  output$cancer_types_select <- renderUI({
    shiny::tagList(
      column(
        width = 4, offset = 2,
        infoBox(
          title = "Number of selected cancers", value = length(.sctps),
          width = 12, color = "aqua", fill = TRUE
        ) # icon = icon("users"),
      ),
      column(
        width = 4,
        infoBox(
          title = "Number of unselected cancers", value = 33 - length(.sctps),
          width = 12, color = "red", fill = TRUE
        ) # icon = icon("credit-card"),
      ),
      column(
        width = 8, offset = 2,
        box(
          solidHeader = TRUE, status = "primary",
          title = "Selected Cancer Types", width = 12,
          paste0(.sctps, collapse = ", ")
        )
      )
    )
  })
}

tissueTypesSelect <- function(input, output, session, .sctps) {
  output$tissue_types_select <- renderUI({
    print(.sctps)
    shiny::tagList(
      column(
        width = 4, offset = 2,
        infoBox(
          title = "Number of selected tissues", value = length(.sctps),
          width = 12, color = "aqua", fill = TRUE
        ) # icon = icon("users"),
      ),
      column(
        width = 4,
        infoBox(
          title = "Number of unselected tissues", value = 30 - length(.sctps),
          width = 12, color = "red", fill = TRUE
        ) # icon = icon("credit-card"),
      ),
      column(
        width = 8, offset = 2,
        box(
          solidHeader = TRUE, status = "primary",
          title = "Selected Tissue Types", width = 12,
          paste0(.sctps, collapse = ", ")
        )
      )
    )
  })
}
# select and submit for UI----

selectAndAnalysisInput <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      column(
        width = 8, offset = 2,
        shinyBS::bsAlert(anchorId = ns("no_gene_set")),
        shinyBS::bsAlert(anchorId = ns("no_paired_sample"))
      )
    )
  )
}


# Simplified cancer types -------------------------------------------------

sub_cancer_types <- list(
  Kidney = c("KICH", "KIRC", "KIRP"),
  Adrenal_Gland = c("ACC", "PCPG"),
  Brain = c("GBM", "LGG"),
  Colorectal = c("COAD", "READ"),
  Lung = c("LUAD", "LUSC"),
  Uterus = c("UCEC", "UCS"),
  Bile_Duct = c("BLCA"),
  Bone_Marrow = c("LAML"),
  Breast = c("BRCA"),
  Cervix = c("CESC"),
  other_tissue = c("DLBC", "ESCA", "STAD", "HNSC", "LIHC", "MESO", "OV", "PAAD", "PRAD", "SARC", "SKCM", "TGCT", "THCA", "THYM", "UVM", "CHOL")
)


# Check and uncheck submit ------------------------------------------------

check_sub_cancer_types <- function(input, output, session, .cts, .check) {
  names(.cts) %>%
    purrr::walk(
      .f = function(.x) {
        .selected <- if (.check) .cts[[.x]] else character(0)
        updateCheckboxGroupButtons(
          session = session,
          inputId = .x, selected = .selected,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
        )
      }
    )
}


# Call by server to update check ------------------------------------------

selectAndAnalysis <- function(input, output, session, .id) {
  observeEvent(
    eventExpr = input$switch,
    handlerExpr = {
      if (input$switch) {
        check_sub_cancer_types(input, output, session, sub_cancer_types, TRUE)
        check_sub_cancer_types(input, output, session, sub_GTEx_tissue, TRUE)
        check_sub_cancer_types(input, output, session, sub_GTEx_eqtl_tissue, TRUE)
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} Select all {.id} @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      } else {
        check_sub_cancer_types(input, output, session, sub_cancer_types, FALSE)
        check_sub_cancer_types(input, output, session, sub_GTEx_tissue, FALSE)
        check_sub_cancer_types(input, output, session, sub_GTEx_eqtl_tissue, FALSE)
        print(glue::glue("{paste0(rep('-', 10), collapse = '')} Deselect all {.id} @ {Sys.time()} {paste0(rep('-', 10), collapse = '')}"))
      }
    }
  )
}


# cancerType server function ----------------------------------------------
# pair with cancerTypeInput in functions_ui.R##
# Call by *_*_server.R by callModule(cancerType,"id pair with UI part")
cancerType <- function(input, output, session) {
  reactive({
    c(
      input$Kidney, input$Adrenal_Gland, input$Brain, input$Colorectal,
      input$Lung, input$Uterus, input$Bile_Duct, input$Bone_Marrow, input$Breast,
      input$Cervix, input$other_tissue
    )
  })
}

resetcancerType <- function(input, output, session) {
  shinyjs::reset("Kidney")
  shinyjs::reset("Adrenal_Gland")
  shinyjs::reset("Brain")
  shinyjs::reset("Colorectal")
  shinyjs::reset("Lung")
  shinyjs::reset("Uterus")
  shinyjs::reset("Bile_Duct")
  shinyjs::reset("Bone_Marrow")
  shinyjs::reset("Breast")
  shinyjs::reset("Cervix")
  shinyjs::reset("other_tissue")
  cancer_type <- c("")
  # cancer_type <- reactive({
  #   c("") -> cancer_type
  # })
  return(cancer_type)
}



# remove pic when stop clicked ----------------------------------------------


removePic <- function(input, output, session, outtype) {
  if (outtype == "image") {
    output$plot <- renderImage({
      NULL
    })
  }
  if (outtype == "plot") {
    output$plot <- renderPlot({
      NULL
    })
  }
}


###############################################################
# Plot function to generate plot in ui#########################
## 1. plotoutout in ui and in server ###########################
## 2. draw specific pic type by calling different function  ######
## PlotInput & Plot#################################
###############################################################
# call in ui by PlotInput("cnv_pie",..) OR  PlotInput("cnv_bar",..)
# call in ser by callModule(Plot,"cnv_pie",...) OR ...

download_bt <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::dropdownButton(
      tags$h3("Download Options"),
      prettyRadioButtons(
        inputId = ns("pictype"),
        label = "Selcet format for your pictur",
        choices = list("PDF" = "pdf", "PNG" = "png","EPS"="eps"),
        inline = TRUE,
        icon = icon("check"),
        bigger = TRUE, status = "info",
        animation = "jelly"
      ),
      numericInput(
        inputId = ns("d_width"),
        label = "Width",
        value = 4,
        min = 1,
        max = 10
      ),
      
      numericInput(
        inputId = ns("d_height"),
        label = "Height",
        value = 6,
        min = 3,
        max = 20
      ),
      downloadButton(
        outputId = ns("picdownload"),
        label = "Download"
      ),
      circle = TRUE, status = "default",
      right = TRUE,
      icon = icon("download"), width = "300px",
      tooltip = shinyWidgets::tooltipOptions(title = "Click to download")
    )
  )
}
PlotInput <- function(id, width, height) {
  ns <- NS(id)

  tagList(
    column(
      width = 2, offset = 0,
      download_bt(id)
    ),
    column(
      width = 12, offset = 0,
      plotOutput(ns("plot")) %>% withSpinner(color = "#0dc5c1")
    )
  )
}

##################### GTEx expression heatmap plot by zhangq#########################

heatmap_GTEX_Plot <- function(input, output, session, data, status, downloadname) {
  # plot function
  plotinput <- function(){
    ggplot(data, aes(Tissue, GeneName)) +
      geom_tile(aes(fill = RPKM)) +
      geom_text(aes(label = RPKM)) +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Expression value of query genes in GTEx dataset") +
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) -> p
  }
  
  # get output
  output$plot <- renderPlot({
    status$analysis
    print(plotinput())
  })
  
  # get download output
  output$picdownload <-downloadHandler(
    filename = function() {
      paste(downloadname, ".", input$pictype, sep = "")
    },
    content = function(file){
      ggsave(file,plotinput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}

box_GTEx_GSVA_Plot <- function(input, output, session, data) {
  output$plot <- renderPlot({
    data %>%
      dplyr::group_by(SMTS) %>%
      dplyr::summarise(m = median(gsva)) %>%
      dplyr::arrange(m) %>%
      dplyr::pull(SMTS) -> lev
    tcc <- tibble(SMTS = lev, color = rainbow(length(lev)))
    data %>%
      dplyr::mutate(SMTS = factor(SMTS, levels = lev)) %>%
      ggplot(aes(x = SMTS, y = gsva)) +
      stat_boxplot(geom = "errorbar", width = 0.3) +
      geom_boxplot(outlier.colour = NA) +
      geom_point(aes(color = SMTS), position = position_jitter(width = 0.05), alpha = 0.4, size = 0.8) +
      scale_color_manual(name = "Tissues", values = dplyr::slice(tcc, match(lev, SMTS)) %>% dplyr::pull(color)) +
      theme(
        axis.line = element_line(color = "black"), axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90),
        panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = NA), panel.spacing.x = unit(0, "lines")
      ) +
      guides(color = F) + labs(x = "Tissue", y = "GSVA Score", title = "") -> p
    return(p)
  })
}




Plot <- function(input, output, session) { # data(raw data, gene set, cancer types),plot type(decide function type)
  # size <- reactive(as.numeric(input$num))
  # x <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  #   })
  # y <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  # })
  x <- rnorm(50)
  y <- nrow(50)

  output$plot <- renderPlot({
    plot(x, y)
  }) # fun argument decide what function will be called.
}

# cnv Point plot --------------------------------------------------------------


cnv_pointPlot <- function(input, output, session, data, cancer, gene, size, color, sizename, colorname, wrap, status_monitor, status,downloadname) {

  # Example: callModule(pointPlot,"cnv_pie",data=cnv_plot_ready_1,cancer="cancer_types",
  #                     gene="symbol",size="per",color="color",sizename="CNV%",
  #                     colorname="SCNA Type",wrap="~ effect")
  # data should include x/y, point size and point color.
  plotinput <- reactive({
    data %>%
      ggplot(aes_string(y = gene, x = cancer)) +
      geom_point(aes_string(size = size, color = color)) +
      xlab("Cancer type") +
      ylab("Symbol") +
      scale_size_continuous(
        name = sizename,
        breaks = c(0.05, 0.1, 0.2, 0.4, 0.6, 1),
        limits = c(0.05, 1),
        labels = c("5", "10", "20", "40", "60", "100")
      ) +
      ggthemes::scale_color_gdocs(
        name = colorname,
        labels = c("Deletion", "Amplification")
      ) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      facet_wrap(as.formula(wrap)) +
      theme(strip.text.x = element_text(size = 15)) -> p
  })
  output$plot <- renderPlot({
    status[[status_monitor]]
    print(plotinput())
  })
  output$picdownload <-downloadHandler(
    filename = function() {
      paste(downloadname, ".", input$pictype, sep = "")
    },
    content = function(file){
      ggsave(file,plotinput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}


# cnv pie plot ----------------------------------------------------------------

piePlot <- function(input, output, session, data, y, fill, facet_grid, outfile, height, status_monitor, status,downloadname) {
  # Example:
  # callModule(piePlot,"cnv_pie",data=pie_plot_ready,y="per",
  #            fill="type",facet_grid="cancer_types ~ symbol")
  # data should include ...

  imgplotInput <- reactive({
    data %>%
      ggplot(aes_string(x = factor(1), y = y, fill = fill)) +
      geom_bar(stat = "identity", position = "stack", color = NA) +
      # scale_y_continuous(limits = c(0,1))
      coord_polar("y") +
      facet_grid(as.formula(facet_grid)) + # cancer_types ~ symbol
      # scale_x_discrete(limits = cnv_gene_rank$symbol) +
      # scale_x_discrete(expand=c(0,0)) +
      # scale_y_discrete(expand=c(0,0)) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),

        strip.text.y = element_text(angle = 0, hjust = 0, size = 4),
        strip.text.x = element_text(size = 4, angle = 90, vjust = 0),
        strip.background = element_blank(),

        legend.title = element_blank(),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "cm"),

        panel.background = element_blank(),
        panel.spacing = unit(0, "null"), # unit(0.01, "lines"),
        panel.spacing.x = unit(0, "null"),

        plot.margin = rep(unit(0, "null"), 4),
        axis.ticks.length = unit(0, "cm")
      ) +
      scale_fill_manual(
        limits = c("a_hete", "a_homo", "d_hete", "d_homo", "other"),
        label = c("Hete Amp", "Homo Amp", "Hete Del", "Homo Del", "None"),
        # Amp RColorBrewer name = "Spectral"
        # Del RColorBrewer name = "BrBG"
        values = c("brown1", "brown4", "aquamarine3", "aquamarine4", "grey")
      ) -> p
  })
  
  output$plot <- renderImage({
    status[[status_monitor]]
    # outfile <- paste("/project/huff/huff/github/GSCALite/userdata","/","TCGA_cnv_pie_rellation_network",'.png',sep="")
    ggsave(outfile, imgplotInput(), device = "png", width = 4, height = height)
    list(
      src = outfile,
      contentType = "image/png",
      # width = 400,
      # height = "900px",
      alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  
  output$picdownload <- downloadHandler(
    filename = function() {
      paste(downloadname, ".", input$pictype, sep = "")
    },
    content = function(file) {
      ggsave(file, imgplotInput(), device = input$pictype, width = input$d_width, height = input$d_height)
    }
  )
}


# gene set CNV frenquencey in each cancer ---------------------------------
# bar stak plot
cnvbarPlot <- function(input, output, session, data, x, y, fill, status_monitor, status, downloadname) {
  # Example:
  # callModule(piePlot,"cnv_pie",data=pie_plot_ready,y="per",
  #            fill="type",facet_grid="cancer_types ~ symbol")
  # data should include ...
  plotinput <- reactive({
    data %>%
      ggplot(aes_string(x = x, y = y, fill = fill)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~cnv_type, ncol = 2) +
      theme(strip.text.x = element_text(size = 15)) +
      ggsci::scale_fill_npg(
        name = "Type",
        limits = c("amp_a", "amp_s", "del_a", "del_s"),
        labels = c("Amp", "Amp Only", "Del", "Del Only")
      ) +
      ggthemes::theme_gdocs() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3)
      ) +
      labs(x = "Cancer Types", y = "CNV Frequency") -> p
  })
  
  output$plot <- renderPlot({
    status[[status_monitor]]
    print(plotinput())
  })
  
  output$picdownload <- downloadHandler(
    filename = function(){
      paste(downloadname,".",input$pictype, sep='')
    },
    content = function(file) {
      ggsave(file,plotinput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}


# snv percentage plot -----------------------------------------------------

snv_per_heatmap <- function(input, output, session, data, cancer, gene, fill, label, cancer_rank, gene_rank, status_monitor, status, downloadname) {
  plotInput <- reactive({
    data %>%
      ggplot(aes_string(x = cancer, y = gene, fill = fill)) +
      geom_tile() +
      geom_text(aes_string(label = label)) +
      scale_x_discrete(position = "top", limits = cancer_rank$x_label) +
      scale_y_discrete(limits = gene_rank$symbol) +
      scale_fill_gradient2(
        name = "Mutation Frequency (%)",
        limit = c(0, 0.8),
        breaks = c(seq(0, 0.2, 0.05), seq(0.25, 0.65, 0.1)),
        label = c("0", "5", "10", "15", "20", "25", "35", "45", "55", "65"),
        high = "red",
        na.value = "white"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = -0.05, size = "10"),
        axis.title.y = element_text(size = "15"),
        panel.grid = element_line(colour = "grey", linetype = "dashed")
      ) +
      guides(fill = guide_legend(
        title = "Mutation Frequency (%)",
        title.position = "left",
        title.theme = element_text(angle = 90, vjust = 2),
        reverse = T,
        keywidth = 0.6,
        keyheight = 0.8
      )) +
      labs(x = "", y = "") -> p
  })
  
  output$plot <- renderPlot({
    # data$per %>% max() ->max.limit
    # max.limit/10 -> inter.limit
    status[[status_monitor]]
    print(plotInput())
  })
  
  output$picdownload <- downloadHandler(
    filename = function() { paste(downloadname, '.',input$pictype, sep='') },
    content = function(file) {
      ggsave(file,plotInput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}


# snv survival point plot -------------------------------------------------

snv_sur_pointPlot <- function(input, output, session, data, cancer, gene, size, color, cancer_rank, gene_rank, sizename, colorname, title, status_monitor, status, downloadname) {
  # Example: callModule(pointPlot,"cnv_pie",data=cnv_plot_ready_1,cancer="cancer_types",
  #                     gene="symbol",size="per",color="color",sizename="CNV%",
  #                     colorname="SCNA Type",wrap="~ effect")
  # data should include x/y, point size and point color.
  plotInput <- reactive({
    data %>%
      ggplot(aes_string(y = gene, x = cancer)) +
      geom_point(aes_string(size = size, color = color)) +
      labs(title = title) +
      xlab("Cancer type") +
      ylab("Symbol") +
      scale_x_discrete(limit = cancer_rank$cancer_types) +
      scale_y_discrete(limit = gene_rank$symbol) +
      scale_size_continuous(
        name = sizename,
        breaks = c(-log10(0.05), 5, 10, 15),
        limits = c(-log10(0.05), 15),
        labels = c("0.05", latex2exp::TeX("$10^{-5}$"), latex2exp::TeX("$10^{-10}$"), latex2exp::TeX("$< 10^{-15}$"))
      ) +
      ggthemes::scale_color_gdocs(
        name = colorname,
        labels = c("High", "Low")
      ) +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        panel.background = element_rect(colour = "black", fill = "white"),
        panel.grid = element_line(colour = "grey", linetype = "dashed"),
        panel.grid.major = element_line(
          colour = "grey",
          linetype = "dashed",
          size = 0.2
        ),
        plot.title = element_text(size = 20),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
      ) -> p
  })
  
  output$plot <- renderPlot({
    status[[status_monitor]]
    print(plotInput())
  })
  
  output$picdownload <- downloadHandler(
    filename = function() { paste(downloadname, '.',input$pictype, sep='') },
    content = function(file) {
      ggsave(file,plotInput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}



# snv maf summary ---------------------------------------------------------

# 1. ui part -----------------------------------------------------------------

imagePlotInput <- function(id, width="100%", height=300) {
  ns <- NS(id)
  shiny::tagList(
    column(
      width = 2, offset = 0,
      shinyWidgets::dropdownButton(
        tags$h3("Download Options"),
        prettyRadioButtons(
          inputId = ns("pictype"),
          label = "Selcet format for your pictur",
          choices = list("PDF" = "pdf", "PNG" = "png","EPS"="eps"),
          inline = TRUE,
          icon = icon("check"),
          bigger = TRUE, status = "info",
          animation = "jelly"
        ),
        numericInput(
          inputId = ns("d_width"),
          label = "Width",
          value = 4,
          min = 1,
          max = 10
        ),

        numericInput(
          inputId = ns("d_height"),
          label = "Height",
          value = 6,
          min = 3,
          max = 20
        ),
        downloadButton(
          outputId = ns("picdownload"),
          label = "Download"
        ),
        circle = TRUE, status = "default",
        icon = icon("download"), width = "300px",
        tooltip = shinyWidgets::tooltipOptions(title = "Click to download")
      )
    ),
    br(),
    br(),
    br(),
    br(),
    column(
      width = 12, offset = 0,
      imageOutput(ns("plot"), width = width, height = height) %>% withSpinner(color = "#0dc5c1")
    )
  )
}

# 2. server part ----------------------------------------------------------

snv_maf_summaryPlot <- function(input, output, session, gene_list_maf, outfile, status_monitor, status, downloadname) {
  plotInput <- reactive({
    maftools::plotmafSummary(gene_list_maf, fs = 3, statFontSize = 2) -> p
    p$plot
  })
  output$plot <- renderImage({
    status[[status_monitor]]
    # png(outfile, width = 1000, height= 700)
    
    # dev.off()
    ggsave(plotInput(), filename = outfile, device = "png", width = 3, height = 2)
    list(
      src = outfile,
      contentType = "image/png",
      # width = 1000,
      # height = 700,
      alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  
  output$picdownload <- downloadHandler(
    filename = function() { paste(downloadname, '.',input$pictype, sep='') },
    content = function(file) {
      ggsave(file,plotInput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}

snv_maf_oncoPlot <- function(input, output, session, gene_list_maf, pancan_color, outfile, status_monitor, status, downloadname) {
  plotInput <- function(){
    col <- RColorBrewer::brewer.pal(n = 8, name = "Paired")
    names(col) <- c(
      "Frame_Shift_Del", "Missense_Mutation", "Nonsense_Mutation", "Multi_Hit", "Frame_Shift_Ins",
      "In_Frame_Ins", "Splice_Site", "In_Frame_Del"
    )
    gene_list_maf %>% maftools::getClinicalData() %>% dplyr::select(Cancer_Types) %>% unique() %>% t() %>% as.character() -> snv_maf_cancer_type
    pancan_color %>%
      dplyr::filter(cancer_types %in% snv_maf_cancer_type) %>%
      dplyr::select(color, cancer_types) -> snv_maf_cancer_type_color
    
    fabcolors <- snv_maf_cancer_type_color$color
    names(fabcolors) <- snv_maf_cancer_type_color$cancer_types
    
    fabcolors <- list(Cancer_Types = fabcolors)
    if (length(snv_maf_cancer_type) > 1) {
      maftools::oncoplot(
        # my_oncoplot(
        maf = gene_list_maf, removeNonMutated = T, colors = col,
        clinicalFeatures = "Cancer_Types", sortByMutation = TRUE, sortByAnnotation = TRUE,
        annotationColor = fabcolors, top = 10
      )
    } else {
      maftools::oncoplot(
        # my_oncoplot(
        maf = gene_list_maf, removeNonMutated = T, colors = col,
        clinicalFeatures = "Cancer_Types", sortByMutation = TRUE, # sortByAnnotation = TRUE,
        annotationColor = fabcolors, top = 10
      )
    }
  }
  output$plot <- renderImage({
    status[[status_monitor]]
    png(outfile, width = 800, height = 600)
    plotInput()
    # maftools::oncoplot(maf = gene_list_maf, top = 10)#, fontSize = 12
    dev.off()
    list(
      src = outfile,
      contentType = "image/png",
      width = 800,
      height = 600,
      alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  
  fn_save <- function(.file,.pictyp,width,height){
    print(.file)
    if(.pictyp == "png"){
      png(.file, width, height,units ="in",res=1200)
      plotInput()
      dev.off()
    } else{
      pdf(.file, width, height)
      plotInput()
      dev.off()
    }
  }
  output$picdownload <- downloadHandler(
    filename = function() {paste(downloadname, '.',input$pictype, sep='') },
    content = function(filename) {
      fn_save(.file=filename, .pictyp=input$pictype, width = input$d_width,height = input$d_height)
    }
  )
}


# methylation plot --------------------------------------------------------


# 1. methy diff -----------------------------------------------------------
methy_diff_pointPlot <- function(input, output, session, data, cancer, gene, size, color, cancer_rank, gene_rank, sizename, colorname, title, status_monitor, status, downloadname) {
  plotinput <- reactive({
    CPCOLS <- c("red", "white", "blue")
    data %>%
      ggplot(aes_string(y = gene, x = cancer)) +
      geom_point(aes_string(size = size, color = color)) +
      scale_y_discrete(limit = gene_rank$symbol) +
      scale_x_discrete(limit = cancer_rank$cancer_types) +
      labs(title = title) +
      ylab("Symbol") +
      xlab("Cancer types") +
      scale_size_continuous(
        name = sizename # "-Log10(FDR)"
      ) +
      scale_color_gradient2(
        name = colorname, # "Methylation diff (T - N)",
        low = CPCOLS[3],
        mid = CPCOLS[2],
        high = CPCOLS[1]
      ) +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(colour = "black", fill = "white"),
        panel.grid = element_line(colour = "grey", linetype = "dashed"),
        panel.grid.major = element_line(
          colour = "grey",
          linetype = "dashed",
          size = 0.2
        ),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 40, size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.key = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 20)
      ) -> p
  })
  
  output$plot <- renderPlot({
    status[[status_monitor]]
    print(plotinput())
  })
  
  output$picdownload <- downloadHandler(
    filename = function() { paste("picture", '.',input$pictype, sep='') },
    content = function(file) {
      ggsave(file,plotinput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}




# rppa --------------------------------------------------------------------
# line contact ----
rppa_line_contact <- function(plot_seg, cancer.text, gene.text, path.text) {
  
  ggplot() -> p
  for (cancers in plot_seg$Cancer %>% unique()) {
    # cancers="LUSC"
    plot_seg %>%
      dplyr::filter(Cancer == cancers) -> data
    curvature <- runif(1, 0.1, 0.3)
    p +
      geom_curve(
        data = data, mapping = aes(
          x = x1,
          y = y1,
          xend = x2,
          yend = y2,
          colour = Cancer,
          linetype = Regulation
        ),
        # colour = "red",
        curvature = curvature
      ) -> p
  }
  
  p +
    guides(color = FALSE) +
    geom_text(
      data = cancer.text,
      mapping = aes(x = x, y = y, label = text, color = text),
      hjust = 1,
      size = 2
    ) +
    geom_text(
      data = gene.text,
      mapping = aes(x = x - 0.4, y = y, label = text),
      hjust = 0,
      size = 2
    ) +
    geom_text(
      data = path.text,
      mapping = aes(x = x, y = y, label = text),
      hjust = 0,
      size = 2
    ) +
    expand_limits(x = c(-1, 10)) +
    theme(
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      # text = element_text(size=5),
      plot.title = element_text(hjust = 0.5, size = 7),
      plot.margin = rep(unit(0, "null"), 4),
      legend.position = "bottom",
      legend.text = element_text(size = 3),
      legend.key.size = unit(0.25, "cm"),
      legend.title = element_text(size = 4)
    ) +
    xlab("") +
    ylab("") +
    labs(title = "Relation network between genes' expression and cancer related pathways' activity.") -> p
}

# rppa pie ----
rppaPiePlot <- function(input, output, session, data, y, fill, facet_grid, height, outfile, status, downloadname) {
  # Example:
  # callModule(piePlot,"cnv_pie",data=pie_plot_ready,y="per",
  #            fill="type",facet_grid="cancer_types ~ symbol")
  # data should include ...

  imgInput <- function(){
    data %>%
      ggplot(aes_string(x = factor(1), y = y, fill = fill)) +
      geom_bar(stat = "identity", position = "stack", color = NA) +
      # scale_y_continuous(limits = c(0,1))
      coord_polar("y") +
      facet_grid(as.formula(facet_grid)) + #  symbol~ cancer_types
      # scale_x_discrete(limits = cnv_gene_rank$symbol) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),

        strip.text.y = element_text(angle = 0, hjust = 0, size = 5),
        strip.text.x = element_text(size = 5, angle = 90, vjust = 0),
        strip.background = element_blank(),

        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.25, "cm"),
        legend.position = "bottom",

        panel.background = element_blank(),
        panel.spacing = unit(0.02, "lines"),
        plot.margin = rep(unit(0, "null"), 4),
        axis.ticks.length = unit(0, "cm")
      ) +
      scale_fill_manual(
        limits = c("Activation", "Inhibition", "None"),
        label = c("Activation", "Inhibition", "None"),
        # Amp RColorBrewer name = "Spectral"
        # Del RColorBrewer name = "BrBG"
        values = c("brown1", "aquamarine3", "grey")
      ) -> p
  }
  
  output$plot <- renderImage({
    status$analysis
    ggsave(outfile, imgInput(), device = "png", width = 4, height = height)
    list(
      src = outfile,
      contentType = "image/png",
      # width = "100%" ,
      # height = 900,
      alt = "This is alternate text"
    )
  }, deleteFile = TRUE)
  
  output$picdownload <- downloadHandler(
    filename = function() {
      paste(downloadname, ".", input$pictype, sep = "")
    },
    content = function(file) {
      ggsave(file, imgInput(), device = input$pictype, width = input$d_width, height = input$d_height)
    }
  )
}

# rppa heatmap percent ----
rppa_heat_per <- function(input, output, session, rppa_per_ready, pathway, symbol, per, height, outfile, status, downloadname) {
  plotInput <- function(){
    rppa_per_ready %>%
      ggplot(aes(x = pathway, y = symbol)) +
      xlab("Pathway") + ylab("Symbol") +
      guides(fill = guide_colorbar("Percent")) +
      geom_tile(aes(fill = per), col = "white") +
      geom_text(
        label = ceiling(rppa_per_ready$per),
        size = 1
      ) +
      scale_fill_gradient2(
        high = "red",
        mid = "white",
        low = "blue"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 5),
        axis.text.y = element_text(size = 5),
        legend.key.size = unit(0.25, "cm"),
        legend.position = "bottom",
        plot.margin = rep(unit(0, "null"), 4),
        axis.ticks.length = unit(0, "cm"),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        legend.title = element_text(size = 6)
      ) +
      xlab("Pathway (a:activate; i:inhibit)") -> p
  }
  
  output$plot <- renderImage({
    status$analysis
    ggsave(outfile, plotInput(), device = "png", width = 4, height = height)
    list(
      src = outfile,
      contentType = "image/png",
      # width = "100%" ,
      # height = 900,
      alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  
  output$picdownload <- downloadHandler(
    filename = function() { paste(downloadname, '.',input$pictype, sep='') },
    content = function(file) {
      ggsave(file,plotInput(),device = input$pictype,width = input$d_width,height = input$d_height)
    }
  )
}





#### GTEx eqtl table output-------------

GTEx_eqtl_Output <- function(id) {
  ns <- NS(id)
  column(
    width = 10, offset = 1,
    shinydashboard::tabBox(
      id = "gtex_eqtl_table", title = "TABLE", width = 12,
      # datatable
      tabPanel(
        title = "Table of eQTL in GTEX dataset",
        DT::dataTableOutput(outputId = ns("gtex_eqtl_dt"))
      )
    )
  )
}