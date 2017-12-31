# sourced from "ui.R"
# save as "function_ui.R"
# function for shiny ui


# cancer type selection----
# Type 2 cancer type selection
# Similar to <cancerType> in global.R, not be used when global cancerType is using.
cancer1Input <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Cancer Type Selection", solidHeader = TRUE,
      background = "olive", width = 12, height = NULL,
      collapsible = FALSE, collapsed = FALSE,
      navlistPanel(
        well = TRUE, fluid = TRUE,
        tabPanel(
          "Kidney",
          checkboxGroupInput(
            inputId = ns("Kidney"), label = NULL,
            choices = list(
              "Kidney Chromophobe(KICH)" = "KICH",
              "Kidney Renal Clear Cell Carcinoma(KIRC)" = "KIRC",
              "Kidney Renal Papillary Cell Carcinoma(KIRP)" = "KIRP"
            )
          )
        ),
        tabPanel(
          "Adrenal Gland",
          checkboxGroupInput(
            inputId = ns("Adrenal_Gland"), label = NULL,
            choices = list(
              "Adrenocortical Carcinoma(ACC)" = "ACC",
              "Pheochromocytoma and Paraganglioma(PCPG)" = "PCPG"
            )
          )
        ),
        tabPanel(
          "Brain",
          checkboxGroupInput(
            inputId = ns("Brain"), label = NULL,
            choices = list(
              "Glioblastoma Multiforme(GBM)" = "GBM",
              "Brain Lower Grade Glioma(LGG)" = "LGG"
            )
          )
        ),
        tabPanel(
          "Colorectal",
          checkboxGroupInput(
            inputId = ns("Colorectal"), label = NULL,
            choices = list(
              "Colon Adenocarcinoma(COAD)" = "COAD",
              "Rectum Adenocarcinoma(READ)" = "READ"
            )
          )
        ),
        tabPanel(
          "Lung",
          checkboxGroupInput(
            inputId = ns("Lung"), label = NULL,
            choices = list(
              "Lung Adenocarcinoma(LUAD)" = "LUAD",
              "Lung Squamous Cell Carcinoma(LUSC)" = "LUSC"
            )
          )
        ),
        tabPanel(
          "Uterus",
          checkboxGroupInput(
            inputId = ns("Uterus"), label = NULL,
            choices = list(
              "Uterine Corpus Endometrial Carcinoma(UCEC)" = "UCEC",
              "Uterine Carcinosarcoma(UCS)" = "UCS"
            )
          )
        ),
        tabPanel(
          "Bile Duct",
          checkboxGroupInput(
            inputId = ns("Bile_Duct"), label = NULL,
            choices = list("Bladder Urothelial Carcinoma(BLCA)" = "BLCA")
          )
        ),
        tabPanel(
          "Bone Marrow",
          checkboxGroupInput(
            inputId = ns("Bone_Marrow"), label = NULL,
            choices = list("Acute Myeloid Leukemia(LAML)" = "LAML")
          )
        ),
        tabPanel(
          "Breast",
          checkboxGroupInput(
            inputId = ns("Breast"), label = NULL,
            choices = list("Breast Invasive Carcinoma(BRCA)" = "BRCA")
          )
        ),
        tabPanel(
          "Cervix",
          checkboxGroupInput(
            inputId = ns("Cervix"), label = NULL,
            choices = list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)" = "CESC")
          )
        ),
        tabPanel(
          "Other tissues",
          checkboxGroupInput(
            inputId = ns("other_tissue"), label = NULL,
            choices = list(
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
              "Uveal Melanoma(UVM)" = "UVM"
            )
          )
        )
      )
    )
  )
}
