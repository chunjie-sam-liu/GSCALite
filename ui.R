# save as 'ui.R'
# shiny ui


# Load library ------------------------------------------------------------
# For shiny
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

library(magrittr)

library(highcharter)
library(DT)
# library(maftools)

library(grid)

# For network
library(igraph)
library(networkD3)


# Load configuration ------------------------------------------------------

source(file = "config.R", local = TRUE)

# Load ui function --------------------------------------------------------

source(file = file.path(config$ui, "functions_ui.R"), local = TRUE)


# Load global module ------------------------------------------------------

source(file = file.path(config$wd, "global.R"), local = TRUE)

# Repeated ui stuff for modals --------------------------------------------

addReport_modelTrivia <- tagList(
  shiny::tags$br(),
  shiny::tags$br(),
  shiny::tags$p("The report can be downloaded in the Report section.")
)

jscode <- "shinyjs.collapse = function(boxid) {$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();}"


# Header Start ------------------------------------------------------------

header <- dashboardHeader(
  # Title
  title = HTML(paste(
    img(
      src = "./imgs/01.GSCA_logo_01.png",
      align = "middle",
      class = "img-responsvie",
      style = "height:55px !important;"
    ), ""
  )),

  dropdownMenuOutput("infoMenu"),

  dropdownMenuOutput("logMenu")
)

# Header End --------------------------------------------------------------


# Sidebar Start -----------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Welcome ----
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),



    # TCGA ----
    menuItem(
      "TCGA Cancer",
      tabName = "tcga",
      icon = icon("thumbs-up"),
      collapsible = TRUE,
      menuSubItem("mRNA Expression", tabName = "tcga_expr"),
      menuSubItem("Single Nucleotide Variation", tabName = "tcga_snv"),
      menuSubItem("Copy Number Variation", tabName = "tcga_cnv"),
      menuSubItem("Methylation", tabName = "tcga_meth"),
      menuSubItem("Pathway Activity", tabName = "tcga_rppa"),
      menuSubItem("miRNA Network", tabName = "tcga_mirna")
    ),

    # Drug ----
    menuItem(
      "Drug Response",
      tabName = "drug",
      icon = icon("list"),
      collapsible = TRUE,
      menuSubItem("GDSC", tabName = "gdsc"),
      menuSubItem("CTRP", tabName = "ctrp")
    ),

    # GTEx ----
    menuItem(
      "GTEx Normal Tissue",
      tabName = "gtex",
      icon = icon("gear"),
      collapsible = TRUE,
      menuSubItem("GTEx expression", tabName = "gtex_expr"),
      menuSubItem("GTEx eQTL", tabName = "GTEx_eqtl")
    ),

    # Downloads ----
    # menuItem("Report", tabName = "downloads", icon = icon("floppy-o")),

    # Help ----
    menuItem(
      "Help",
      tabName = "help",
      icon = icon("question")
    ),

    # About ----
    menuItem("About", tabName = "about", icon = icon("graduation-cap"))
  )
)

# Sidebar End -------------------------------------------------------------

# Body Start --------------------------------------------------------------

body <- dashboardBody(
  shiny::tags$head(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script = file.path(config$wd, "www", "js", "gscalite.js")),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
    shiny::tags$script(type = "text/javascript", src = "js/main.js")
    # shiny::tags$style(HTML(config$stylesheet)),
    # shiny::includeScript(file.path(config$wd, "www", "js", "tooltip-delay.js"))
  ),

  # Main body ----
  tabItems(

    # Welcome ----
    source(file = file.path(config$wd, "ui", "welcome_ui.R"), local = TRUE)$value,


    # GTEx ----

<<<<<<< HEAD


    source(file = file.path(config$wd, "ui", "GTEx_exp_ui.R"), local =TRUE)$value,
    source(file = file.path(config$wd, "ui", "GTEx_eqtl_ui.R"), local =TRUE)$value,

#    source(file = file.path(config$wd, "ui", "GTEx_exp_ui.R"), local = TRUE)$value,
#    source(file = file.path(config$wd, "ui", "GTEx_eqtl_ui.R"), local = TRUE)$value,

=======
    source(file = file.path(config$wd, "ui", "GTEx_exp_ui.R"), local = TRUE)$value,
    source(file = file.path(config$wd, "ui", "GTEx_eqtl_ui.R"), local = TRUE)$value,
>>>>>>> fd66611be66888e0dad93bcc01abc61475e289a6


    # TCGA ----
    # expr ----
#    source(file = file.path(config$wd, "ui", "tcga_expr_ui.R"), local = TRUE)$value,
    # cnv ----
#    source(file = file.path(config$wd, "ui", "tcga_cnv_ui.R"), local = TRUE)$value,
    # snv ----
#    source(file = file.path(config$wd, "ui", "tcga_snv_ui.R"), local = TRUE)$value,

    # meth ----
#    source(file = file.path(config$wd, "ui", "tcga_meth_ui.R"), local = TRUE)$value,

    # rppa ----
#    source(file = file.path(config$wd, "ui", "tcga_rppa_ui.R"), local = TRUE)$value,
    # mirna ----
#    source(file = file.path(config$wd, "ui", "tcga_mirna_ui.R"), local = TRUE)$value
    # Drug ----
    # gdsc
    # source(file = file.path(config$wd, "ui", "tcga_gdsc_ui.R"), local = TRUE)$value

    # ctrp
    # source(file = file.path(config$wd, "ui", "tcga_ctrp_ui.R"), local = TRUE)$value

    # Download ----

    # Help ----
    source(file = file.path(config$wd, "ui", "help_ui.R"), local = TRUE)$value,
    # About ----
    source(file = file.path(config$wd, "ui", "about_ui.R"), local = TRUE)$value
  )

  # Modals ----
  # source(file = file.path(config$wd, "ui", "modals_ui.R"), local = TRUE)$value
)


# Body End ----------------------------------------------------------------


# Shiny UI ----------------------------------------------------------------
shinyUI(dashboardPage(
  title = "GSCA - Gene Set Cancer Analysis",
  header = header,
  sidebar = sidebar,
  body = body
))

# Test --------------------------------------------------------------------
# shinyApp(ui = ui, server = function(input, output, session){})
