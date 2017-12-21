# sourced by 'ui.R'
# save as 'tcga_meth_ui.R'
# ui elements 'tcga_meth' sub tab of 'tcga' tab

tabItem(
  tabName = "tcga_meth", align = "center",
  shinyjs::useShinyjs(),

  ## meth message ----
  fluidRow(
    style = "width:80%;",
    HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>Methy
                      <font color='#777777'>
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Methylation</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>TCGA methylation data will be used to give you a visualization of you gene set for seleted cancer types.
                      <br>GSAC offers different types of results (Differential Methylation, Heatmap, Boxplot, Survival, see details in <code>help page</code> below.) for you to visualize the meth of your gene set for your seleted cancer types.</p>
                      </div>
                      </div>
                      </div>
                      </div>")
  ),
  ## Hlep message including in tcga_meth_help.ui----
  source(file.path(config$ui, "tcga_meth_help.R"))[1],

  shiny::tags$br(),
  shiny::tags$hr(width = "85%"),

  # cancer type selection and result output---------------------------------------------------
  # cancer type selection----
  cancerTypeInput("meth"),

  # Selected cancer show ----
  shiny::tags$h3("Cancer Type Check", class = "text-success"),
  shiny::tags$h4(
    "The cancers you selected: ",
    textOutput("meth_selected_cancer"),
    " Confirm and start analysis by click Submit!"
  ),

  # Confirm and submit ----
  fluidRow(
    column(width = 4),
    column(
      width = 2, offset = 0,
      actionButton("meth_submit", label = "Submit!", icon = icon("check"))
    ),
    column(
      width = 2, offset = 0,
      actionButton("meth_reset", label = "Resect!", icon = icon("refresh")) # ,status = "danger"?
    ),
    column(width = 4)
  ),
  shiny::tags$hr(width = "85%"),

  # output plot -------------------------------------------------------------
  # Tabset Panel
  fluidRow(
    column(
      width = 10,
      offset = 1,
      shiny::tags$br(),
      shinydashboard::tabBox(
        id = "snv_PLOT", title = "PLOT", width = 12,
        tabPanel(title = "Differential Methylation", PlotInput(id = "meth_diff")),
        tabPanel(title = "Methylation Survival", PlotInput(id="meth_survival")),
        tabPanel(title = "Methylation to Expression", PlotInput(id="meth_exp"))
        # tabPanel(title="SNV oncoplot",plotOutput("snv_oncoplot-plot")),
        # # tabPanel(title="SNV oncostrip",PlotInput("snv_oncostrip")),
        # # tabPanel(title="SNV lollipop",PlotInput("snv_lollipop")),
        # tabPanel(title="SNV survival",PlotInput("snv_survival"))
        # #tabPanel(title="SNV mutation load",PlotInput("snv_mut_load"))
      )
    )
  ),
  # load footer
  source(file.path(config$ui, "footer.R"))[1]
) # close tab