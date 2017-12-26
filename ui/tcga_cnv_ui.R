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
        # cancer type selection----
        cancerTypeInput("cnv"),
        
        # Selected cancer show ----
        shiny::tags$h3("Cancer Type Check",class="text-success"),
        shiny::tags$h4("The cancers you selected: ",
                       textOutput("cnv_selected_cancer"),
                       " Confirm and start analysis by click Submit!"),
        
        # Confirm and submit ----
        fluidRow(
          column(width = 4),
          column(
            width = 2, offset = 0,
            actionButton("cnv_submit", label = "Submit!", icon = icon("check"))
          ),
          column(
            width = 2, offset = 0,
            actionButton("cnv_reset", label = "Resect!", icon = icon("refresh")) # ,status = "danger"?
          ),
          column(width = 4)
        ),
        shiny::tags$hr(width="85%"),
        
        # output plot -------------------------------------------------------------
        # Tabset Panel
        fluidRow(
          column(width = 10,
                 offset = 1,
                 shiny::tags$br(),
                 shinydashboard::tabBox(id = "cnv_PLOT",title = "PLOT",width = 12,
                                        tabPanel(title="CNV Pie distribution",imagePlotInput(id="cnv_pie",width="100%",height="100%")),
                                        tabPanel(title= "Hete CNV profile",PlotInput(id="cnv_hete")),
                                        tabPanel(title="Homo CNV profile",PlotInput(id="cnv_homo")),
                                        tabPanel(title="CNV Bar distribution",PlotInput("cnv_bar")),
                                        tabPanel(title="CNV to Expression",PlotInput("cnv_exp"))
                                        # tabPanel(title="CNV oncostrip",PlotInput("cnv_oncostrip")),
                                        # tabPanel(title="Exclusive CNV",PlotInput("cnv_exclusive"))
                 )
          )
        ),
        
        # load footer ------------------------------------------------------
        source(file.path(config$ui, "footer.R"))[1]
        
        
) # close tab