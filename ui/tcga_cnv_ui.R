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
                <p class='lead text-left'>TCGA CNV data will be used to give you a visualization of you gene set for seleted cancer types.  GSCALite offers different types of graphic layout (CNV Pie distribution, Hete CNV, Homo CNV, CNV overall frequency distribution and correlation to expression, see details in <code>help page</code> below.).</p>
                </div>
                </div>
                </div>
                </div>")
        ),
        ## Hlep message including in tcga_cnv_help.ui----
        source(file.path(config$ui,"tcga_cnv_help.R"))[1],
        
        shiny::tags$hr(width="85%"),
        
        # cancer type selection and result output---------------------------------------------------
        # cancer type selection----
        cancerTypeInput("cnv"),
        
        
        # Confirm and submit ----
        fluidRow(
          selectAndAnalysisInput("cnv")
        ),
        shiny::tags$hr(width="85%"),
        
        # output plot -------------------------------------------------------------
        # Tabset Panel
        fluidRow(
          column(width = 10,
                 offset = 1,
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