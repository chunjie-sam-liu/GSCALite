# sourced by 'ui.R'
# save as 'tcga_snv_ui.R'
# ui elements 'tcga_snv' sub tab of 'tcga' tab

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
        # cancer type selection----
        cancerTypeInput("snv"),
        
        # Selected cancer show ----
        shiny::tags$h3("Cancer Type Check",class="text-success"),
        shiny::tags$h4("The cancers you selected: ",
                       textOutput("snv_selected_cancer"),
                       " Confirm and start analysis by click Submit!"),
        
        # Confirm and submit ----
        column(width = 2,offset = 5,
               actionButton("snv_submit", label ="Submit!",icon = icon("check"))
        ),
        shiny::tags$hr(width="85%"),
        
        # output plot -------------------------------------------------------------
        # Tabset Panel
        fluidRow(
          column(width = 10,
                 offset = 1,
                 shiny::tags$br(),
                 shinydashboard::tabBox(id = "snv_PLOT",title = "PLOT",width = 12,
                                        tabPanel(title= "SNV percentage profile",PlotInput(id="snv_percentage")),
                                        tabPanel(title="SNV summary plot",plotOutput("snv_summary-plot")),
                                        tabPanel(title="SNV oncoplot",plotOutput("snv_oncoplot-plot")),
                                        # tabPanel(title="SNV oncostrip",PlotInput("snv_oncostrip")),
                                        # tabPanel(title="SNV lollipop",PlotInput("snv_lollipop")),
                                        tabPanel(title="SNV survival",PlotInput("snv_survival"))
                                        #tabPanel(title="SNV mutation load",PlotInput("snv_mut_load"))
                 )
          )
        ),
 
        # load footer
        source(file.path(config$ui, "footer.R"))[1]
        
        
) # close tab