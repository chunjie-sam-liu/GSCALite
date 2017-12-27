# sourced by 'ui.R'
# save as 'tcga_mirna_ui.R'
# ui elements 'tcga_mirna' sub tab of 'tcga' tab

tabItem(tabName = "tcga_mirna", align = "center",
        shinyjs::useShinyjs(),
        
        ## SNV message ----
        fluidRow(style="width:80%;",
                 HTML("<div class='section'>
                      <div class='container'>
                      <div class='row'>
                      <div class='col-md-12'>
                      <h1 class='text-success text-left'>
                      <i class='fa fa-angle-double-right  fa-fw'></i>miRNA
                      <font color='#777777'>
                      <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Regulation Network</span>
                      </font>
                      </h1>
                      <hr>
                      <p class='lead'>A miRNA regulation network will be given, you will find which miRNA regulate your genes and have a global understanding by network presentation.

                      <br> miRNA regulation data is collected form databases <code>help page</code> below.), a network will be draw for you to visualize the miRNA regulation to your gene set.</p>
                      </div>
                      </div>
                      </div>
                      </div>")
                 ),
        ## Hlep message including in tcga_mirna_help.ui----
        source(file.path(config$ui,"tcga_mirna_help.R"))[1],
        
        shiny::tags$br(),
        shiny::tags$hr(width="85%"),
        
        # cancer type no selection and only result output---------------------------------------------------

        
        # output plot -------------------------------------------------------------
        # Tabset Panel
        fluidRow(
          column(width = 10,
                 offset = 1,
                 shiny::tags$br(),
                 shinydashboard::tabBox(id = "mirna_PLOT",title = "PLOT",width = 12,
                                        tabPanel(title="networkD3",
                                                 forceNetworkOutput("mirna_net1",height = "900px")),
                                        tabPanel(title= "visNetwork",
                                                 visNetwork::visNetworkOutput("mirna_net2",height = "900px"))
                 )
          )
        ),
        
        # load footer ------------------------------------------------------
        source(file.path(config$ui, "footer.R"))[1]
        
        
                 ) # close tab