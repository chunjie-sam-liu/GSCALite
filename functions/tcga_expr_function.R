# sourced by "tcga_expr_server.R"

fn_expr_welcome <- function(){
  column(
    width = 12, offset = 0,
    shiny::tags$h1(
      class = "text-success text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Gene Set Expression"
    ),
    shiny::hr(),
    shiny::tags$p(
      class = "lead text-justify",
      "GCSALite mRNA expression module calculate the gene set differential expression across cancers based on the TCGA expression data.", 
      "The module analysis result provides differential expression, survival analysis and subtype analysis."
    )
  )
}

fn_expr_help <- function(){
  column(
    width = 12, offset = 0,
    # use primary panel
    shiny::tags$div(
      class = "panel panel-primary",
      
      # panel head
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#help_expr",
            shiny::icon(name = "info-circle", class = "fa-fw"),
            "Click here for the detailed description of methods and results"
            )
          )
        ),
      
      # panel body
      shiny::tags$div(
        id = "help_expr", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          column(
            width = 12, offset = 0,
            
            # Methods
            shiny::tags$div(
              class = "bs-callout bs-callout-primary",
              shiny::tags$h3("Methods"),
              
              shiny::tags$dl(
                class = "dl-horizontal",
                
                shiny::tags$dt("Data:"),
                shiny::tags$dd(
                  "We collected 10995 mRNA Seq level 3 data and 11160 clinical data from",
                  shiny::tags$a("href" = "https://gdc.cancer.gov/", "NCI Genomic Data Commons") 
                  ),
                
                shiny::tags$dt("Tumor vs. Normal:"),
                shiny::tags$dd(
                  "The number of sample in each cancer types ranges from 48 to 1,098, but only 14 cancer types have over ten paired tumor and normal samples.",
                  
                  "In the mRNA differential expression analysis, we use TCGA normalized ",
                  
                
                shiny::tags$dt("Survival:"),
                shiny::tags$dd("Survival and subtype was analysis across all the cancer types the user chose")
              )
            ),
            
            # Results
            shiny::tags$div(
              class = "bs-callout bs-callout-danger",
              shiny::tags$h3("Results"),
              
              shiny::tags$dl(
                class = "dl-horizontal",
                
                shiny::tags$dt("Tumor vs. Normal:"),
                shiny::tags$dd("In the result figure, the row is the gene set symbol and column is the selected cancer types. The color from purple to red represent the fold change between tumor vs normal. The size dot indicates the significance. The dot was filtered by the fold change (fc>2) and significance (fdr < 0.05)"),
                
                shiny::tags$dt("Table:"),
                shiny::tags$dd("The table provides the detailed information of first figure"),
                
                shiny::tags$dt("Survival:"),
                shiny::tags$dd("The dot represent the gene affects survival of the cancer types, the p-value is the Kaplan Meier P-value. The dot color indicates the worse of the high or low expression in the cancer types"),
                
                shiny::tags$dt("Subtype:"),
                shiny::tags$dd("Each gene may have different expression in the different subtypes. This figure represent the gene affect subtype")
                
              )
            )
          )
        )
      )
    )
  )
}
# Expr output -------------------------------------------------------------

exprOutput <- function(id) {
  ns <- NS(id)
  
  column(
    width = 12, offset = 0,
    shinydashboard::tabBox(
      id = "expr_plot", title = "", width = 12,
      # bubble plot for tumor vs. normal
      tabPanel(
        title = "Tumor vs. Normal",
        uiOutput(ns("de_massage")),
        column(width=2,
               download_bt(ns("de"))
               ),
        column(
          width=12,
          plotOutput(outputId = ns("expr_bubble_plot"),height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        )
      ),
      # datatable
      tabPanel(
        title = "Table of comparison",
        DT::dataTableOutput(outputId = ns("expr_dt_comparison")) %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      ),
      tabPanel(
        title = "Survival",
        uiOutput(ns("sur_massage")),
        column(width=2,
               download_bt(ns("sur"))
        ),
        column(
          width=12,
        plotOutput(outputId = ns("survival"),height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        )
      ),
      tabPanel(
        title = "Subtype",
        uiOutput(ns("sub_massage")),
        column(width=2,
               download_bt(ns("sub"))
        ),
        column(
          width=12,
        plotOutput(outputId = ns("subtype"),height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        )
      )
    )
  )
}

fn_expr_result <- function(.expr){
  
  if (.expr == TRUE) {
    exprOutput("expr")
  } else{
    column(
      width = 12, offset = 0,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}

fn_dropdown_widget <- function(){
  print("dropdown menu for the image")
  dropdownButton(
    
    tags$h3("List of Inputs"),
    
    selectInput(inputId = 'xcol',
                label = 'X Variable',
                choices = names(iris)),
    
    selectInput(inputId = 'ycol',
                label = 'Y Variable',
                choices = names(iris),
                selected = names(iris)[[2]]),
    
    sliderInput(inputId = 'clusters',
                label = 'Cluster count',
                value = 3,
                min = 1,
                max = 9),
    
    
    circle = TRUE, status = "danger",
    icon = icon("gear"), width = "300px",
    
    tooltip = tooltipOptions(title = "Click to see inputs !")
  )
}

fn_img_download <- function(){
  output$down <- downloadHandler(
    filename =  function() {
      paste("iris", input$var3, sep = ".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if (input$var3 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      plot(x = x(), y = y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
      dev.off()  # turn the device off

    } 
  )
}
