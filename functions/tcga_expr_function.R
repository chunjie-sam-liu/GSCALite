# sourced by "tcga_expr_server.R"

fn_expr_welcome <- function(){
  column(
    width = 12, offset = 0,
    shiny::tags$h1(
      class = "text-success text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Gene Set Expression"
    ),
    shiny::tags$p(
      class = "lead text-left",
      "GCSALite mRNA expression module calculate the gene set differential expression across cancers based on the TCGA expression data.", 
      "The module analysis result provides differential expression, survival analysis and subtype analysis.",
      "See the details in",
      shiny::tags$code("help page.")
    )
  )
}

fn_expr_help <- function(){
  column(
    width = 12, offset = 0,
    
    shiny::tags$div(
      class = "panel panel-default",
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#help_expr",
            shiny::icon(name = "question", class = "fa-fw"),
            "Click here for help"
            )
          )
        )
      ),
    shiny::tags$div(
      id = "help_expr", class = "panel-collapse collapse",
      shiny::tags$div(
        class = "panel-body",
        column(
          width = 12, offset = 0,
          # methods
          column(
            width = 10, offset = 1,
            shiny::tags$h3("Method", class = "text-success"),
            shiny::tags$p(
              class = "text-justify",
              "1. The mRNA expression and clinical data was downloaded from", shiny::tags$a("href" = "https://gdc.cancer.gov/", "NCI Genomic Data Commons", style = "color:#08176" )
            ),
            
            shiny::tags$p(
              class = "text-justify",
              "2. TCGA maintains 33 cancer types, but only 14 cancer types have paired tumor vs. normal data. The  gene set mRNA differential expression was based on the 14 cancer types."
            ),
            
            shiny::tags$p(
              class = "text-justify",
              "3. Survival and subtype was analysis across all the cancer types the user chose"
            ),
            shiny::tags$hr(width = "100%")
            ),
          column(
            width = 10, offset = 1,
            shiny::tags$h3("Figure and Tables Description", class = "text-success"),
            shiny::tags$table(
              class = "table table-striped",
              shiny::tags$thead(
                shiny::tags$th("Result"),
                shiny::tags$th("Description")
              ),
              shiny::tags$tr(
                shiny::tags$td("Tumor vs. Normal"),
                shiny::tags$td("In the result figure, the row is the gene set symbol and column is the selected cancer types. The color from purple to red represent the fold change between tumor vs normal. The size dot indicates the significance. The dot was filtered by the fold change (fc>2) and significance (fdr < 0.05)")
              ),
              shiny::tags$tr(
                shiny::tags$td("Table of comparison"),
                shiny::tags$td("The table provides the detailed information of first figure.")
              ),
              shiny::tags$tr(
                shiny::tags$td("Survival"),
                shiny::tags$td("The dot represent the gene affects survival of the cancer types, the p-value is the Kaplan Meier P-value. The dot color indicates the worse of the high or low expression in the cancer types.")
              ),
              shiny::tags$tr(
                shiny::tags$td("Subtype"),
                shiny::tags$td("Each gene may have different expression in the different subtypes. This figure represent the gene affect subtype.")
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
    width = 10, offset = 1,
    shinydashboard::tabBox(
      id = "expr_plot", title = "", width = 12,
      # bubble plot for tumor vs. normal
      tabPanel(
        title = "Tumor vs. Normal",
        column(width=2,
               download_bt(ns("de"))
               ),
        column(
          width=12,
          plotOutput(outputId = ns("expr_bubble_plot")) 
        )
      ),
      # datatable
      tabPanel(
        title = "Table of comparison",
        DT::dataTableOutput(outputId = ns("expr_dt_comparison")) %>% withSpinner()
      ),
      tabPanel(
        title = "Survival",
        column(width=2,
               download_bt(ns("sur"))
        ),
        column(
          width=12,
        plotOutput(outputId = ns("survival")) %>% withSpinner()
        )
      ),
      tabPanel(
        title = "Subtype",
        column(width=2,
               download_bt(ns("sub"))
        ),
        column(
          width=12,
        plotOutput(outputId = ns("subtype")) %>% withSpinner()
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
      width = 10, offset = 1,
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
