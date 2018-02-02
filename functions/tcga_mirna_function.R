download_button <- function(id){
  ns <- NS(id)
  shiny::tagList(
    column(
      width = 2, offset = 0,
      shinyWidgets::dropdownButton(
        tags$h3("Download Options"),
        prettyRadioButtons(
          inputId = ns("pictype"),
          label = "Selcet format for your pictur",
          choices = list("PDF" = "pdf", "PNG" = "png"),
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
    )
  )
}

mirnaOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    shinydashboard::tabBox(id = "mirna_PLOT",title = "",width = 12,
                           tabPanel(title="networkD3",
                                    # download_button("mirna_net1"),
                                    column(width = 12,
                                           forceNetworkOutput("mirna_net1",height = "700px") %>% withSpinner(color="#0dc5c1")
                                           )
                                    ),
                           tabPanel(title= "visNetwork",
                                           visNetwork::visNetworkOutput("mirna_net2",height = "700px") %>% withSpinner(color="#0dc5c1")
                                    )
    )
  )
}

fn_mirna_result <- function(.mirna){
  if (.mirna == TRUE) {
    mirnaOutput()
  } else{
    column(
      width = 10, offset = 1,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}