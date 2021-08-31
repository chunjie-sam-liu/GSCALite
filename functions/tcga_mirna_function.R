download_button_2 <- function(id){
  ns <- NS(id)
  shiny::tagList(
    column(
      width = 6, offset = 0,
      column(width = 4,
             prettyRadioButtons(
               inputId = ns("pictype"),
               label = "Format",
               choices = list("HTML" = "html"),
               inline = TRUE,
               icon = icon("check"),
               bigger = TRUE, status = "info",
               animation = "jelly"
             )
             ),
      # column(width = 4,
      #        actionButton("store_position", "Store positions of nodes")
      #        ),
      column(width = 4,
             downloadButton(
               outputId = ns("downloadNetwork"),
               label = "Download"
             )
             )
    )
  )
}

download_button_1 <- function(id){
  ns <- NS(id)
  shiny::tagList(
    column(
      width = 6, offset = 0,
      column(width = 4,
             prettyRadioButtons(
               inputId = ns("pictype"),
               label = "Format",
               choices = list("HTML" = "html"),
               inline = TRUE,
               icon = icon("check"),
               bigger = TRUE, status = "info",
               animation = "jelly"
             )
      ),
      column(width = 4,
             downloadButton(
               outputId = ns("downloadNetwork"),
               label = "Download"
             )
      )
    )
  )
}

mirnaOutput <- function() {
  # ns <- NS(id)
  column(
    width = 12, offset = 0,
    shinydashboard::tabBox(id = "mirna_PLOT",title = "",width = 12,
                           tabPanel(title="networkD3",
                                    download_button_1("mirna_net1"),
                                    column(width = 12,
                                           forceNetworkOutput("mirna_net1",height = "800px") %>% withSpinner(color="#0dc5c1")
                                           )
                                    ),
                           tabPanel(title= "visNetwork",
                                    download_button_2("mirna_net2"),
                                    column(width = 12,
                                    visNetwork::visNetworkOutput("mirna_net2",height = "800px") %>% withSpinner(color="#0dc5c1"))
                                    )
    )
  )
}

fn_mirna_result <- function(.mirna){
  if (.mirna == TRUE) {
    mirnaOutput()
  } else{
    column(
      width = 12, offset = 0,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}