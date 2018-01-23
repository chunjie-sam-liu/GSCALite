mirnaOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    shinydashboard::tabBox(id = "mirna_PLOT",title = "PLOT",width = 12,
                           tabPanel(title="networkD3",
                                    forceNetworkOutput("mirna_net1",height = "700px") %>% withSpinner(color="#0dc5c1")),
                           tabPanel(title= "visNetwork",
                                    visNetwork::visNetworkOutput("mirna_net2",height = "700px") %>% withSpinner(color="#0dc5c1"))
    )
  )
}

fn_mirna_result <- function(.mirna){
  if (.mirna == TRUE) {
    mirnaOutput()
  } else{
    column(
      width = 10, offset = 1,
      tags$div("This analysis is not selected")
    )
  }
}