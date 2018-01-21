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
      "TCGA expression data will be used to give you a visualization of your gene set for seleted cancer types."
    )
  )
}