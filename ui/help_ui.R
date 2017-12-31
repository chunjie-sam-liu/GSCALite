# sourced by 'ui.R'
# save as 'help_ui.R'
# ui elements for help


tabItem(
  tabName = "help",

  # help information ----
  fluidRow(
    style = "width:100%;",

    column(
      width = 12, offset = 0,
      shiny::tags$h1("1: TCGA anaysis result in GSCALite"),
      shiny::tags$h2("1.1: Gene Set Expression analysis result"),
      shiny::tags$img(
        src = "./imgs/1.mrna_expression/tumorVSnormal.png",
        class = "center-block img-responsive",
        style = "height: 300px;" ),
      shiny::tags$p("This picture here displayed the expression ratio of gene set in the tumor-VS-normal compairson. 
                    The dot denotes FDR from multiple t-test."),
      shiny::tags$img(
        src = "./imgs/1.mrna_expression/FC-table.png",
        class = "center-block img-responsive",
        style = "height: 300px;" ),
      shiny::tags$p("The table above indicates the detailed information for the gene set queried. 
                    User could click the arrow besides column names to change the rank. 
                    The boxes under column names help user to search target results."),
      shiny::tags$h2("1.2: SNV analysis results"),
      shiny::tags$h3("1.2.1: SNV percentage profile"),
      shiny::tags$img(
        src = "./imgs/imgs/2.snv/percent.png",
        class = "center-block img-responsive",
        style = "height: 300px;" ),
      shiny::tags$p("The number in the main picture denotes the mututaion frequency of the gene in a specific cancer type.
                    each row represents a gene."),
      shiny::tags$h3("1.2.1: SNV percentage profile"),
    )
  ),


  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem
