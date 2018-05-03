eqtlOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    fluidRow(GTEx_eqtl_Output("gtex_eqtl"))
  )
}

fn_eqtl_result <- function(.eqtl){
  if (.eqtl == TRUE) {
    eqtlOutput()
  } else{
    column(
      width = 10, offset = 1,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}

fn_gtex_eqtl_welcome <- function(){
  column(
    width = 12, offset = 0,
    shiny::tags$h1(
      class = "text-success text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Expression quantitative trait locis (eQTLs) in GTEx Dataset"
    ),
    shiny::hr(),
    
    shiny::tags$p(
      class = "lead text-justify",
      "The eQTLs of gene set in the selected GTEx normal tissues will be displayed here in the form of table."
    )
  )
}

fn_gtex_eqtl_help <- function(){
  column(
    width = 12, offset = 0,
    
    shiny::tags$div(
      class = "panel panel-primary",
      
      #Head
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          
          shiny::tags$a(
            "data-toggle" = "collapse", "href" = "#sqGTEx_eqtl",
            shiny::icon(name = "info-circle", class = "fa-fw"),
            "Click here for the detailed description of methods and results"
          )
        )
      ),
      
      # Body
      shiny::tags$div(
        id = "sqGTEx_eqtl", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          column(
            width = 12, offset = 0,
            
            # Methods
            shiny::tags$div(
              class = "bs-callout bs-callout-primary",
              shiny::tags$h3("Method"),
              
              shiny::tags$dl(
                class = "dl-horizontal",
                
                shiny::tags$dt("Data & Scripts:"),
                shiny::tags$dd(
                  "eQTLs are genomic loci which contribute to variation of gene expression. The Genotype-Tissue Expression (GTEx) project provides valuable information to identify genomic variations associated with gene expression in multiple tissues from health individuals."
                )
              )
            ),
            
            # Result
            shiny::tags$div(
              class = "bs-callout bs-callout-danger",
              shiny::tags$h3("Result"),
              
              shiny::tags$dl(
                class = "dl-horizontal",
                
                shiny::tags$dt("gene_name:"),
                shiny::tags$dd("Approved Symbol. The official gene symbol approved by the HGNC."),
                
                shiny::tags$dt("gene_chr:"),
                shiny::tags$dd("Which chromosome the gene locates."),
                
                shiny::tags$dt("gene_start:"),
                shiny::tags$dd("Which loci the gene starts."),
                
                shiny::tags$dt("gene_end:"),
                shiny::tags$dd("Which loci the gene ends."),
                
                shiny::tags$dt("strand:"),
                shiny::tags$dd("Which strand the gene locates."),
                
                shiny::tags$dt("chr:"),
                shiny::tags$dd("The eQTL loci for this gene locates in which chromosome"),
                
                shiny::tags$dt("pos:"),
                shiny::tags$dd("The eQTL loci."),
                
                shiny::tags$dt("rs_id:"),
                shiny::tags$dd("rs id here denotes the eQTL loci is recorded in dbsnp, otherwise use dot instead."),
                
                shiny::tags$dt("qval"),
                shiny::tags$dd("The FDR of this eQTL."),
                
                shiny::tags$dt("tissue"),
                shiny::tags$dd("The eQTL influences the expression of gene in this tissue.")
              )
            )
          )
        )
      )
    )
  )
}
