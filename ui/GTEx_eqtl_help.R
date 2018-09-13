# sourced by 'GTEx_eqtl_ui.R'
# save as 'GTEx_eqtl_help.R'
# help file for GTEx eqtl ui part

fluidRow(
  style = "width:85%",
  column(
    width = 12,
    # collapsable panel
    shiny::tags$div(
      class = "panel panel-default",
      shiny::tags$div(
        class = "panel-heading",
        shiny::tags$h3(
          class = "panel-title text-left",
          HTML('<a data-toggle="collapse" href="#sqGTEx_eqtl"><i class="fa fa-question fa-fw"></i> 
          Click here for help</a>')
        )
      ),
      shiny::tags$div(
        id = "sqGTEx_eqtl", class = "panel-collapse collapse",
        shiny::tags$div(
          class = "panel-body",
          column(
            width = 12,
            # overall description----
            column(
              width = 10, offset = 1,
              shiny::tags$h3("GTEx eQTLs", class = "text-success"),
              shiny::tags$p(class = "text-justify", "eQTLs are genomic loci which contribute to variation of gene expression. The Genotype-Tissue Expression (GTEx) project provides valuable information to identify genomic variations associated with gene expression in multiple tissues from health individuals."),
              shiny::tags$br(),
              shiny::tags$hr(width = "100%")
            ),
            # result description----
            column(
              width = 10, offset = 1,
              shiny::tags$h3("Results commentary", class = "text-success"),
              shiny::tags$table(
                class = "table table-striped",
                shiny::tags$thead(
                  shiny::tags$th("Columns"),
                  shiny::tags$th("Description")
                ),
                shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("gene_name"),
                    shiny::tags$td("Approved Symbol. The official gene symbol approved by the HGNC.")
                  ),
                  #        shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("gene_chr"),
                    shiny::tags$td("Which chromosome the gene locates.")
                  ),
                  #         shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("gene_start"),
                    shiny::tags$td("Which loci the gene starts.")
                  ),
                  #          shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("gene_end"),
                    shiny::tags$td("Which loci the gene ends.")
                  ),
                  #           shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("strand"),
                    shiny::tags$td("Which strand the gene locates.")
                  ),
                  #            shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("chr"),
                    shiny::tags$td("**The eQTL loci for this gene locates in which chromosome")
                  ),
                  #             shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("pos"),
                    shiny::tags$td("The eQTL loci.")
                  ),
                  #              shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("rs_id"),
                    shiny::tags$td("rs id here denotes the eQTL loci is recorded in dbsnp, otherwise use dot instead.")
                  ),
                  #              shiny::tags$tbody(
                  shiny::tags$tr(
                    shiny::tags$td("qval"),
                    shiny::tags$td("The FDR of this eQTL.")
                  ),
                  shiny::tags$tr(
                    shiny::tags$td("tissue"),
                    shiny::tags$td("The eQTL influences the expression of gene in this tissue.")
                  )
                )
              )
            ) # result description end
          ) # all description end
        )
      )
    )
  )
)
