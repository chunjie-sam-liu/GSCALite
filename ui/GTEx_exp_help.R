# sourced by 'GTEx_exp_help.R'
# save as 'GTEx_exp_help.R'
# help file for GTEx expression ui part

fluidRow(style="width:85%",
  column(width = 12,
  # collapsable panel
  shiny::tags$div(class="panel panel-default",
    shiny::tags$div(class="panel-heading",
      shiny::tags$h3(class="panel-title text-left",
        HTML('<a data-toggle="collapse" href="#sqGTEx_expression"><i class="fa fa-question fa-fw"></i> 
          Click here for help</a>')
      )
    ),
    shiny::tags$div(id="sqGTEx_expression", class="panel-collapse collapse",
      shiny::tags$div(class="panel-body",
        column(width=12,
        # overall description----
          column(width=10, offset=1,
            shiny::tags$h3("GTEx Gene Expression", class="text-success"),
            shiny::tags$p(class="text-justify","The Genotype-Tissue Expression (GTEx) project provides valuable insights 
                            into the mechanisms of gene expression and regulationship in multiple tissues from health individuals, 
                            which offered important information for exploring disease-related perturbations. 
                            GTEx Expressiondataset (V7.0) composed of 11,688 samples contains the expression profiles of 56,202 genes from 30 organs (53 tissues), 
                            which donated by 714 health individuals."),
            shiny::tags$hr(width="100%")
          ),
          # result description----
          column(width=10, offset = 1,
            shiny::tags$h3("Results commentary", class="text-success"),
            shiny::tags$table(class="table table-striped",
              shiny::tags$thead(
                shiny::tags$th("Results type"),
                shiny::tags$th("Description")
              ),
              shiny::tags$tbody(
								shiny::tags$tr(
                  shiny::tags$td("Heatmap"),
                  shiny::tags$td("Heatmap gives you the expression profiles of your gene set in selected tissues.")
                ),
                shiny::tags$tr(
                  shiny::tags$td("GSVA Boxplot"),
                  shiny::tags$td("The Gene Set Variation Analysis (GSVA) boxplot displays the estimating variation of gene set enrichment through the samples in the GTEx expression dataset. 
                                 The dot denotes a sample in the given tissue.")
                )
              )
            )
          )# result description end
                                                                       
                                                                       
        )# all description end
      )
    )
  )
                
)
)
