fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left",
                                                               HTML('<a data-toggle="collapse" href="#sqTCGA_cnv">
                                                                    <i class="fa fa-question fa-fw"></i> 
                                                                    Click here for help</a>')
                                                               )
                                                               ),
                                shiny::tags$div(id="sqTCGA_cnv", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       # overall description----
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What is Copy number variation", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","Copy number variation is a type of structural variation: specifically, it is a type of duplication or deletion event that affects a considerable number of base pairs.<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1226196/'>Reference</a>."),
                                                                              
                                                                              shiny::tags$br(),
                                                                              
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
                                                                                                    shiny::tags$td("CNV Pie distribution"),
                                                                                                    shiny::tags$td("CNV pie plot gives you a global profile that shows the constitute of Heterozygous and Homozygous CNV about each gene in each cancer.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Heterozygous CNV"),
                                                                                                    shiny::tags$td("Heterozygous CNV gives you a percentage information, include the amplification and deletion percentage of heterozygous CNV about each gene in each cancer.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Homozygous CNV"),
                                                                                                    shiny::tags$td("Homozygous CNV gives you a percentage information, include the amplification and deletion percentage of homozygous CNV about each gene in each cancer.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Overall CNV Frequency"),
                                                                                                    shiny::tags$td("Overall CNV frenquency gives you a bar plot, which shows the frequency of all genes' CNV in each cancer type.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Oncostrip"),
                                                                                                    shiny::tags$td("Similar to OncoPrinter tool on cBioPortal and oncoplot at SNV part, oncostrip draws CNV in each sample. Can combine with SNV data.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Exclude CNV"),
                                                                                                    shiny::tags$td("Exclude CNV is a phenomenon that two genes' CNV happened different in a same sample all the time, this difference should meet a statistic difference at least.")
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