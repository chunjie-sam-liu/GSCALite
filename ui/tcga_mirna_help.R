# sourced by 'tcga_mirna_ui.R'
# save as 'tcga_mirna_help.R'
# help file for tcga_mirna ui part

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left",
                                                               HTML('<a data-toggle="collapse" href="#sqTCGA_mirna">
                                                                    <i class="fa fa-question fa-fw"></i> 
                                                                    Click here for help</a>')
                                                               )
                                                               ),
                                shiny::tags$div(id="sqTCGA_mirna", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       # overall description----
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What is miRNA?", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","A microRNA (abbreviated miRNA) is a small non-coding RNA molecule (containing about 22 nucleotides) found in plants, animals and some viruses, that functions in RNA silencing and post-transcriptional regulation of gene expression.
. See details at: ",
                                                                                            shiny::tags$a(href="http://www.sciencedirect.com/science/article/pii/S0092867404000455?via%3Dihub","D. P. Bartel, MicroRNAs: Genomics, Biogenesis, Mechanism, and Function. Cell. 116, 281–297 (2004)"),"."),
                                                                              #shiny::tags$p(class="text-justify",),
                                                                              
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
                                                                                                    shiny::tags$td("miRNA regulation network"),
                                                                                                    shiny::tags$td("The regulation of mirnas to gene recorded by database will be showed in this network. A node represent a miRNA or gene, an edge represent a regulation of miRNA to gene. We have cluster them by color, and you can click and drag the network as you wish.")
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