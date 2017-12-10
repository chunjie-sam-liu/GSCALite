# sourced by 'tcga_meth_ui.R'
# save as 'tcga_meth_help.R'
# help file for tcga_meth ui part

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left",
                                                               HTML('<a data-toggle="collapse" href="#sqTCGA_meth">
                                                                    <i class="fa fa-question fa-fw"></i> 
                                                                    Click here for help</a>')
                                                               )
                                                               ),
                                shiny::tags$div(id="sqTCGA_meth", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       # overall description----
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What is Methylation ?", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","DNA methylation is a key epigenetic modification to cytosines, often in CpG dinucleotides. This modification has been frequently associated with gene silencing, but the precise role of DNA methylation in development and disease remains a mystery."),
                                                                              
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
                                                                                                    shiny::tags$td("Differential Methylation"),
                                                                                                    shiny::tags$td("We offer a methylation change between disease and normal samples use TCGA data. Blue points represent a hypomethylation in tumors, red points represent a hypermethylation in tumors.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Heatmap"),
                                                                                                    shiny::tags$td("Heatmap gives you the methylation level of your gene set in a specific cancer type when you click on the cancer name on Differential Methylation.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Boxplot"),
                                                                                                    shiny::tags$td("The methylation change between disease and normal samples showed by boxplot, draw it by click a bubble on Differential Methylation Plot.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Methylation Survival"),
                                                                                                    shiny::tags$td("Gives you a survival difference between samples with a specific genes' high and low-methylation, only p value significant(<=0.05) genes will be displayed here. Click gene name to show the survival plot.")
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