# sourced by 'tcga_rppa_ui.R'
# save as 'tcga_rppa_help.R'
# help file for tcga_rppa ui part

fluidRow(style="width:85%",
         column(width = 12,
                # collapsable panel
                shiny::tags$div(class="panel panel-default",
                                shiny::tags$div(class="panel-heading",
                                                shiny::tags$h3(class="panel-title text-left",
                                                               HTML('<a data-toggle="collapse" href="#sqTCGA_rppa">
                                                                    <i class="fa fa-question fa-fw"></i> 
                                                                    Click here for help</a>')
                                                               )
                                                               ),
                                shiny::tags$div(id="sqTCGA_rppa", class="panel-collapse collapse",
                                                shiny::tags$div(class="panel-body",
                                                                # here comes the content
                                                                ###############
                                                                column(width=12,
                                                                       # overall description----
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What is RPPA ?", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","Reverse phase protein array (RPPA) is a high-throughput antibody-based technique with the procedures similar to that of Western blots. Proteins are extracted from tumor tissue or cultured cells, denatured by SDS, printed on nitrocellulose-coated slides followed by antibody probe (",
                                                                              shiny::tags$a(href="http://bioinformatics.mdanderson.org/main/TCPA:Overview","TCPA database"),
                                                                              class="text-justify",")TCPA RPPA data are all from TCGA samples."),
                                                                              
                                                                              shiny::tags$br(),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                       ),
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What can you get.", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","In this part we got the relationship between gene expression and 10 pathways which important for cancer development, you can see a potential influence that your genes may have to some famous pathways.",
                                                                              shiny::tags$br(),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                       )),
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("Get pathway activity.", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","RBN RPPA data were median-centered and normalized by standard deviation across all samples for each component to obtain the relative protein level. The pathway score is then the sum of the relative protein level of all positive regulatory components minus that of negative regulatory components in a particular pathway(",
                                                                                            shiny::tags$a(href="https://doi.org/10.1038/ncomms4887","R. Akbani et al."),")."),
                                                                              
                                                                              shiny::tags$br(),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                       ),
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("What pathway we included ?", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","The pathway we included in are: TSC/mTOR, RTK, RAS/MAPK, PI3K/AKT, Hormone ER, Hormone AR, EMT, DNA Damage Response, Cell Cycle, Apoptosis. They are all famous cancer related pathway."),
                                                                              
                                                                              shiny::tags$br(),
                                                                              
                                                                              shiny::tags$hr(width="100%")
                                                                              
                                                                       ),
                                                                       column(width=10, offset=1,
                                                                              
                                                                              shiny::tags$h3("Calculation method", class="text-success"),
                                                                              
                                                                              shiny::tags$p(class="text-justify","Gene expression was divided into 2 groups by median expression, the difference of pathway activity between groups is defined by a student T test, p value <=0.05 is considered as significant."),
                                                                              
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
                                                                                                    shiny::tags$td("Global percentage"),
                                                                                                    shiny::tags$td("Global percentage of genes in all cancers(32), shows percentage(number of activate or inhibit cancer types/32) of gene's function (activation or inhibition) for each pathway in all cancers.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Relation network"),
                                                                                                    shiny::tags$td(shiny::tags$p("This network show you the relationship between genes and pathways by a line connection, solid line means activation, dashed lines means inhibition."))
                                                                                                  )
                                                                                                )
                                                                              )
                                                                       )# result description end
                                                                       )
                                                                       
                                                                       
                                                                )# all description end
                                                )
                                )
                                                )
                
         )
         