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
                                                                              
                                                                              shiny::tags$h3("Get pathway score.", class="text-success"),
                                                                              
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
                                                                                                    shiny::tags$td("Global percentage of genes in each cancer, shows percentage of gene's function (activation or inhibition) for each pathway in each cancer.")
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Protein expression profile"),
                                                                                                    shiny::tags$td(shiny::tags$p("Boxplot of Gene's protein expression in each cancer types will be shown when you click on gene symbol on ",
                                                                                                                          shiny::tags$b("Global percentage")," plot. Only genes who have been measured by TCPA will be shown."))
                                                                                                  ),
                                                                                                  shiny::tags$tr(
                                                                                                    shiny::tags$td("Pathway activity of specific gene"),
                                                                                                    shiny::tags$td(shiny::tags$p("A detailed function profile of candidate gene for cancer related pathway in each cancer types will be shown by click on gene symbol on ",
                                                                                                                          shiny::tags$b("Global percentage"),"."))
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